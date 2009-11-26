open Error 
open Network 
open Mods2


let trace = Data.show_steps_in_compression  (*true for dumping compression log*)
let trace_print s = 
  if !trace then
    begin
      print_string s;
      print_newline ()
    end 

let opt_extract f =
  match f with
      Some a -> a
    | None -> Error.runtime (None,None,None) "Story_compression.opt_extract: option doesn't have a Some argument"

type portid = int
type eventid = int
module PortIdSet = Set.Make (struct type t = int let compare = compare end)
module EidMap = Map.Make (struct type t = int let compare = compare end)

type quark_type = State | Binding | AnyBound
type quark = (int*string)*quark_type 
module QuarkMap = Map.Make (struct type t = quark let compare = compare end)

let back = ref 0 
let init_time = ref 0. 
let error_state = ref None 

let set_init_time () = 
  begin
    init_time := Mods2.gettime ();
    error_state := None
  end 

let test_time () = 
  Mods2.gettime () -. (!init_time) < !Data.max_time_per_compression

let set_error x = 
  error_state := Some x 

type case = 
    {
    port_id:int;
    eid:int;
    e_shortid:int;
    state_before:string;
    state_after:string 
  }


let empty_case = 
  {port_id = -1;
    eid = -1;
    e_shortid = -1;
    state_before = "";
    state_after = ""}
    
type network  = 
    {sparse_matrix:case array array;
     nevents_by_wire: int array; 
     nevents:int;
     nwires:int;
     wire_map: int QuarkMap.t;
      eid_to_ports: PortIdSet.t array;
      short_events:int EidMap.t array} (*port -> eid -> shorteid*)

 
module type Input = 
  sig 
    type choice
    type configuration
    type output 
    type stack
    type property 
	  
    val sol_init:network -> configuration
    val init_output: output
    val init_stack: stack
    val upgrade: choice option -> property list list 
    val upgrade_pos: choice -> property 
    val upgrade_neg: choice -> property
    val propagate: property list -> configuration -> stack -> (int -> int list) -> (property list * configuration*stack) option    
    val best_choice: configuration -> choice option * configuration
    val is_solution: configuration -> bool 
    val store_solution: configuration -> output -> output
    val restore: configuration -> stack  -> configuration
    val dump_output: output -> unit
    val fold_first_output: (int -> 'a  -> 'a) -> 'a -> output -> 'a 
    val fold_first_output_remove: (int -> 'a -> 'a) -> 'a -> output -> 'a
    val empty_output: output -> bool

	
  end
      
module IntSet = Set.Make (struct type t = int let compare = compare end)
module IntMap = Map.Make (struct type t = int let compare = compare end)

module Input = 
  (struct 
    type choice = int  
    type instr = int*int*(string option)
    type property = Stuck_event of int | Select_event of int | Erase_event of int | Select_value of (int*int*string) 
    type stack = instr list 	
    type final_solution = bool IntMap.t
    type output = final_solution list 
    let init_ouput = []
   
	    
    type configuration = 
	{
	network:network;
	remaining_event:IntSet.t;
	n_event:int;
	status: string option array array ;
	selected_event_map: bool  IntMap.t;
	filo:int list 
          }

    let dump_output = 
      List.iter 
	(fun a ->
	  let _ = IntMap.fold 
	      (fun x b bool -> 
		if b then ((if bool then print_string ",");
			   print_int x;true)
		else bool)
	      a false in 
	  print_newline ()) 
	
    let empty_output t = t=[] 
    let fold_first_output f i0 l =
      match (List.rev l) with [] -> i0
      |	 t::_ -> 
	  IntMap.fold 
	    (fun key bool sol -> if bool then f key sol else sol) 
	    t i0
	     
    let fold_first_output_remove f i0 l =
      match (List.rev l)  with [] -> i0
      |	 t::_ -> 
	  IntMap.fold 
	    (fun key bool sol -> if not bool then f key sol else sol) 
	    t i0
	
	
    let step_after_event i = i+1
    let step_before_event i = i
    let event_before_step i = i-1
    let event_after_step i = i 

   
    let string_of_cell (w,(b,a),s) = "WIRE: "^(string_of_int w)^
(match (b,a) with 
  Some b,None ->" AFTER EVENT "^(string_of_int b)
| None, Some a -> " BEFORE EVENT "^(string_of_int a)
| Some b,Some a-> " BETWEEN EVENT "^(string_of_int b)^" AND EVENT "^(string_of_int a)
|  _ -> "")^" STATE: "^(match s with None -> "None" | Some s ->  s)
			  
    let compute_before_after i wire configuration = 
      (try  Some (configuration.network.sparse_matrix.(wire).(i).eid) 
      with _ -> None),
      (try  Some (configuration.network.sparse_matrix.(wire).(i+1).eid) 
      with _ -> None)

    let string_of_choice (a,_) = 
      match a with 
	None -> ""
      |	Some a -> "Branch on event "^(string_of_int a)

    let restore configuration stack = 
      let nback = (!back)+1 in
      let _ = if nback > !Data.max_backtrack or (not (test_time ()))
              then (Error.too_expensive (Some "story_compressor.ml",Some 141,Some "too expensive")) 
      in  
      let _ = 
	if !Mods2.bench_mode && nback mod 1000 = 0 
	then 
	  (print_string "BACKTRACKS";
	   print_int nback;
	   print_newline ()) in
      let _ = trace_print "\nBacktrack\nCancel last assumption" in
      let _ =  
	List.iter 
	  (fun (i,j,k)  -> 
	    let _ = trace_print (string_of_cell (j,
						 compute_before_after i j configuration,
						 k)) in 
	    configuration.status.(j).(i) <- k)
	  stack 
      in 
      let _ = trace_print "Cancellation finished" in 
      let _ = back:=nback in
      configuration  

    let sol_init network =
      let n_event = network.nevents in
      let n_wire = network.nwires in
      let list_wire = 
	let rec aux k sol = 
	  if k<0 then sol
	  else aux (k-1) (k::sol)
	in aux (n_wire-1) [] in
      let status = Array.make (n_wire) [||] in
      let _ = 
	List.iter 
	  (fun k -> 
	    let nevents = Array.length network.sparse_matrix.(k) in
	    status.(k)<- Array.make (nevents+1)  None;
	    status.(k).(0)<-Some "_";
	    let _ = if !trace then 
	      (print_string ("INIT "^(string_of_cell 
				       (k,
					(None,Some (network.sparse_matrix.(k).(0).eid )),
					Some "_"))^"\n")) in 
	    ())
	  list_wire in 
      let selected = IntMap.empty in
      let remaining_event = 
	List.fold_left 
	  (fun a b -> IntSet.add b a) 
	  IntSet.empty 
	  (let rec aux k sol = 
	    if k = n_event 
	    then sol  
	    else aux (k+1) (k::sol) in
	  aux 0 [])
	  in 
      {network=network;
	n_event = n_event ;
	status = status ;
       	selected_event_map  = selected;
	remaining_event = remaining_event;
	filo = [] }
   
    let store_solution configuration output = configuration.selected_event_map::output
										  
    let is_solution configuration = IntSet.is_empty configuration.remaining_event
      
    let rec best_choice1 configuration = 
       match configuration.filo with 
 	[] -> (try Some (IntSet.max_elt configuration.remaining_event),configuration
 	    with _ -> None,configuration)
       |	t::q -> 
 	  if IntSet.mem t configuration.remaining_event 
 	  then (Some t,{configuration with filo = q})
 	  else best_choice1 {configuration with filo = q}
 	      	

    let best_choice2 configuration = 
      (try (let a = IntSet.max_elt configuration.remaining_event
	  in Some a)
      with _ -> None),configuration

    let best_choice x =
      if test_time () then 
       if Data.story_iteration_strategy = 1 
       then best_choice1 x
       else best_choice2 x
      else
	raise Too_expensive 
	 
    let best_choice x = 
      let rep = best_choice x in 
      let _ = 
	if !trace
	then 
	  let _ = print_string (string_of_choice rep) in 
	  let _ = print_newline () in 
	    () 
      in
	rep

    let network_after_short configuration event wire = 
      configuration.network.sparse_matrix.(wire).(event).state_after

    let network_before_short configuration event wire = 
      configuration.network.sparse_matrix.(wire).(event).state_before


    let check_short shortstep wire configuration working_list = 
      let event_after = event_after_step shortstep in
      let event_before = event_before_step shortstep in
      let state = 
	match configuration.status.(wire).(shortstep) with 
        Some s -> s
	| None -> raise Exit in
      let working_list =
	if event_before = (-1) 
	then 
	  working_list
	else 
	  let event_before_long = configuration.network.sparse_matrix.(wire).(event_before).eid in
	  if (try (not (IntMap.find event_before_long configuration.selected_event_map)) with Not_found -> false) 
	  then 
	    (Select_value (shortstep-1,wire,state))::working_list
	  else if 
	    network_after_short configuration event_before wire<> state 
	  then 
	    Erase_event (event_before_long)::working_list
	  else 
	    Stuck_event(event_before_long)::
	    (
	    let working_list =
	      if Data.story_propagation_strategy = 1
	      then working_list 
	      else 
	       if state = "_" then working_list 
	       else 
		 let rec aux short_step old = 
		   let event_before = event_before_step short_step in 
		   if event_before   = (-1) 
		   then 
		     match old with None -> (print_string "1:???\n";working_list) 
		     | Some i -> (Select_event(i)::working_list)
		   else
		     let event_before_long = configuration.network.sparse_matrix.(wire).(event_before).eid in
		     try (
		       if IntMap.find event_before_long configuration.selected_event_map 
		       then 
			 (let state' = network_after_short configuration event_before wire in
			 if state=state' then working_list
			 else
			   match old with None -> (print_string "2:???\n";working_list)
			   | Some i -> Select_event(i)::working_list)
		       else
			 aux (short_step-1) old 
			   ) 
		     with Not_found ->
		       (let state' = network_after_short configuration event_before wire in 
		       if  state' = state 
		       then 
			 match old with None -> 
			   aux (short_step-1) (Some event_before_long)
			 | Some _ -> working_list  
			      
		       else
			 (aux (short_step-1) old)) in
		 aux (shortstep) None 
		 in working_list) 
	     in 
	
      
      let working_list = 
	if state <> "_" 
	then Select_event(configuration.network.sparse_matrix.(wire).(0).eid )::working_list 
	else working_list in
      let working_list =
	if event_after = configuration.network.nevents_by_wire.(wire) then working_list
	else 
	  let event_after_long = configuration.network.sparse_matrix.(wire).(event_after).eid in 
	  if (try (not (IntMap.find event_after_long configuration.selected_event_map))with Not_found -> false)
	    (*or (try network_after_short configuration event_after wire = state with Not_found -> false)*)
	    
	  then (Select_value (shortstep+1,wire,state))::working_list
	  else if network_before_short configuration event_after wire <> state 
	  then (Erase_event (event_after_long)::working_list)
	  else Stuck_event (event_after_long)::
	  (
	  let working_list =
	    if Data.story_propagation_strategy = 1
	    then working_list 
	    else 
	      let rec aux short_step old = 
		   let event_after = event_after_step short_step in 
		   if event_after   = configuration.network.nevents_by_wire.(wire) 
		   then working_list
		   else
		     let event_after_long = configuration.network.sparse_matrix.(wire).(event_after).eid in
		     try (
		       if IntMap.find event_after_long configuration.selected_event_map 
		       then 
			 (let state' = network_before_short configuration event_after wire in
			 if state=state' then working_list
			 else
			   match old with None -> (print_string "2:???\n";working_list)
			   | Some i -> Select_event(i)::working_list)
		       else
			 aux (short_step+1) old 
			   ) 
		     with Not_found ->
		       (let state' = network_before_short configuration event_after wire in 
		       if state' = state 
		       then 
			 match old with None -> 
			   aux (short_step+1) (Some event_after_long)
			 | Some _ -> working_list  
			      
		       else
			 (aux (short_step+1) old)) in
		 aux (shortstep) None 
		 in working_list) 
	   in
      working_list 


    let propagate working_list configuration stack forbid = 
      match working_list with 
	[] -> Some ([],configuration,stack) 
      | t::q -> 
	  begin
	    try 
	      ( match t 
	      with 
		Stuck_event event ->
		  begin 
		    if event<0 or event>=configuration.n_event or 
		      not (IntSet.mem event configuration.remaining_event) then 
		      (Some (q,configuration,stack))
 		    else
 		      (Some (q,{configuration with filo = event::configuration.filo},stack)) 
		  end 
	      |		Select_event event -> 
		  begin

		  
		    if event<0 or event>=configuration.n_event then Some(q,configuration,stack)
	            else
		       (try 
			 (if (not (IntMap.find event configuration.selected_event_map)) 
 			 then let _ = restore configuration stack in None 
			 else (Some (q,configuration,stack)))
		       with Not_found -> 
			 let _ = trace_print ("SELECT_EVENT "^(string_of_int event)) in 
			 let configuration = 
			  {configuration with 
			    selected_event_map = IntMap.add event true configuration.selected_event_map ;
			    remaining_event = IntSet.remove event configuration.remaining_event} in
			let list_wire = 
			  configuration.network.eid_to_ports.(event) in 
			let working_list = 
			  PortIdSet.fold
			    (fun wire l ->
			      let short_event = EidMap.find event configuration.network.short_events.(wire) in 
			      let case = configuration.network.sparse_matrix.(wire).(short_event) in 
			      let a = case.state_before in 
			      let b = case.state_after in 
			      let l = 
				Select_value (step_before_event short_event,wire,a)::l
			      in
			      let l = 
				Select_value (step_after_event short_event,wire,b)::l
			      in
			      l
				)
			    list_wire q in
			let working_list = 
			  List.fold_left
			    (fun sol j ->
			      if j = event then sol else 
			      (Erase_event j)::sol)
			    working_list (forbid event) in 
			Some(working_list,configuration,stack)) 
		  end
	      | Erase_event event -> 
		  begin
		    if event<0 or event >= configuration.n_event 
		    then Some (q,configuration,stack) 
		    else
		      (try 
			if IntMap.find event configuration.selected_event_map
			then (let _ = restore configuration stack in None)
			else Some (q,configuration,stack)
		      with
			Not_found -> 
			   let _ = trace_print ("ERASE_EVENT "^(string_of_int event)) in
		    	   let configuration = 
			     {configuration with 
			       selected_event_map = IntMap.add event false configuration.selected_event_map ;
			       remaining_event = IntSet.remove event configuration.remaining_event} in
			   let list_wire = configuration.network.eid_to_ports.(event) in 
			   let working_list = 
			     PortIdSet.fold
			       (fun wire l ->
				 let short_event = EidMap.find event configuration.network.short_events.(wire) in 
				 let a = configuration.status.(wire).(step_before_event short_event) in
				 let b = configuration.status.(wire).(step_after_event short_event) in
				 let l = 
				match a with None -> l 
				| Some a -> 
				    Select_value (step_after_event short_event,wire,a)::l
			      in
			      let l = 
				match b with None -> l 
				| Some b -> 
				    Select_value (step_before_event short_event,wire,b)::l in
			      l
				)
			   list_wire q in
			
			Some(working_list,configuration,stack) 
		  ) end
	      | Select_value (step,wire,s) ->
		  let _ = trace_print ("Select_value: "^(string_of_cell (wire,
									 compute_before_after (step-1) wire configuration,Some s))) in 
		  begin
		    if step<0 or step >= configuration.network.nevents_by_wire.(wire) 
		    then Some(q,configuration,stack)
		    else 
		      let old = configuration.status.(wire).(step) in
		      match old with 
			Some x when x=s -> Some (q,configuration,stack)
		      | None -> 
			  (configuration.status.(wire).(step)<- Some s;
			   Some (
			   (check_short step wire configuration q),
			   configuration,
			   ((step,wire,old)::stack)))
		      | Some y -> 
			  (
			  let _ = 
			    (if !trace
			    then 
			      let _ = print_string "FAIL" in
			      let _ = print_string " Step " in
			      let _ = print_int step in
			      let _ = print_string " Wire " in
			      let _ = print_int wire in 
			      let _ = print_string (" "^s^"."^y) in ()) in   
			  let _ = restore configuration stack in 
			  None)
		  end
		
	      ) with Exit -> (let _ = restore configuration stack in None)
     end
    let upgrade i = 
      match i with None -> []
      |	 Some i ->  [[Erase_event i];[Select_event i]]
    let upgrade_pos i = 
      Select_event i 
    let upgrade_neg i = 
      Erase_event i 
    let init_stack = []
    let init_output = []
	
	
	      
	  end:Input with type choice = int)



module Solve = 
  functor (I:Input with type choice = int ) 
    ->
      struct 
	let check configuration  output = 
	  if I.is_solution configuration
	  then 
	    let o = I.store_solution configuration  output
	    in 
	    let _ = 
	      if !trace then 
		begin 
		  let _ = print_string "SUCCESS\n We keep events: " in
		  let _ = I.dump_output o in ()

		end
	    in 
	    true,o 
	  else 
	    false,output
	let rec propagate workinglist configuration stack forbid =
	  match workinglist 
	    with [] -> Some (configuration,stack)
	  | _ -> match I.propagate workinglist  configuration stack forbid  
		with None -> None
		| Some (w,c,s) -> propagate w c s forbid 

	let rec visit_all call_stack output forbid = (*tail recursive *)
	  match call_stack with 
	    []  -> output 
	  | ([],configuration,undo)::call_stack ->
	      let _  = I.restore configuration undo in
	      visit_all call_stack  output forbid 
	  | (t::q,configuration,undo)::call_stack -> 
	      let undo_stack = I.init_stack in
	      match propagate t configuration undo_stack forbid with
		None -> visit_all ((q,configuration,undo)::call_stack)  output forbid 
	      |	Some(configuration',undo_stack') -> 
		  let bool,output = check configuration' output in
		  let choice,configuration' = I.best_choice configuration' in
		  visit_all 
		    ((I.upgrade choice,configuration',undo_stack')::(q,configuration,undo)::call_stack) 
		    output
		    forbid 
	let visit_all configuration output forbid  =
	  let choice,configuration = I.best_choice configuration in 
	  configuration,
	  visit_all 
	    [I.upgrade choice,configuration,I.init_stack]
	    output  
	    forbid 
	let rec visit_first call_stack output forbid = (*tail recursive *)
	  match call_stack with 
	    []  -> output 
	  | ([],configuration,undo)::call_stack ->
	      let _  = I.restore configuration undo in
	      visit_first call_stack  output forbid 
	  | (t::q,configuration,undo)::call_stack -> 
	      let undo_stack = I.init_stack in
	      match propagate t configuration undo_stack forbid with
		None -> visit_first ((q,configuration,undo)::call_stack)  output forbid 
	      |	Some(configuration',undo_stack') -> 
		  let bool,output = check configuration' output  in
		  if bool then output 
		  else 
		    let choice,configuration' = I.best_choice configuration' in 
		  visit_first 
		    ((I.upgrade choice,configuration',undo_stack')::(q,configuration,undo)::call_stack) 
		    output forbid 
	
	let visit_first configuration output forbid  =
	  let choice,configuration = I.best_choice configuration in 
	  configuration,visit_first
	    [I.upgrade choice ,configuration,I.init_stack]
		    output forbid 
	let rep = ref None
	let main mode (network,(erased,mandatory),forbid)  = 
	  let visit = 
	    if mode = Data.ALL then visit_all
	    else if mode = Data.FIRST then visit_first
	    else raise Exit in
	  
	  let init = I.sol_init network in
	  let properties = 
	    (List.rev_map I.upgrade_neg erased)@(List.rev_map I.upgrade_pos mandatory) in
	  match propagate properties init (I.init_stack) forbid 
	  with None -> (init,I.init_output)
	  | Some (configuration,_) ->
	      let bool,output = check configuration I.init_output in 
	      let _ = rep:=Some (configuration,output) in 
	      if bool  then (configuration,output)
	      else
		try (let rep = visit configuration  output forbid 
                     in rep)
		with Exit -> 
		  match !rep with  Some(a,b) -> ((a,b)) 
		  | None -> (configuration,output)
		
	let dump_output = I.dump_output
	    	
	let fold_first_output = I.fold_first_output
	let fold_first_output_remove = I.fold_first_output_remove 
	let empty_output = I.empty_output
	    

      end

 

    let print_network network = 
      let adjust i = i in 
      let _ = print_string "CONSTRAINTS \n" in
      let _ = print_string "A wire is described as WIRE: n(desc)::(eid:before,after;)*, where 'n' is the wire index, 'eid' is the event index, 'before' is the state of the wire 'wire' before event 'eid' (if the event 'eid' is selected), 'after' is the state of the wire 'wire' after the event 'eid' (if the event 'eid' is selected).\n" in   
      let decode_map = 
	let map = 
	  QuarkMap.fold 
	    (fun quark i map -> IntMap.add i quark map)
	    network.wire_map IntMap.empty  
	in 
	(fun i -> 
	  try IntMap.find i map
	  with Not_found -> raise Exit)
      in 
      let print_port ((i,x),s) = 
	print_string "(Agent ";
	print_int i;
	if x="_!"
	then print_string ",is present?)"
	else 
	  begin 
	    print_string ",site ";
	    print_string (String.sub x 0 ((String.length x)-1));
	    print_string ",";
	    print_string 
	      (match s with 
		State ->   "internal state"
	      | Binding -> "to which site?"
	      | AnyBound ->"free / bound? ");
	    print_string ")" 
	  end 
      in 
	  
      Array.iteri 
	(fun port r -> 
	  let _ = print_string "WIRE: " in 
	  let _ = print_int port in
	  let _ = print_port (decode_map port) in 
	  let _ = print_string ":: " in 
	  let _ = 
	    Array.iteri (
	    fun a i -> 
	      print_int i.eid;
	      print_string ":"; 
	      match i with 
		case -> print_string ((adjust case.state_before)^","^(adjust case.state_after)^" ; ")
		    ) r 
	  in print_newline ())
	network.sparse_matrix    

let last s = String.sub s ((String.length s)-1) 1 
let is_state port = last (snd port) = "~"
let is_binding port = last (snd port) = "!"  
let is_tail port = String.sub (snd port) 0 1 = "_"
module Convert =
  struct
      let enrich_net (a:Network.t)   = 
      let n_event = a.Network.fresh_id in
      let kept_events = 
	Network.EventArray.fold 
	  (fun i j -> IntSet.add i)
	  a.Network.events
	  IntSet.empty in
      let kept_port = 
	PortMap.fold
	  (fun port wire sol ->
	    if Network.Wire.exists 
		(fun (eid,_) -> 
		  IntSet.mem eid kept_events)
		wire
	    then PortSet.add port sol else sol)
	  a.Network.wires 
	  PortSet.empty in 

      let (erased,mandatory)  = 
	let rec aux k (erased,mandatory) = 
	  if k=(-1) then (erased,mandatory)
	  else aux (k-1)
	      (try (let e = Network.EventArray.find k a.Network.events in
	      if e.Network.kind = 2 then (erased,k::mandatory)
	      else (erased,mandatory))
	      with Not_found -> (k::erased,mandatory))
	in aux (n_event-1) ([],[]) in
      let erased_set = 
	List.fold_left 
	  (fun sol x -> IntSet.add x sol) 
	  IntSet.empty erased in
      let sol = 
	PortMap.fold
	  (fun port wire sol ->
	    if not (PortSet.mem port kept_port)
	    then sol 
	    else 
	      let fadd eid port (image:Rule.modif_type) events = 
		let event  = 
		  try EventArray.find eid events 
		  with Not_found -> 
		    {(EventArray.find eid a.Network.events) with nodes = PortMap.empty}  in 
		EventArray.add eid 
		    {event with nodes = PortMap.add  port [image] event.nodes} events in 
	      let sol,_,_  = 
		Network.Wire.fold_left
		  (fun (events,old_bound,old_state) (eid,rule) ->
		   
		    if IntSet.mem eid erased_set then (events,old_bound,old_state)
		    else
		      match rule with 
			  Rule.Before_After (x,y) -> 
			    raise Exit
			  
			 | Rule.Bound _ | Rule.Break _ | Rule.Side_break _  -> 
			    
			    (fadd eid port (Rule.Before_After(old_bound,rule)) events),rule,old_state
			| Rule.Init_bound (ag,i,s) ->
			    fadd eid port rule events,Rule.Bound(i,s),old_state
			| Rule.Init_free  ag -> 
			    fadd eid port rule events,rule,old_state
			| Rule.Marked _  | Rule.Test_marked _ ->
			    (fadd eid port (Rule.Before_After(old_state,rule)) events),old_bound,rule
			| Rule.Init_mark (a,mark) -> 
			    fadd eid port rule events,old_bound,Rule.Marked(a,mark)
			| Rule.Remove -> 
			    (fadd eid port rule events,old_bound,old_state)
			    
			| Rule.Test_bound _ | Rule.Test_free | Rule.Test_any_bound  ->
			    fadd eid port (Rule.Before_After(old_bound,rule)) events ,old_bound,old_state
			
			 			  )		  
		  (sol,Rule.Remove,Rule.Remove)    wire  in sol )
	  a.Network.wires (EventArray.create 100)
	      in sol 



      let expand (a:Network.t) = 
	let n_event = a.Network.fresh_id in
	let kept_events = 
	  Network.EventArray.fold 
	    (fun i j -> IntSet.add i)
	    a.Network.events
	    IntSet.empty in
	let kept_port = 
	  PortMap.fold
	    (fun port wire sol ->
	       if Network.Wire.exists 
		 (fun (eid,_) -> 
		    IntSet.mem eid kept_events)
		 wire
	       then PortSet.add port sol else sol)
	    a.Network.wires 
	    PortSet.empty in 
	let (erased,mandatory)  = 
	  let rec aux k (erased,mandatory) = 
	    if k=(-1) then (erased,mandatory)
	    else aux (k-1)
	      (try (let e = Network.EventArray.find k a.Network.events in
		      if e.Network.kind = 2 then (erased,k::mandatory)
		      else (erased,mandatory))
	       with Not_found -> (k::erased,mandatory))
	  in aux (n_event-1) ([],[]) in
	let erased_set = 
	  List.fold_left 
	    (fun sol x -> IntSet.add x sol) 
	    IntSet.empty erased in
        let id_to_name  = 
	  PortMap.fold
	    (fun port wire sol ->
	       if not (PortSet.mem port kept_port)
	       then sol 
	       else
		 let i = fst port in 
		 let sol  = 
		   Network.Wire.fold_left
		     (fun sol (eid,rule) ->
			if IntSet.mem eid erased_set then sol
			else 
			  let rec aux rule sol = 
			    match rule with 
				Rule.Test_bound _ | Rule.Test_marked _  | Rule.Test_any_bound  | Rule.Test_free | Rule.Remove | Rule.Marked _ | Rule.Side_break _ | Rule.Bound _ | Rule.Break _ -> sol
			      | Rule.Init_mark (ag,_) | Rule.Init_bound (ag,_,_) | Rule.Init_free ag ->
				  IntMap.add i ag sol
			      | Rule.Before_After (y,x) -> aux x (aux y sol)
			  in aux rule sol)
		     sol wire 
		 in sol
	    )
	    a.Network.wires IntMap.empty in
	let agent_to_wire =
	  IntMap.fold
	    (fun i a map -> 
	       let old = try StringMap.find a map with Not_found -> IntSet.empty in
		 StringMap.add a (IntSet.add i old) map)
	    id_to_name StringMap.empty in
	  (*	let _ = 
		if !trace then 
		let _ = 
		print_string "AGENT_TO_WIRE\n" in 
		let _ = 
		StringMap.iter 
		(fun i x -> print_string i;
		IntSet.iter (fun x -> print_int x;print_string ";") x;print_newline ())
		agent_to_wire in 
		() in *)
	let agent_to_wire = StringMap.map (fun y -> IntSet.fold (fun x a -> x::a) y []) agent_to_wire in 
	  
	let net' = (Network.empty (),None,IntMap.empty) in
	let permutation_of_modifs modifs = 
	  let arg = 
	    PortMap.fold 
	      (fun ((i,_)) a sol -> 
		 let rec aux a sol =  
		   match a with 
		       Rule.Bound (j,_)  | Rule.Test_bound (j,_) | Rule.Side_break (j,_) | Rule.Break (j,_) | Rule.Init_bound (_,j,_) -> 
			 IntSet.add j sol
			   
		     | Rule.Test_marked _ | Rule.Test_any_bound | Rule.Test_free | Rule.Remove | Rule.Marked _ | Rule.Init_mark _  | Rule.Init_free _  -> sol
		     | Rule.Before_After (x,y) -> aux x (aux y sol)
		 in List.fold_left 
		      (fun sol a -> aux a sol)
		      (IntSet.add i sol) a)
	      modifs IntSet.empty in 
	  let perm = 
	    IntSet.fold 
	      (fun i partial_perm_list -> 
		 List.fold_right  
		   (fun j extended_perm_list -> 
		      List.fold_left 
			(fun sol (rho,codomain) -> 
			   if IntSet.mem j codomain then sol
			   else (IntMap.add i j rho,IntSet.add j codomain)::sol)
			extended_perm_list partial_perm_list)
		   (try (StringMap.find 
			   (IntMap.find i id_to_name) agent_to_wire) with Not_found -> print_string "BUG STORY COMPRESSOR 861";print_int i ;(try (print_string (IntMap.find i id_to_name)) with Not_found -> ());print_newline ();[i](*raise Exit*)) [])
	      arg [IntMap.empty,IntSet.empty] in 
	    List.map fst perm in 
	  
	let apply perm (rule,modif) = 
	  let f x = IntMap.find x perm in
	  let rec apply f act = 
	    match  act with   Rule.Bound (j,b) -> Rule.Bound (f j,b)
	      | Rule.Test_bound (j,b) ->   Rule.Test_bound (f j,b)
	      | Rule.Side_break (j,b) -> Rule.Side_break (f j,b) 
	      | Rule.Break (j,b) -> Rule.Break (f j,b)
	      | Rule.Init_bound (x,j,y) -> Rule.Init_bound (x,f j,y)
	      | Rule.Test_marked _ 
	      | Rule.Test_any_bound  
	      | Rule.Test_free 
	      | Rule.Remove 
	      | Rule.Marked _  
	      | Rule.Init_mark _    
	      | Rule.Init_free _ -> act
	      | Rule.Before_After (x,y) -> Rule.Before_After (apply f x,apply f y) in 
	    
	  let rule' = rule in
	  let modif' = 
	    PortMap.fold 
	      (fun ((i,a)) act  -> 
		 PortMap.add ((f i,a)) (List.map (apply f) act))
	      modif PortMap.empty 
	  in rule',modif' 
	in
	  begin
	    let old_events = enrich_net a in 
	    let add j (net',flg,forbid)  =
	      let k = net'.Network.fresh_id in 
	      let event = EventArray.find j old_events in
	      let modifs = event.Network.nodes in  
	      let perms = permutation_of_modifs modifs in 
	      let _ = 
		if !trace 
		then 
		  let _ = print_string "PERMUTATIONS "in 
		  let _ = print_newline () in  
		  let _ = 
		    List.iter 
		      (fun w->
			 IntMap.iter (fun x y -> print_int x;print_string "->";print_int y;print_string ";") w;print_newline ()) perms in 
		  let _ = print_newline () in () in 
	      let net',flg = 
		match event.Network.kind with 
		    0 -> 
		      List.fold_left  
			(fun (net',flag) sigma -> 
			   add_intro (apply sigma (event.Network.label,modifs)) false net' ,flg) (net',flg) perms   
		  | 1 ->
		      List.fold_left  
			(fun (net',flag) sigma ->
			   re_add net' (apply sigma  (event.Network.r,modifs)) false,flg) (net',flg) perms
		  | _ -> re_add net' ((event.Network.r,modifs)) false ,event.Network.r.Rule.flag
	      in 
	      let l = net'.Network.fresh_id in 
	      let list = 
		let rec aux i sol = 
		  if i=l then sol else aux (i+1) (i::sol) in aux k [] in
		net',flg,List.fold_left 
		  (fun sol i -> IntMap.add i l sol) forbid list 
	    in 
	    let net,flag,forbid = 
	      EventArray.fold
		(fun j _ sol -> add j sol)
		a.events 
		net'
	    in
	    let net = 
	      opt_extract 
		(Network.cut net 
		   (Mods2.IntSet.empty,
		    match flag with 
			Some f -> f 
		      | None ->  
			  let s = "Simulation.iter: obs has no flag" in
			    runtime 
			      (Some "story_compressor.ml",
			       Some 897,
			       Some s)
			      s) 
		)
	    in 
	      Some net 
	  end
	
	    
      let try_permutation (a:Network.t) eid  = 
	let n_event = a.Network.fresh_id in
	let k = eid in
	let event = 
	  try Some (EventArray.find k a.Network.events) with Not_found -> None in
	  match event with None -> [] | Some event -> 
	    let modifs = event.Network.nodes in  
	      
	    let kept_events = 
	      Network.EventArray.fold 
		(fun i j -> IntSet.add i)
		a.Network.events
		IntSet.empty in
	    let kept_port = 
	      PortMap.fold
		(fun port wire sol ->
		   if Network.Wire.exists 
		     (fun (eid,_) -> 
			IntSet.mem eid kept_events)
		     wire
		   then PortSet.add port sol else sol)
		a.Network.wires 
		PortSet.empty in 
            let (erased,mandatory)  = 
	      let rec aux k (erased,mandatory) = 
		if k=(-1) then (erased,mandatory)
		else aux (k-1)
		  (try (let e = Network.EventArray.find k a.Network.events in
			  if e.Network.kind = 2 then (erased,k::mandatory)
			  else (erased,mandatory))
		   with Not_found -> (k::erased,mandatory))
	      in aux (n_event-1) ([],[]) in
            let erased_set = 
	      List.fold_left 
		(fun sol x -> IntSet.add x sol) 
		IntSet.empty erased in
	    let id_to_name  = 
	      PortMap.fold
		(fun port wire sol ->
		   if not (PortSet.mem port kept_port)
		   then sol 
		   else
		     let i = fst port in 
		     let sol  = 
		       Network.Wire.fold_left
			 (fun sol (eid,rule) ->
			    if IntSet.mem eid erased_set (*or eid>k*) then sol
			    else 
			      let rec aux rule sol = 
				match rule with 
				    Rule.Test_bound _ | Rule.Test_marked _  | Rule.Test_any_bound  | Rule.Test_free | Rule.Remove | Rule.Marked _ | Rule.Side_break _ | Rule.Bound _ | Rule.Break _ -> sol
				  | Rule.Init_mark (ag,_) | Rule.Init_bound (ag,_,_) | Rule.Init_free ag ->
				      IntMap.add i ag sol
				  | Rule.Before_After (y,x) -> aux x (aux y sol)
			      in aux rule sol)
			 sol wire 
		     in sol
		)
		a.Network.wires IntMap.empty in
	    let agent_to_wire =
	      IntMap.fold
		(fun i a map -> 
		   let old = try StringMap.find a map with Not_found -> IntSet.empty in
		     StringMap.add a (IntSet.add i old) map)
		id_to_name StringMap.empty in
	    let agent_to_wire = StringMap.map (fun y -> IntSet.fold (fun x a -> x::a) y []) agent_to_wire in 
	    let permutation  = 
	      let arg = 
		PortMap.fold 
		  (fun ((i,_)) a sol -> 
		     let rec aux a sol =  
		       match a with 
			   Rule.Bound (j,_)  | Rule.Test_bound (j,_) | Rule.Side_break (j,_) | Rule.Break (j,_) | Rule.Init_bound (_,j,_) -> 
			     IntSet.add j sol
			       
			 | Rule.Test_marked _ | Rule.Test_any_bound | Rule.Test_free | Rule.Remove | Rule.Marked _ | Rule.Init_mark _  | Rule.Init_free _  -> sol
			 | Rule.Before_After (x,y) -> aux x (aux y sol)
		     in 
		       List.fold_left 
			 (fun sol a -> aux a sol)
			 (IntSet.add i sol) a)
		  modifs IntSet.empty in 
	      let perm = 
		IntSet.fold 
		  (fun i partial_perm_list -> 
		     List.fold_right  
		       (fun j extended_perm_list -> 
			  List.fold_left 
			    (fun sol (rho,codomain) -> 
			       if j=try (Mods2.IntMap.find i rho) with Not_found -> i
			       then (rho,codomain)::sol
			       else
				 if IntSet.mem j codomain (**) or IntSet.mem i codomain (**) then sol
				 else ((**)Mods2.IntMap.add j i (**)(Mods2.IntMap.add i j rho),(**)IntSet.add i(**) (IntSet.add j codomain))::sol)
			    extended_perm_list partial_perm_list)
		       (try (StringMap.find 
			       (IntMap.find i id_to_name) agent_to_wire) with Not_found -> print_string "BUG STORY COMPRESSOR 1068 (when introduced species are not atomic)";print_int i ;(try (print_string (IntMap.find i id_to_name)) with Not_found -> ()); [i]) [])
		  arg [Mods2.IntMap.empty,IntSet.empty] in 
		List.map fst perm in 
	      
	    let apply perm (rule,modif) = 
	      let f x = try Mods2.IntMap.find x perm with _ -> x in
	      let rec apply f act = 
		match  act with   Rule.Bound (j,b) -> Rule.Bound (f j,b)
		  | Rule.Test_bound (j,b) ->   Rule.Test_bound (f j,b)
		  | Rule.Side_break (j,b) -> Rule.Side_break (f j,b) 
		  | Rule.Break (j,b) -> Rule.Break (f j,b)
		  | Rule.Init_bound (x,j,y) -> Rule.Init_bound (x,f j,y)
		  | Rule.Test_marked _ 
		  | Rule.Test_any_bound  
		  | Rule.Test_free 
		  | Rule.Remove 
		  | Rule.Marked _  
		  | Rule.Init_mark _    
		  | Rule.Init_free _ -> act
		  | Rule.Before_After (x,y) -> Rule.Before_After (apply f x,apply f y) in 
		
	      let rule' = rule in
	      let modif' = 
		PortMap.fold 
		  (fun ((i,a)) act  -> 
		     PortMap.add ((f i,a)) (List.map (apply f) act))
		  modif PortMap.empty 
	      in rule',modif' 
	    in
	      begin
		let add j (sigma:int Mods2.IntMap.t)  (net',flg)  =
		  let event = EventArray.find j a.Network.events in
		  let modifs = event.Network.nodes in  
		  let arg =
		    if j<eid then IntSet.empty
		    else
		      PortMap.fold 
			(fun ((i,_)) a sol -> 
			   let rec aux a sol =  
			     match a with 
				 Rule.Bound (j,_)  | Rule.Test_bound (j,_) | Rule.Side_break (j,_) | Rule.Break (j,_) | Rule.Init_bound (_,j,_) -> 
				   IntSet.add j sol
				     
			       | Rule.Test_marked _ | Rule.Test_any_bound | Rule.Test_free | Rule.Remove | Rule.Marked _ | Rule.Init_mark _  | Rule.Init_free _  -> sol
			       | Rule.Before_After (x,y) -> aux x (aux y sol)
			   in 
			     List.fold_left 
			       (fun sol a -> aux a sol)
			       (IntSet.add i sol)
			       a)
			modifs IntSet.empty in
		  let sigma0 = sigma in 
		  let sigma = if j<eid then Mods2.IntMap.empty else sigma in 
		  let f i = try Mods2.IntMap.find i sigma with Not_found -> i in
		  let _ = 
		    IntSet.fold 
		      (fun i sol -> let j = f i in
			 if IntSet.mem j sol then raise Exit
			 else IntSet.add j sol)
		      arg IntSet.empty in 
		  let new_event = 
		    if j<eid 
		    then 
		      (event.Network.r,modifs)
		    else
		      apply sigma  (event.Network.r,modifs)
		  in
		  let (net',sigma),flg = 
		    match event.Network.kind with 
			0 -> (add_intro (event.label,snd new_event) false net',sigma),flg
			  
		      | 1 -> 
			  let a,b,c = re_add_rename sigma net' new_event  false in (a,b),flg
		      | _ -> let a,b,c = re_add_rename sigma net' new_event false in if c then (a,b),event.Network.r.Rule.flag else (raise Exit)
		  in (net',if j<eid then sigma0 else sigma),flg
		in
		  List.fold_left
		    (fun sol sigma ->
		       try 
			 let (net,flag),sigma  = 
			   EventArray.fold
			     (fun j _ (sol,sigma) -> 
				let (a,b),c = add 
				  j 
				  sigma 
				  sol in (a,c),b)
			     a.events 
			     ((Network.empty (),None),sigma) in
			   
			 let net = opt_extract
			   (Network.cut net (Mods2.IntSet.empty, 
					     match flag with Some f -> f | None ->  let s = "Simulation.iter: obs has no flag"
					     in
					       Error.runtime
						 (Some "story_compressor.ml",
						  Some 1104,
						  Some s)
						 s
					    ) 
			   )
			 in 
			   (Some (net,sigma))::sol with _ -> sol)
 		    [] permutation 
	      end
	
  
    let convert (a:Network.t)  granularity = 
      let n_event = a.Network.fresh_id in
      let kept_events = 
	Network.EventArray.fold 
	  (fun i j -> IntSet.add i)
	  a.Network.events
	  IntSet.empty in
      let kept_port = 
	PortMap.fold
	  (fun port wire sol ->
	    if Network.Wire.exists 
		(fun (eid,_) -> 
		  IntSet.mem eid kept_events)
		wire
	    then PortSet.add port sol else sol)
	  a.Network.wires 
	  PortSet.empty in 

      let (n_wire,wire_map) = 
	PortMap.fold 
	  (fun port _ (n,wire_map) ->
	    if not (PortSet.mem port kept_port) then (n,wire_map) else 
	    begin
	    if is_state port then 
	     begin
	       if is_tail port then 
		 (n,wire_map)
	       else 
	      (n+1,
	       QuarkMap.add (port,State) n wire_map)
	     end
	    else if is_binding port  then 
	      begin
		if is_tail port then 
		  (n+1,
		   QuarkMap.add (port,AnyBound) n wire_map)
		else 
		  (n+2,
		   QuarkMap.add (port,AnyBound) n (
		   QuarkMap.add (port,Binding) (n+1) wire_map))
	      end
	    else (print_string (snd port);raise Exit)
	  end  )
	  a.Network.wires  (0,QuarkMap.empty) in
    
      let wire_qmap = wire_map in 
      let wire_map i = QuarkMap.find i wire_map in
      let (erased,mandatory)  = 
	let rec aux k (erased,mandatory) = 
	  if k=(-1) then (erased,mandatory)
	  else aux (k-1)
	      (try (let e = Network.EventArray.find k a.Network.events in
	      if e.Network.kind = 2 then (erased,k::mandatory)
	      else (erased,mandatory))
	      with Not_found -> (k::erased,mandatory))
	in aux (n_event-1) ([],[]) in
      let erased_set = 
	List.fold_left 
	  (fun sol x -> IntSet.add x sol) 
	  IntSet.empty erased in
      let string_of = 
	if granularity = Data.WEAK 
	then string_of_int 
	else 
	  (fun x ->

	      let sol = 
		PortMap.fold
		  (fun port wire sol ->
		    if not (PortSet.mem port kept_port)
		    then sol 
		    else
		      let i = fst port in 
		      let sol  = 
			Network.Wire.fold_left
			  (fun sol (eid,rule) ->
			    if IntSet.mem eid erased_set then sol
			    else 
			      let rec aux rule = 
				match rule with 
				  Rule.Test_bound _ | Rule.Test_marked _  | Rule.Test_any_bound  | Rule.Test_free | Rule.Remove | Rule.Marked _ | Rule.Side_break _ | Rule.Bound _ | Rule.Break _ -> sol
				| Rule.Init_mark (ag,_) | Rule.Init_bound (ag,_,_) | Rule.Init_free ag ->
				    IntMap.add i ag sol
				| Rule.Before_After (_,x) -> aux x 
			      in aux rule) 
			  sol wire 
		      in sol
			)
		  a.Network.wires IntMap.empty in
		      try (IntMap.find x sol) with Not_found -> string_of_int x) in

     
       let sol = 
	PortMap.fold
	  (fun port wire sol ->
	    if not (PortSet.mem port kept_port)
	    then sol 
	    else 
	      let fadd eid port image map = 
		let old = 
		  try (IntMap.find port map) with Not_found -> EidMap.empty in
		IntMap.add port (EidMap.add eid image old) map in 
	      let sol,_,_,_  = 
		Network.Wire.fold_left
		  (fun (map,old_anybound,old_bound,old_state) (eid,rule) ->
		    if IntSet.mem eid erased_set then (map,old_anybound,old_bound,old_state)
		    else
		      let rec f rule bool (map,old_anybound,old_bound,old_state) = 
			let fadd = if bool then fadd else (fun a b c d -> d)
			  in
			match rule with 
			  Rule.Before_After (x,y) -> 
			    f y true (f x false (map,old_anybound,old_bound,old_state))
			| Rule.Bound (i,s) -> 
			    (let new_anybound = "Bound" in
			    let new_bound = (string_of  i)^"."^s in  
			    let map = fadd eid (wire_map (port,AnyBound)) (old_anybound,new_anybound) map in
			    let map = 
			      if not (is_tail port) 
			    then 
				fadd eid (wire_map (port,Binding)) (old_bound,new_bound) map 
			      else map in 
			    (map,new_anybound,new_bound,old_state))
			| Rule.Init_bound (ag,i,s) ->
			    (let new_anybound = "Bound" in
			    let new_bound = (string_of  i)^"."^s in  
			  let map = fadd eid (wire_map (port,AnyBound)) (old_anybound,new_anybound) map in
			  let map = 
			    if not (is_tail port) 
			    then 
			      fadd eid (wire_map (port,Binding)) (old_bound,new_bound) map 
			    else map in 
			  (map,new_anybound,new_bound,old_state))
			      
			    
		      | Rule.Break _ -> 
			  (let new_anybound = "Free" in
			  let new_bound = "Free" in  
			  let map = 
			    fadd eid (wire_map (port,AnyBound)) (old_anybound,new_anybound) map in 
			  let map = 
			    if not (is_tail port) 
			    then
			      fadd eid (wire_map (port,Binding)) (old_bound,new_bound) map 
			    else map in
			  (map,new_anybound,new_bound,old_state))  
		      | Rule.Init_free  ag -> 
			  (let new_anybound = "Free" in
			  let new_bound = "Free" in  
			  let map = 
			    fadd eid (wire_map (port,AnyBound)) (old_anybound,new_anybound) map in 
			  let map = 
			    if not (is_tail port) 
			    then
			      fadd eid (wire_map (port,Binding)) (old_bound,new_bound) map 
			    else map in
			  (map,new_anybound,new_bound,old_state))
		      | Rule.Side_break (i,s) -> 
			  (if old_anybound = "Bound"
			  then (
			    let new_anybound = "Free" in
			    let new_bound = "Free" in  
			    let map = 
			      fadd eid (wire_map (port,AnyBound)) (old_anybound,new_anybound) map in 
			    let map = 
			      if not (is_tail port) 
			      then
				fadd eid (wire_map (port,Binding)) (old_bound,new_bound) map 
			      else map in
			    (map,new_anybound,new_bound,old_state))
			  else 
			    let map = fadd eid (wire_map (port,AnyBound)) ("Free","Free") map in (map,"Free","Free",old_state))
		      | Rule.Marked (_,mark) ->
			  let map = 
			    if not (is_tail port) 
			    then 
			      fadd eid (wire_map (port,State)) (old_state,mark) map 
			    else map in 
			  (map,old_anybound,old_bound,mark)
		      | Rule.Init_mark (ag,mark) -> 
			  let map = 
			    if not (is_tail port) 
			    then 
			      fadd eid (wire_map (port,State)) (old_state,mark) map 
			    else map in 
			  (map,old_anybound,old_bound,mark)
			    
		      | Rule.Remove -> 
			  (let new_anybound = "_" in
			  let new_bound = "_" in  
			  let map = try (fadd eid (wire_map (port,AnyBound)) (old_anybound,new_anybound) map) with Not_found -> map  in
			  let map = 
			    if not (is_tail port) 
			    then 
			      let map = try (fadd eid (wire_map (port,Binding)) (old_bound,new_bound) map) with Not_found -> map in
			      let map = try (fadd eid (wire_map (port,State)) (old_state,"_") map) with Not_found -> map in map 
			    else map in
			  (map,new_anybound,new_bound,old_state))
			    
		      | Rule.Test_bound (i,s) ->
			  let map = fadd eid (wire_map (port,AnyBound)) ("Bound","Bound") map in
			  let map = 
			    if not (is_tail port) 
			    then 
			      fadd eid (wire_map (port,Binding)) ((string_of  i)^"."^s,(string_of  i)^"."^s) map 
			    else map 
			  in
			  (map,"Bound",(string_of i)^"."^s,old_state)
		      | Rule.Test_marked (mark) ->  
			  let map = fadd eid (wire_map (port,State)) (old_state,mark) map in 
			  (map,old_anybound,old_bound,mark)
		      | Rule.Test_any_bound -> 
			  let map = fadd eid (wire_map (port,AnyBound)) ("Bound","Bound") map in (map,"Bound",old_bound,old_state)
		      | Rule.Test_free -> 
			  let map = fadd eid (wire_map (port,AnyBound)) ("Free","Free") map in (map,"Free","Free",old_state) in 
		      f rule true (map,old_anybound,old_bound,old_state))
		  (sol,"_","_","_")    wire  in sol )
	   a.Network.wires IntMap.empty in
       let sparse_matrix = Array.make n_wire [||] in
       let nevents_by_wire = Array.make n_wire 0 in
       let eid_to_ports = Array.make n_event PortIdSet.empty in
       let fadd_eid_to_ports eid port = 
	 eid_to_ports.(eid)<-PortIdSet.add port (eid_to_ports.(eid)) in
       let short_events = Array.make n_wire EidMap.empty in
       let _ = 
	 IntMap.iter 
	   (fun wire eidmap ->
	     let n_event,event_map = 
	       EidMap.fold 
		 (fun x _ (i,map) ->
		   (i+1,EidMap.add x i map))
		 eidmap (0,EidMap.empty) in
	     let _ = nevents_by_wire.(wire) <- n_event in
	     let array = Array.make n_event empty_case in
	     let _ = short_events.(wire)<-event_map  in
	     let _ = sparse_matrix.(wire)<-array in 
	       let _ = 
	       EidMap.fold 
		 (fun x (a,b) i ->
		   array.(i)<-{port_id = wire ;
                               eid = x  ;
                               e_shortid = i ;
				state_before = a;
				state_after = b};
		   fadd_eid_to_ports x wire;
		   (i+1))
		 eidmap 0 in
	       ()) 
	   sol in 
	let _ = 
	  if !Mods2.bench_mode
	  then 
	    (print_string "Start compression \n";
	    print_int (n_event-(List.length erased)) ;
	    print_string " events \n";
	    print_int n_wire ;
	    print_string " wires ";
	    print_newline ())
	    in 
	    
      {nevents = n_event;
	wire_map = wire_qmap;
	nwires = n_wire;
	nevents_by_wire = nevents_by_wire ;
	sparse_matrix = sparse_matrix;
	eid_to_ports = eid_to_ports;
	short_events = short_events}
	 ,(erased,mandatory), fun x -> []  
  
  end
    
module A = Solve(Input)

let correct_depth net = 
  let events = 
    Network.EventArray.fold 
      Mods2.IntMap.add 
      net.Network.events
      Mods2.IntMap.empty 
  in 
    Mods2.IntMap.fold 
      (fun i e net' ->
	 let s_pred = Network.strong_preds i net
	 and w_pred = Network.weak_preds i net
	 in
	 let s_depth_pred = 
	   Mods2.IntSet.fold 
	     (fun j k -> max k (Network.event_of_id j net').Network.s_depth)
	     s_pred (-1) 
	 and g_depth_pred = 
	   Mods2.IntSet.fold
	     (fun j k -> max k (Network.event_of_id j net').Network.g_depth)
	     (Mods2.IntSet.union w_pred s_pred) (-1)
	 in
	 let s_depth = s_depth_pred+1 
	 and g_depth = g_depth_pred+1 
	 in
	   {net' with 
	      Network.events = 
	       Network.EventArray.add i {e with 
					   Network.s_depth = s_depth ;
					   Network.g_depth = g_depth
					} net'.Network.events }
      ) events net


let compress net iter_mode granularity =
  if !Data.story_compression then 
    let _ = back:= 0 in
      try 
	begin
	  let network,a,forbid  = Convert.convert net granularity in 
	  let _ = if !trace then 
	    let _ = print_network network in 
	    let _ = print_string "SELECT_EVENT(observable) " in
	    let _ = List.iter (fun x -> print_int x;print_newline ()) (snd a) in
	      () in 
	  let _,output = 
	    A.main iter_mode (network,a,forbid) in 
	    if A.empty_output output 
	    then None 
	    else 
	      begin
		let add j (net',flg)  =
		  let event = EventArray.find j net.Network.events in
		  let modifs = event.Network.nodes in  
		    match event.Network.kind with
			0 -> add_intro (event.Network.label,modifs) true net' ,flg  
		      | 1 -> re_add net' (event.Network.r,modifs) true ,flg 
		      | _ -> re_add net' (event.Network.r,modifs) true ,event.Network.r.Rule.flag
		in 
		let add' event (net',flg) = 
		  let modifs = event.Network.nodes in  
		    match event.Network.kind with
			0 -> add_intro (event.Network.label,modifs) true net' ,flg  
		      | 1 -> re_add net' (event.Network.r,modifs) true ,flg 
		      | _ -> re_add net' (event.Network.r,modifs) true ,event.Network.r.Rule.flag
		in 
		let net,flag = 
		  A.fold_first_output add (Network.empty (),None) output in 
		let net = opt_extract
		  (Network.cut net 
		     (Mods2.IntSet.empty,match flag with 
			  Some f -> f 
			| None ->  
			    let s = "Simulation.iter: obs has no flag" in
			      runtime 
				(Some "story_compressor.ml",
				 Some 1463,
				 Some s)
				s) )
		in 
		let net = 
		  if !Data.reorder_by_depth 
		  then 
		    let event_by_depth = 
		      EventArray.fold 
			(fun i j sol -> 
			   let depth = j.Network.g_depth in 
			   let old = 
			     try 
			       IntMap.find depth sol 
			     with Not_found -> []
			   in 
			     IntMap.add depth (j::old) sol)
			net.Network.events
			IntMap.empty in
		    let net,flag = 
		      IntMap.fold
			(fun i l net ->
			   List.fold_left 
			     (fun net a -> add' a net)
			     net l)
			event_by_depth (Network.empty (),None) 
		    in
		      opt_extract
			(Network.cut net 
			   (Mods2.IntSet.empty,
			    match flag 
			    with Some f -> f 
			      | None ->  
				  let s = "Simulation.iter: obs has no flag" in
				    runtime 
				      (Some "story_compressor.ml",
				       Some 1496,
				       Some s)
				      s)
			)
		  else
		    net 
		in
		  Some net 
	      end
	end 
      with Error.Too_expensive -> (set_error "-Causal trace computation has been aborded (too expensive)";None)
	| Error.Not_handled_yet s -> (set_error "-Causal trace computation has been aborded (pattern not implemented yet)";None)
  else (Some net) 
