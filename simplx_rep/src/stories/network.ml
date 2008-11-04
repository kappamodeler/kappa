(**Implementation of non interleaving semantics of computation traces*)

open Mods2

type eid = int

type node = (eid*Rule.modif_type)

(*kind:= 0:intro 1:classic 2:obs*)
type event = {r:Rule.t;label:string;s_depth:int;g_depth:int ;kind:int;nodes:Rule.modif_type list PortMap.t} 

let empty_event = {r=Rule.empty;label="";s_depth=(-1); g_depth=(-1); kind=(-1);nodes=PortMap.empty}
let is_empty_event e = (e.s_depth = (-1)) 

(*e.nodes : wire_id -> modif_type (or Not_found) *) 

module Wire : 
sig
  type t 
  val empty : t
  val plug : string -> (int*string) -> node -> t -> t
  val rename_before_plugging  : int IntMap.t -> (int*string) -> node -> t -> int IntMap.t 
  val unsafe_plug : node -> t -> t 
  exception Empty
  val top_event : t -> node
  val backtrack :  t -> eid -> t 
  val last_mod : t -> eid
  val testing : t -> eid list
  val print : t -> unit
  val fold_left : ('a -> node -> 'a) -> 'a -> t -> 'a 
  val exists: (node -> bool) -> t -> bool
  val can_push_to_collapse: t -> eid -> eid -> bool
end
  =
struct
  type t = node list
  let exists = List.exists
  let fold_left f a b = List.fold_left f a (List.rev b)
  let empty = []

  let rec can_push_to_collapse w eid rm_id =
    match w with
	[] -> Error.runtime "Wire.push_to_collapse: empty wire"
      | (i,modif)::tl -> 
	  if i=rm_id then can_push_to_collapse tl eid rm_id
	  else 
	    if i=eid then true
	    else
	      if Rule.is_pure_test [modif] then can_push_to_collapse tl eid rm_id
	      else false

  let unsafe_plug node w = (*pluggin a new quark modif/test without consistency requirement -> strong compression *)
    node::w 
  
  let plug rule_str (i,s) node w = (*safe pluggin of a new quark modif/test*)
    if w = [] then [node] (*if wire is empty, plugging always succeeds*)
    else
      let old_state = (fun (x,y) -> y)(List.hd w) 
      and new_state = (fun (x,y) -> y) node 
      in
      let old_state = 
	let rec aux old_state = 
	  match old_state with Rule.Before_After (_,y) -> aux y
	    | _ -> old_state in
	  aux old_state in 
      let error () = 
	let err = (*if !Data.sanity_check then*) Error.runtime (*else Error.warning*) in
	let add_msg = Printf.sprintf "\nIn rule %s\n" rule_str in
	  err (
	    (string_of_port (i,s))^": "
	    ^"Consistency check failed, adding "
	    ^(Rule.string_of_modif_type new_state)
	    ^" when previous node was "
	    ^(Rule.string_of_modif_type old_state)
	    ^add_msg
	  )
      in
      let push (eid,modif) w = 
	match w with
	    [] -> [(eid,modif)]
	  | (eid',_)::tl -> if eid=eid' then (eid,modif)::tl else (eid,modif)::w
      in
      let rec aux new_state  = 
	match new_state with
	    Rule.Before_After (_,y) -> aux y 
	  | Rule.Test_bound (i,s) -> (
	      match old_state with
		  (Rule.Bound (i',s') | Rule.Init_bound (_,i',s') | Rule.Test_bound (i',s')) -> 
		    if (i'=i) && (s=s') then push node w else error() 
		      
	        | Rule.Test_any_bound -> push node w
		| _ -> error()
	    )
	  | Rule.Test_marked s -> (
	      match old_state with
		  (Rule.Test_marked s' | Rule.Marked (_,s') | Rule.Init_mark (_,s')) -> if s'=s then push node w else error()
		| _ -> error()
	    )
	  | Rule.Test_any_bound -> (
	      match old_state with
		  (Rule.Before_After _ |Rule.Init_bound _ | Rule.Bound _ | Rule.Test_bound _ | Rule.Test_any_bound) -> push node w 
		| _ -> error()
	    )
	  | Rule.Test_free -> (
	      match old_state with
		  (Rule.Before_After _ | Rule.Init_free _ | Rule.Test_free | Rule.Break _ | Rule.Side_break _) -> push node w 
		| _ -> error()
	    )
	  | Rule.Bound (i,s) -> (
	      match old_state with 
		| Rule.Init_free _ | Rule.Break _ | Rule.Test_free | Rule.Side_break _ -> push node w
		| _ -> error()
	    ) 
	  | Rule.Break (i,s) -> 
	      (match old_state with Rule.Before_After _ -> push node w 
		 |	_ -> 
			  if (old_state = Rule.Bound(i,s)) or (old_state = Rule.Test_bound(i,s)) or (old_state = Rule.Test_any_bound)
			    or (match old_state with Rule.Init_bound (_,i',s') when i=i' && s=s' -> true | _ -> false) 
			  then push node w else error())
	  | Rule.Marked (old_m,_) -> (
	      match old_state with
		  (Rule.Marked (_,s') | Rule.Init_mark (_,s') | Rule.Test_marked s') -> 
		    if (old_m = s') then push node w else error()
		| _ -> error()
	    )
	  | Rule.Remove -> push node w (*remove action is always compatible*)
	  | Rule.Side_break (i,s) -> 
	      if (old_state = Rule.Bound(i,s))
		or (match old_state with Rule.Init_bound(_,i',s') when i=i' && s=s' -> true | _ -> false)
		or (old_state = Rule.Test_bound(i,s)) 
		or (old_state = Rule.Test_any_bound)
	      then push node w else error()
	  | Rule.Init_bound _ -> 
	      begin
		match old_state with 
		    Rule.Init_free _ -> push node w
		  | _ -> Error.runtime "Network.plug: invalid sequence of modif on a single quark"
	      end
	  | Rule.Init_mark _ | Rule.Init_free _ -> 
	      Error.runtime "Network.plug: adding an initial state to a non empty wire." in 
	aux new_state
	
  let rec rename_before_plugging sigma (i,s) node w = 
    let codomain = 
      IntMap.fold 
	(fun _ -> IntSet.add) sigma IntSet.empty in
    let f x = try IntMap.find x sigma with Not_found -> 
      if IntSet.mem x codomain then x else x in
    let i' = f i in
    let node' = (fst node,Rule.subs_act sigma (snd node)) in
      (*safe pluggin of a new quark modif/test*)
    if w = [] then raise Exit (*if wire is empty, plugging always fails*)
    else
      let old_state = (fun (x,y) -> y)(List.hd w) 
      and new_state = (fun (x,y) -> y) node' 
      in
      let old_state = 
	let rec aux old_state = 
	  match old_state with Rule.Before_After (_,y) -> aux y
	  | _ -> old_state in
	  aux old_state in
      let old_state = Rule.subs_act sigma old_state in
      let new_state = Rule.subs_act sigma new_state in 
      let error () = let s (*Error.runtime*) 
	= (string_of_port (i',s))^": "
	^"Consistency check failed, adding "
	^(Rule.string_of_modif_type new_state)
	^" when previous node was "
	^(Rule.string_of_modif_type old_state)
      in Error.runtime s
      
      in
      let unify i i' sigma = 
	try (IntMap.find i sigma;None) 
	with Not_found -> 
	  Some (IntMap.add i i' sigma)
      in 
      let rec aux new_state  = 
	match new_state with
	    Rule.Before_After (_,y) -> aux y 
	  | Rule.Test_bound (i,s) -> (
	      match old_state with
		  (Rule.Bound (i',s') | Rule.Init_bound (_,i',s') | Rule.Test_bound (i',s')) -> 
		    if (i'=i) && (s=s') then sigma  else 
		      begin
			if s=s' then 
			  match unify i i' sigma with 
			      None -> error ()
			    |Some sigma -> sigma 
			else 
			  error()
		      end
		| Rule.Test_any_bound -> sigma 
		| _ -> error()
	    )
	  | Rule.Test_marked s -> (
	      match old_state with
		  (Rule.Test_marked s' | Rule.Marked (_,s') | Rule.Init_mark (_,s')) -> if s'=s then sigma  else error()
	      | _ -> error()
	    )
	  | Rule.Test_any_bound -> (
	      match old_state with
		  (Rule.Before_After _ |Rule.Init_bound _ | Rule.Bound _ | Rule.Test_bound _ | Rule.Test_any_bound) -> sigma 
		| _ -> error()
	    )
	  | Rule.Test_free -> (
	      match old_state with
		  (Rule.Before_After _ | Rule.Init_free _ | Rule.Test_free | Rule.Break _ | Rule.Side_break _) -> sigma  
		| _ -> error()
	    )
	  | Rule.Bound (i,s) -> (
	      match old_state with 
	      | Rule.Init_free _ | Rule.Break _ | Rule.Test_free | Rule.Side_break _ -> sigma 
		| _ -> error()
	    ) 
	  | Rule.Break (i,s) -> 
	      (match old_state with Rule.Before_After _ -> sigma  
	      |	_ -> 
		   
		  (match old_state with Rule.Bound(i',s') 
		      | Rule.Test_bound(i',s') 
		      |Rule.Init_bound (_,i',s') -> 
			 if  i=i' && s=s' then  sigma
			 else if s=s' then 
			   match unify i i' sigma 
			   with Some sigma' -> sigma'
			     | None -> error ()
			 else error ()
		      | _ -> error ()
		  ))
	  | Rule.Marked (old_m,_) -> 
	      (
	      match old_state with
		  (Rule.Marked (_,s') | Rule.Init_mark (_,s') | Rule.Test_marked s') -> 
		    if (old_m = s') then sigma  else error() 
		| _ -> error()
	      )
	  | Rule.Remove -> sigma  (*remove action is always compatible*)
	  | Rule.Side_break (i,s) -> 
	      (match old_state 
	       with 
		   Rule.Bound(i',s')
		 |Rule.Init_bound(_,i',s') 
		 | Rule.Test_bound(i',s') -> if i=i' && s=s' then sigma  else if s = s' then 
		     match unify i i' sigma with 
			 None -> error ()
		       |Some sigma -> sigma
		   else error () 
		 |
		     Rule.Test_any_bound -> sigma 
		 | _ -> error ()) 
		
	  | (Rule.Init_bound _|Rule.Init_mark _|Rule.Init_free _) -> 
	      Error.runtime "Network.plug: adding an initial state to a non empty wire." in 
	aux new_state

  exception Empty
    (*backtracks last modif of wire *)
  let rec backtrack w eid = 
    match w with
	(eid',state)::tl -> 
	  if (eid = eid') then tl 
	  else
	    if (Rule.is_pure_test [state]) then (eid',state)::(backtrack tl eid) (*sanity check*)
	    else
	      Error.runtime "Network.Wire.backtrack: sanity check failed"
      | [] -> raise Empty

  let rec last_mod w = 
    match w with
	(eid,state)::tl  -> if Rule.contains_modif [state] then eid else (last_mod tl)
      | [] -> raise Not_found

  let rec testing w =
    match w with
	(eid,state)::tl -> if Rule.is_pure_test [state] then (eid::(testing tl)) else []
      | [] -> []

  let top_event w =
    match w with
	(eid,state)::_  -> (eid,state)
      | [] -> raise Empty

  let rec print w = 
    match w with
	(eid,state)::tl  -> Printf.printf "%d--(%s)\n" eid (Rule.string_of_modif_type state); print tl
      | [] -> ()
end

module EventArray = Array_ext.Make(struct type t = event let default = empty_event end)

(*Replace wires by extensible array*)
type t = {fresh_id:eid; (*new event identifier*)
	  name_of_agent:string array option;
	  events: int EventArray.t ;
	  s_preds:IntSet.t IntMap.t;  (*preds: eid -> {eids} -- set of identifiers that precede pid*)
	  w_preds: IntSet.t IntMap.t; (*weak predecessors of eid*)
	  wires:Wire.t PortMap.t; (*wires: (i,s) -> Wire.t = [Test;Test;...;Bound (i,s);....]*)
	  last: IntSet.t ;
	 } 

let strong_preds eid net = try IntMap.find eid net.s_preds with Not_found -> IntSet.empty
let weak_preds eid net = try IntMap.find eid net.w_preds with Not_found -> IntSet.empty

type marshalized_t = {f_fresh_id:eid; (*new event identifier*)
		      f_name_of_agent:string list ;
		      f_events: (int*event) list ;
		      f_s_preds:IntSet.t IntMap.t;  (*preds: eid -> {eids} -- set of identifiers that precede pid*)
		      f_w_preds: IntSet.t IntMap.t ;
		      f_wires:Wire.t PortMap.t; (*wires: (i,s) -> Wire.t = [Test;Test;...;Bound (i,s);....]*)
		      f_last: IntSet.t ;
		     }

let marshal net =
  {f_fresh_id = net.fresh_id ;
   f_name_of_agent = 
      begin
	match net.name_of_agent with
	    None -> []
	  | Some n_of_a -> Array.to_list n_of_a
      end; 
   f_events = EventArray.fold (fun i e cont -> (i,e)::cont) net.events [] ;
   f_s_preds = net.s_preds ;
   f_w_preds = net.w_preds ;
   f_wires = net.wires ; 
   f_last = net.last
  }

let unmarshal f_net =
  {fresh_id = f_net.f_fresh_id ;
   name_of_agent = 
      begin
	match f_net.f_name_of_agent with
	    [] -> None
	  | n_of_a -> Some (Array.of_list n_of_a)
      end; 
   events = List.fold_left (fun ar (i,e) -> let ar = EventArray.add i e ar in ar) (EventArray.create 1) f_net.f_events ;
   s_preds = f_net.f_s_preds ;
   w_preds = f_net.f_w_preds ;
   wires = f_net.f_wires ; 
   last = f_net.f_last
  }

let empty() = 
  {fresh_id=0; 
   events = EventArray.create 100 ;
   wires = PortMap.empty;
   s_preds = IntMap.empty ;  
   w_preds = IntMap.empty ;
   last = IntSet.empty;
   name_of_agent = None;
  } 

let is_empty net = EventArray.is_empty net.events

let copy net = 
  let name_of_agent'= 
    match net.name_of_agent with
	None -> None
      | Some n_of_a -> Some (Array.copy n_of_a)
  in
    {net with events = EventArray.copy net.events ; name_of_agent = name_of_agent'}

let event_of_id i net =
  try 
    (*IntMap.find i net.events *)
    EventArray.find i net.events
  with 
      Not_found -> 
	let error = Printf.sprintf "event_of_id: %d does not correspond to any event" i 
	in Error.runtime error
 
let unsafe_add_wire (i,s) node (wires:Wire.t PortMap.t) = 
  let w = try PortMap.find (i,s) wires with Not_found -> Wire.empty in
    PortMap.add (i,s) (Wire.unsafe_plug node w) wires
    

let add_wire str (i,s) node (wires:Wire.t PortMap.t) = 
  let w = try PortMap.find (i,s) wires with Not_found -> Wire.empty in
    PortMap.add (i,s) (Wire.plug str (i,s) node w) wires
 
let rename_before_adding_wire  sigma  (i,s) node (wires:Wire.t PortMap.t) = 
  let w = try PortMap.find 
    ((try IntMap.find i sigma with Not_found -> i),s) wires with Not_found -> Wire.empty in
  let sigma  = Wire.rename_before_plugging  sigma (i,s) node w in 
  sigma
 

let add_intro (str,modifs) safe net =
  let add_wire = if safe then add_wire str else unsafe_add_wire in 
  let eid = net.fresh_id in
  let e = {r=Rule.empty;label=str;s_depth=0;g_depth=0;kind=0;nodes=PortMap.empty} 
  in
  let net =
    PortMap.fold (fun (i,s) state net ->
		    let node = (eid,List.hd state) in (*no need to add sequence of modifications since there is no previous event*)
		    let e = 
		      try EventArray.find eid net.events with Not_found -> e 
		    in 
		    let e = {e with nodes = PortMap.add (i,s) state e.nodes}
		    in
		    let wires = add_wire  
			(i,s) 
			node 
			net.wires 
		    in
		      {net with 
			 events = EventArray.add eid e net.events ; 
			 wires = wires; 
			 last = IntSet.add eid net.last}
		 ) modifs net 
  in
    {net with fresh_id = net.fresh_id+1}

let preds_closure net closure = 
  let rec f net closure preds = 
    let news =
      IntSet.fold (fun i set -> 
		     let p_i = strong_preds i net in  
		       IntSet.fold (fun i set -> 
				      if IntSet.mem i closure then set 
				      else IntSet.add i set
				   ) p_i set
		  ) preds IntSet.empty
    in
      if IntSet.is_empty news then closure
      else f net (IntSet.union preds (IntSet.union news closure)) news
  in
    f net closure closure
      
(*returns immediate predecessors of eid, whether weak or strong, which are maximal if eid is removed*)
let immediate_preds eid net =
  let is_top_after_removal rm_id kept_id net =
    try
      PortMap.fold (fun (i,s) modif _ -> 
		      let w_is = PortMap.find (i,s) net.wires in
			if Wire.can_push_to_collapse w_is eid rm_id then ()
			else raise False
		   ) (event_of_id kept_id net).nodes () ; true
    with
	False -> false
      | _ -> Error.runtime "Network.immediate_preds: event or wire not found in network"
  in
  let weak = weak_preds eid net
  and strong = strong_preds eid net
  in
  let prob_preds = IntSet.union weak strong in
  let set,_ =
    IntSet.fold (fun j (set,blacklist) -> 
		   let weak_j = weak_preds j net 
		   and strong_j = strong_preds j net 
		   in
		   let set = IntSet.fold (fun i set -> IntSet.remove i set) weak_j set in
		   let set = IntSet.fold (fun i set -> IntSet.remove i set) strong_j set in
		   let blacklist = IntSet.union weak_j (IntSet.union strong_j blacklist) in
		     if IntSet.mem j blacklist then (set,blacklist) else 
		       if is_top_after_removal eid j net then (IntSet.add j set,blacklist)
		       else (set,IntSet.add j blacklist)
		) prob_preds (IntSet.empty,IntSet.empty)
  in
    set    
      
(**Adding the new event [eid] and updating precedence relation if necessary*)
let add_event ?(replay=false) eid (r,modifs) add_wire net =
  let net = 
    PortMap.fold (fun (i,s) modif_list net -> 
		    List.fold_right (fun state net ->
				       if Rule.contains_test [state] then (*adding a test node*)
					 let preds_eid,weak_preds = (*predecessors are modifications of wires of eid*)
					   let w_is = try PortMap.find (i,s) net.wires with Not_found -> Wire.empty in
					   let l_test = if Rule.contains_modif [state] then Wire.testing w_is else [] in
					   let l_mod = try [Wire.last_mod w_is] with Not_found -> [] (*event modif. (i,s)*)
					   and old_strong = strong_preds eid net
					   and old_weak = weak_preds eid net
					   in
					   let strong,weak = List.fold_right (fun j (strong,weak) -> 
										if j=eid then (strong,weak) (*if event has multiple modifs*)
										else 
										  let weak = IntSet.remove j weak
										  and strong = IntSet.add j strong
										  in
										    (strong,weak)
									     ) l_mod (old_strong,old_weak)
					   in
					   let weak = 
					     List.fold_right (fun j set -> 
								if (j=eid) or (IntSet.mem j strong) then set 
								else 
								  IntSet.add j set
							     ) l_test weak
					   in
					     (strong,weak)
					 in
					 let wires = add_wire (i,s) (eid,state) net.wires
					 in
					   {net with
					      s_preds = IntMap.add eid preds_eid net.s_preds;
					      w_preds = IntMap.add eid weak_preds net.w_preds;
					      wires = wires
					   }
				       else (*Pure modif of a quark does not genereate precedence but may generate non permutation*)
					 let weak_preds = 
					   let w_is = try PortMap.find (i,s) net.wires with Not_found -> Wire.empty in
					   let l_test = Wire.testing w_is in
					   let old_strong = strong_preds eid net in
					   let old_weak = weak_preds eid net in
					     List.fold_right (fun i set -> 
								if i=eid or IntSet.mem i old_strong then set 
								else IntSet.add i set
							     ) l_test old_weak
					 in
					 let wires = add_wire (i,s) (eid,state) net.wires in
					   {net with
					      w_preds = IntMap.add eid weak_preds net.w_preds;
					      wires = wires
					   }
				    ) modif_list net
		 ) modifs net
  in
  let strong = strong_preds eid net in
  let weak = weak_preds eid net in
  let _ = 
    if !Data.sanity_check then 
      let set = IntSet.inter weak strong in 
	if IntSet.is_empty set then () 
	else Error.runtime "QA failed in Network.event_add: weak and strong precedence should have an emtpy intersection"
    else () 
  in
  let s_d,g_d,last = 
    IntSet.fold (fun i (s_mx,g_mx,last) -> 
		   try
		     let last = IntSet.remove i last in
		     let e = EventArray.find i net.events 
		     in 
		     let s_mx = if (e.s_depth > s_mx) && (IntSet.mem i strong) then e.s_depth else s_mx
		     and g_mx = if (e.g_depth > g_mx) then e.g_depth else g_mx
		     in
		       (s_mx,g_mx,last) (*weak preds does not change depth of events*)
		   with Not_found -> 
		     Error.runtime 
		       ("Network.add: event "^(string_of_int i)^" not found in predecessors of "^(string_of_int eid))
		) (IntSet.union strong weak) (0,0,net.last)
  in
  let e = {r=r;
	   label=PortMap.fold (fun (i,s) _ label -> 
				 if (s="_!" or s="_~") then label else
				   Printf.sprintf "<%d,%s>%s" i s label
			      ) modifs (Printf.sprintf "\n%s" r.Rule.input) ;
	   s_depth=s_d+1;
	   g_depth=g_d+1;
	   kind=1;
	   nodes = modifs 
	  } 
  in
    {net with 
       events = EventArray.add eid e net.events ;
       fresh_id = net.fresh_id+1;
       last = IntSet.add eid last
    }
  
let add sol net (r,modifs) debug compress =
  try
    (*raise (Rule.Opposite e.nodes) in case of success*)
    let _ =
      if compress then
	let candidates =
	  PortMap.fold (fun (i,s) states candidates -> 
			  if not (Rule.contains_modif states) or (Rule.is_creation states) or (Rule.is_deletion states) 
			  then candidates (*if List.hd contains modif than so other actions in List.tl*)
			  else
			    let w_is = try PortMap.find (i,s) net.wires with Not_found -> Error.runtime (Printf.sprintf "Network.add: Wire (%d,%s) not found" i s)
			    in 
			    let opt_eid_state = (try Some (Wire.top_event w_is) with Wire.Empty -> None) in
			      match opt_eid_state with
				  Some (eid',state') -> 
				    if not (IntSet.mem eid' net.last) then candidates else
				      let e = event_of_id eid' net in
					if e.kind=0 then candidates 
					else
					  if Rule.contains_modif [state'] then IntSet.add eid' candidates
					  else IntSet.empty (*a pure test is preventing trivial compression*)
				| None -> candidates
		       ) modifs IntSet.empty
	in
	  try
	    IntSet.iter (fun eid -> 
			   let e = EventArray.find eid net.events 
			   in 
			     (*Printf.printf "%s\n%s\n" e.label r.Rule.input; flush stdout ; *)
			     Rule.opposite modifs e.nodes sol
			) candidates
	  with Not_found -> Error.runtime "Network.add: event not found"
      else ()
    in
    let eid = net.fresh_id in
      (try
	 add_event eid (r,modifs) (add_wire (Printf.sprintf "%s_%d" r.Rule.input eid)) net (*If there is no trivial compression*)
       with Not_found -> Error.runtime "Network.add_event: not found raised")
  with
      Rule.Opposite nodes -> (**When added event can collapse nodes in [nodes]*)
	begin
	  let opt = PortMap.fold (fun (i,s) states opt ->
				    if not (Rule.contains_modif states) then opt 
				    else
				      let eid,_ = Wire.top_event (PortMap.find (i,s) net.wires) in
					match opt with 
					    Some eid' -> 
					      if eid=eid' then Some eid
					      else
						begin
						  let msg = 
						    Printf.sprintf 
						      "Network.add: invariant violation, cannot collapse \n%s\n%s\n with" 
						      (PortMap.fold (fun (i,s) _ label -> 
								       if (s="_!" or s="_~") then label else
									 Printf.sprintf "<%d,%s>%s" i s label
								    ) modifs "")
						      r.Rule.input
						  and msg2 = 
						    let event = event_of_id eid net 
						    and event'= event_of_id eid' net
						    in
						      Printf.sprintf "%d:%s\n%d:%s\n" eid event.label eid' event'.label
						  in
						    Printf.printf "last:%s\n" (string_of_set string_of_int IntSet.fold net.last) ; flush stdout ;
						    Error.runtime (msg^"\n"^msg2) 
						end
					  | None -> Some eid
				 ) nodes None
	  in
	  let rm_eid = match opt with None -> Error.runtime "Network.add: invalid argument" | Some eid -> eid in
	  let _ = if IntSet.mem rm_eid net.last then () else Error.runtime "Network.add: removed event is not maximal" in
	  let preds_rm = immediate_preds rm_eid net in (*try IntMap.find rm_eid net.preds with Not_found -> IntSet.empty in*)
	  let events,preds,w_preds =
	    ( 
	      EventArray.remove rm_eid net.events,  (*removing rm_eid from network*)
	      IntMap.remove rm_eid net.s_preds, (*removing information about preds of rm_eid which is now useless*)
	      IntMap.remove rm_eid net.w_preds
	    )
	  in
	  let _ =
	    if !Data.sanity_check then
	      (
		IntMap.fold (fun eid set _ -> 
			       if IntSet.mem rm_eid set then 
				 Error.runtime (Printf.sprintf "QA failed in Network.add: removed eid is a weak predecessor of event %d" eid) 
			       else ()
			    ) net.w_preds () ;
		IntMap.fold (fun eid set _ -> 
			       if IntSet.mem rm_eid set then 
				 Error.runtime (Printf.sprintf "QA failed in Network.add: removed eid is a strong predecessor of event %d" eid) 
			       else ()
			    ) net.s_preds ()
	      )
	  in
	  let net = 
	    PortMap.fold (fun (i,s) _ net -> 
			    let wires =
			      try
				let w_is = PortMap.find (i,s) net.wires in
				let w_is'= Wire.backtrack w_is rm_eid in
				  PortMap.add (i,s) w_is' net.wires
			      with 
				  Wire.Empty -> 
				    let msg = Printf.sprintf "Wire (%d,%s) is empty" i s in
				      Error.runtime msg
				| Not_found -> 
				    let msg=Printf.sprintf "Wire (%d,%s) not found" i s in
				      Error.runtime msg
			    in
			      {net with wires=wires}
			 ) nodes {net with events=events;s_preds=preds; w_preds=w_preds ; fresh_id=net.fresh_id+1}
	  in
	  let last =
	    IntSet.fold (fun eid last -> IntSet.add eid last) preds_rm (IntSet.remove rm_eid net.last)
	  in
	    {net with last = last}
	end
    | Not_found -> Error.runtime "Network.add: not found raised"

let re_add net (r,modifs) safe  =
  let add_wire = if safe then (add_wire r.Rule.input) else unsafe_add_wire in 
  let eid = net.fresh_id in
    add_event ~replay:true eid (r,modifs) add_wire net

let re_add_rename sigma  net (r,modifs) safe  =
  let add_wire = if safe then (add_wire r.Rule.input) else unsafe_add_wire in 
  let eid = net.fresh_id in
  let arg = 
    PortMap.fold 
      (fun ((i,_)) modif_list sol -> 
	 let rec aux a sol =  
	   match a with 
	       Rule.Bound (j,_)  
	     | Rule.Test_bound (j,_) 
	     | Rule.Side_break (j,_) 
	     | Rule.Break (j,_) 
	     | Rule.Init_bound (_,j,_) -> (IntSet.add j sol)
	     | Rule.Test_marked _ 
	     | Rule.Test_any_bound | Rule.Test_free | Rule.Remove | Rule.Marked _ | Rule.Init_mark _  | Rule.Init_free _  -> sol
	     | Rule.Before_After (x,y) -> aux x (aux y sol)
	 in 
	   List.fold_left 
	     (fun sol a -> aux a sol)
	     (IntSet.add i sol) modif_list)
      modifs IntSet.empty 
  in 
  let unify_port (i,s) state sigma = 
    (*if Rule.is_pure_test state then (*adding a test node*)*)
    let sigma  = rename_before_adding_wire sigma (i,s) (eid,state) net.wires
    in
      sigma 
	(*else (*adding a modification node*)
	  let sigma  = rename_before_adding_wire sigma (i,s) (eid,state) net.wires
	  in
	  sigma 
	*)
  in
    try 
      begin
	let sigma = 
	  PortMap.fold 
	    (fun (i,s) modif_list sigma -> 
	       let state = List.hd (List.rev modif_list) in 
		 try (let _ = IntMap.find i sigma in 
			unify_port (i,s) state sigma )
		 with Not_found -> sigma)
	    modifs sigma 
	in 
	let f x = try (IntMap.find x sigma) with Not_found -> x in 
	let _ = 
	  IntSet.fold
	    (fun i set -> let j = f i in
	       if IntSet.mem j set then raise Exit
	       else IntSet.add j set)
	    arg IntSet.empty in
	let net = add_event ~replay:true eid (r,modifs) add_wire net in
	  (net,sigma,true)
      end
    with _ -> (net,sigma,false)  
    		 
let cut net (*port_obs*) obs_str =
  let ids = 
    IntSet.singleton (net.fresh_id-1) (*if flagged rule then obs is simply the last event*)
  in
  let preds_star =  preds_closure net ids
  in
  let h =
    IntSet.fold (fun i h ->
		   let e = 
		     try EventArray.find i net.events with Not_found -> Error.runtime "Network.cut: event not bound"
		   in 
		     {h with 
			events = EventArray.add i e h.events ;
			s_preds = IntMap.add i (strong_preds i net) h.s_preds ;  
			w_preds = 
			 begin
			   let w_ids = weak_preds i net in
			      (*only adding weak arrows that are internal to the story*)
			   let w_ids = 
			     IntSet.fold (fun eid set ->
					    if IntSet.mem eid preds_star then
					      IntSet.add eid set
					    else set
					 ) w_ids IntSet.empty
			   in
			     IntMap.add i w_ids h.w_preds
			 end;
			fresh_id = if (i >= h.fresh_id) then (i+1) else h.fresh_id ;
			last = if IntSet.mem i net.last then IntSet.add i h.last else h.last ;
		     }
		) preds_star {(empty()) with wires = net.wires}
  in
    (*if port_obs = [] then *)
  let id_obs = 
    try IntSet.choose h.last with Not_found -> Error.runtime "Network.cut: empty story"
  in (*should be only one*)
  let e_obs = (*IntMap.find id_obs h.events *) 
    try EventArray.find id_obs h.events with Not_found -> Error.runtime "Network.cut: empty story"
  in
    {h with events = EventArray.add id_obs {e_obs with kind=2} h.events}
      

let obs_story h = 
  let rec find_obs last =
    if IntSet.is_empty last then Error.runtime "No observable in story"
    else
      let i = IntSet.choose last in
      let e = (*IntMap.find i h.events*) EventArray.find i h.events in
	if e.kind = 2 then 
	  match Rule.flag e.r with
	      None -> e.label
	    | Some flg -> flg
	else
	  Error.runtime "No observable in story" (*find_obs (IntSet.remove i last)*)
  in
    find_obs h.last
	

let weigth net = 
  if !Data.reorder_by_depth then
    let map = 
      EventArray.fold
	(fun i j map -> 
	  let depth = j.g_depth in
	  let old = 
	    try (IntMap.find depth map)
	    with Not_found -> 0 in
	  IntMap.add depth (old+1) map)
	net.events 
	IntMap.empty in
    IntMap.fold (fun depth n sol -> if n=0 then sol else (depth,n)::sol)
      map []
  else
    [0,net.fresh_id]

let rec compare_net w w' =
  match w,w'
  with 
    [],[] -> 0
  | [],_  -> -1
  | _,[]  -> 1
  | (a,b)::q,(a',b')::q' ->
      if a<a' then -1
      else if a>a' then 1
      else if b<b' then -1 
      else if b>b' then 1
      else compare_net q q'
