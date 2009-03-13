(* 06/09/2008 *)
(* Views database handler for ODE generation *)
(* Jerome Feret pour PlectiX *)
(* views.ml *)

open Data_structures 
open Pb_sig
open Tools 
open Annotated_contact_map 
open Ode_print_sig 
open Ode_print 
open Error_handler 

let explicit = false
let debug = false
let log_step = false
let memory = true

let print_debug = 
  if debug then print_string else (fun x -> ())

let error i = 
  unsafe_frozen None (Some "views.ml") None (Some ("line  "^(string_of_int i))) (fun () -> raise Exit)


type ('bdd,'var,'sb,'expr,'pb,'export,'restore) ode_handler = 
    {project:'bdd->('var -> bool) -> 'bdd;
     export_ae:'export;
     restore:'restore;
     b_of_var:'var->Pb_sig.b;
     var_of_b:Pb_sig.b->'var;
     print_sb:'sb;
       print_sb_latex:((string*string)-> (string*string) -> bool) ->'sb;
     fnd_of_bdd:'bdd -> ('var*bool) list list ;
     conj:'bdd->'bdd->'bdd; 
     atom_pos:'var->'bdd;
     atom_neg:'var->'bdd;
     expr_true:'bdd}
    
let print_sb ode_handler a b c = 
     List.iter (fun x -> ode_handler.print_sb a b (Some x)) c 
 
let print_sb_latex f ode_handler a b c = 
  List.iter (fun x -> ode_handler.print_sb_latex f a b (Some x)) c 
 
let print_log s = 
  if log_step then () else (print_string s;print_newline ())



 
type 'a views = 
    {agent:string;
     interface:Annotated_contact_map.template_piece;
     solid_pending_edges: String4Set.t;
     target:(string*string) String2Map.t; 
     valuation:('a*bool) list;
     valuation_map: bool BMap.t}

let build_view (b_of_var) (a,i,v) = 
  {agent=a;
    interface=i;
    valuation=v;
    target=String2Map.empty;
    solid_pending_edges=String4Set.empty;
    valuation_map=
    List.fold_left
      (fun map (a,b) -> BMap.add (b_of_var a) b map)
      BMap.empty
      v
  }

let agent_of_view a = a.agent
let interface_of_view a = a.interface 
let valuation_of_view a = a.valuation
let valuation_map_of_view a = a.valuation_map 
let pending_edges a = a.solid_pending_edges   

let destroy view = 
  valuation_of_view view,
     agent_of_view view,
     interface_of_view view,
     pending_edges view 


(*let create_view_hashtable ode_handler print  pb  = 
  let hash_tp_list,dump = 
    let n = ref 1 in
    let map = ref IntListMap.empty in
    let f x = 
      let x' = List.sort compare x in
      try (IntListMap.find x' (!map))
      with Not_found -> 
	let rep = (!n) in
	let _ = if debug then 
          let _ = print_log ("NEW FRAGMENT: "^(string_of_int (!n))) in
   	  let _ = List.iter 
	      (fun i -> print_log (string_of_int i))
	      x' in () in 
	let _ = n:= (!n)+1 in
	let _ = map:=IntListMap.add x' rep (!map) in
	rep
    in f,
    let dump fmap  = 
      let l = 
	IntListMap.fold
	  (fun l n list -> 
	    ((n,
	      let expr = 
		List.fold_left 
		  (fun expr xold -> 
		    let view = 
		      (try Arraymap.find xold fmap 
		      with Not_found -> 
			error 93) in
		    let agent = agent_of_view view in 
		    (StringMap.add 
		       agent
		       (List.fold_left 
			  (fun expr (y,z) ->
			    ode_handler.conj expr
			      ((if z then ode_handler.atom_pos else ode_handler.atom_neg) ( y)))
			  (ode_handler.atom_pos (ode_handler.var_of_b (H(agent,agent))))
			  (valuation_of_view view))
		       expr ))
		  StringMap.empty 
		  l in
	      expr)::list)) (!map) [] in
      let l = List.sort (fun (a,b) (c,d) -> compare a c) l in
(*      let _ = List.iter 
	(fun (n,expr) -> 
	  let _ = pprint_obs print (print_sb ode_handler)   n expr pb in () )
	  l 
      in *)
      () in dump 
  in
  hash_tp_list,dump *)

let translate_classes_into_views ode_handler subviews rep  = 
  (** this primitives computes the set of valuated subviews 
      In translate_classes_into_views ode_handler subviews rep, 
      ode_handler allows to make computations with boolean variables 
      subviews is the result of the reachability analysis (i.e. a map between agent type and subviews) 
      rep is the result of the independence flow analysis *) 
          
  StringMap.fold
    (fun a tp  sol  ->
      List.fold_left
	(fun sol sites -> 
	  let g x = 
	    match ode_handler.b_of_var x
	    with M((_,_,y),_) 
	    | B(_,_,y)
	    | AL((_,_,y),_)  -> StringSet.mem y sites.kept_sites
	    | _ -> false in
	  let l = 
	    try 
 	      let res = (ode_handler.project 
			   (ode_handler.conj 
			      (ode_handler.atom_pos (ode_handler.var_of_b (H(a,a)))) 
			      (ode_handler.restore (StringMap.find a subviews)
				 ))
			   g) in
	      let sb =  ode_handler.fnd_of_bdd res in
	      List.map (fun x -> build_view ode_handler.b_of_var (a,sites,x)) sb 
	    with Not_found -> [] in
	  l@sol)
	sol tp )
    rep.subviews
    [] 

type views_id = int 

type 'a view_data_structure = 
    {interface_map: ('a views)  Arraymap.t;
     (** a mapping between views_id and views description *)

     link_to_template: (views_id list) String4Map.t;
     (** a mapping between typed bonds and the ids of the views that can be plugged *)

     blist_to_template: views_id StringBListMap.t}
     (** a mapping between view description (as a ordered blist) and the corresponding view id *) 

let empty_structure v = 
  {interface_map=Arraymap.create v; 
   link_to_template=String4Map.empty;
   blist_to_template=StringBListMap.empty}

let dummy = () 

let gather ode_handler rep rep2 = (*GENERATE DATA STRUCTURES *)
  List.fold_left  
    (fun (data_structures,n) view  -> 
      let int,int2 = 
	List.fold_left 
	  (fun (sol,sol2) (b,bool) -> 
	    match ode_handler.b_of_var b,bool with 
	      AL((a,b,c),(d,e)),true 
	      when keep_this_link (a,c) (d,e) rep
	      -> 
		(String4Set.add ((a,c),(d,e)) sol,
		 String2Map.add (a,c) (d,e) sol2)
	    | _ -> sol,sol2) 
	  (String4Set.empty,
	   String2Map.empty)
	  (valuation_of_view view) in
      let view = {view with solid_pending_edges = int;
                            target=int2 } in 
      let interface_map = Arraymap.add n view data_structures.interface_map in 
      let link_to_template =
	    String4Set.fold
	      (fun x link_to_template -> 
		let old = 
		  try 
		    String4Map.find x link_to_template
		  with Not_found 
		    -> [] 
		in
		String4Map.add x (n::old) link_to_template)
	      int 
	  data_structures.link_to_template 
      in
      let blist_to_template = 
	StringBListMap.add (agent_of_view view,
			    (List.sort compare 
			       (List.map (fun (b,b2) -> 
				 (ode_handler.b_of_var b,b2)) (valuation_of_view view)))) 
	  n 
	  data_structures.blist_to_template in 
      
      {
      interface_map = interface_map;
      link_to_template = link_to_template;
      blist_to_template = blist_to_template},n+1
	)
	(match rep2 with 
	  t::q -> empty_structure t,1
	| _ -> empty_structure dummy,1)
	rep2 


let view_of_tp_i tp_i int_map = 
   (** give the view associated to a view id *)
  let view = 
    try Arraymap.find tp_i int_map with 
      Not_found -> error 195
  in view 

let bool_of_tp_i tp_i int_map = 
   (** give the boolean valuation associated to a view id *)
      (*compute the set of booleans and their valuation attached to a piece *)
  valuation_of_view (view_of_tp_i tp_i int_map)

let agent_of_tp_i tp_i int_map = 
   (** give the agent type of a given view id *)
  agent_of_view (view_of_tp_i tp_i int_map)
    
let compute_interface_tp_i tp_i int_map ode_handler result = 
   (** compute the set agents that can be attached to a piece by a solid line *)
  List.fold_left 
    (fun sol (b,bool) -> 
      match ode_handler.b_of_var b,bool with 
	AL((a,b,c),(d,e)),true -> 
	  if keep_this_link (a,c) (d,e) result 
	  then 
		d::sol
	  else sol
      |  _ -> sol)
    [] (bool_of_tp_i tp_i int_map) 



let compute_compatible_views_id blist restricted_blist bmap agent_id (specie_of_id,agent_to_int_to_nlist,view_of_tp_i,ode_handler) = 
  (* we compute the list of sites *)
  let site_list,_ = 
       List.fold_left
	 (fun (sol,black) (b,bool) -> 
	   match b with 
	     B(_,_,s) | AL((_,_,s),_) | M((_,_,s),_) -> 
	       if StringSet.mem s black then (sol,black) else
	       s::sol,StringSet.add s black
	   | L((a',_,s'),(b',_ ,s'')) -> 
	       let sol,black = 
		 if a'=agent_id then 
		   if StringSet.mem s' black then
		     (sol,black) 
		   else s'::sol,StringSet.add s' black  
		 else sol,black in 
	       let sol,black = 
		 if b'=agent_id then 
		   if StringSet.mem s''  black then
		     (sol,black) 
		   else s''::sol,StringSet.add s'' black 
		 else sol,black 
	       in 
	       sol,black
	   | _ -> sol,black )
	 ([],StringSet.empty) 
      restricted_blist in
  let site_list = List.sort compare site_list in
  let tp_list = 
    try 
      StringListMap.find 
	site_list
	(StringMap.find 
	   (specie_of_id agent_id)  
	   agent_to_int_to_nlist) 
    with Not_found -> 
      ((try (print_debug "ID:";
             print_debug agent_id;
	     print_debug "\n";
	     print_debug "TYPE:"; 
             print_debug (specie_of_id agent_id);
	     print_debug "\n";
	     print_debug "INTERFACE:";
	     List.iter 
	       (print_debug) 
	       site_list;
	     print_debug "\n")
	
      with _ -> ());
       [] (*error 1190*))
	
  in
  let (tp_list:views_id list) = 
    List.fold_left 
      (fun sol xx -> 
	let view = view_of_tp_i xx in
	let valuation  = valuation_of_view view in 
	let rec test_b s = 
	  match s with 
	    [] -> true 
	  | (b,bool)::q -> 
	      let b = ode_handler.b_of_var b in 
	      if 
		try 
		  (BMap.find b bmap)=bool
		with 
		  Not_found -> true 
	      then 
		test_b q
	      else false
	in
	let test = test_b valuation in
	if not test then sol 
	else 
	  (xx::sol))
      [] tp_list in
  tp_list 
    
