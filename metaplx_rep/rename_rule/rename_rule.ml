open Data_structures_metaplx
open Rename_agent 


let add_decl a l = a::l 

let declare_full agent interface_database i =
  let interface = 
    List.fold_left 
      (fun set (a,_) -> SiteSet.add a set)
      SiteSet.empty
      agent.interface 
  in 
  try 
    let (int,line) = 
      AgentMap.find agent.agent_name interface_database.concrete_names
    in 
    match int with None -> raise Not_found 
    | Some int -> 
	if SiteSet.equal int interface 
	then 
	  Some  {interface_database 
		with 
		  concrete_names = 
		  AgentMap.add 
		    agent.agent_name 
		    (Some interface,add_decl i  line) 
		    interface_database.concrete_names}
	else 
	  failwith 
	    (List.fold_left 
	       (fun string i -> string^(string_of_int i)^";")
	       ("Incombatible interfaces between line "^(string_of_line i))
	       line)
  with 
    Not_found -> 
      Some {interface_database 
	       with 
	     concrete_names = AgentMap.add agent.agent_name (Some interface,add_decl i []) interface_database.concrete_names}

let check_rule rule interface_database i = 
  let f = 
    List.fold_left 
      (fun database ag -> 
	match declare_full ag database i
	with None -> failwith ("Problem with rule "^rule.flag^" at line "^(string_of_int i))
	| Some a -> a)
  in
  let g1 = 
    List.fold_left 
      (fun database ag -> 
	let set = AgentSet.add ag.agent_name database.agents in 
	if AgentSet.equal set database.agents 
	then 
	  database
	else
	  {database with agents = set}
	  )
  in 
  let g2 = 
    List.fold_left 
      (fun database ag -> 
	let set = AgentSet.add ag.agent_name database.agents in 
	if AgentSet.equal set database.agents 
	then 
	  database
	else
	  {database with agents = set}
	  )
  in 
  g2 
    (g2 
       (g1 
	  (f 
	     (f 
		interface_database 
		rule.fixed_left_hand_side)
	     rule.fixed_right_hand_side)
	  rule.hand_side_common)
       rule.mod_left_hand_side)
    rule.mod_right_hand_side 
    

let rename_rule rule interface_database flagmap  = 
  let fadd x y map = 
    let old = 
      try 
	StringMap.find x map 
      with 
	Not_found -> [] in 
    StringMap.add x (y::old) map in 
  let sol = {rule 
	    with hand_side_common = [] ; 
	      mod_left_hand_side = [] ;
	      mod_right_hand_side = []} in
  let rename l = 
    List.fold_left 
      (fun prefix_list a -> 
	let sol = rename_agent a interface_database in 
	List.fold_left 
	  (fun a (kappa_prefix,tag_prefix) -> 
	    List.fold_left 
	      (fun a (tag_suffix,kappa_suffix)  -> 
		(kappa_suffix::kappa_prefix,tag_suffix::tag_prefix)::a)
	      a sol)
	  [] prefix_list) 
      [[],[]] l 
  in 
  let hand_side_common' = rename rule.hand_side_common in 
  let mod_left_hand_side' = rename rule.mod_left_hand_side in 
  let mod_right_hand_side' = rename rule.mod_right_hand_side in 
  let rule_list = 
    List.fold_left 
      (fun liste (hs,hs') -> 
	List.fold_left 
	  (fun liste (left,left') -> 
	    List.fold_left 
	      (fun (liste,flags) (right,right') -> 
		let add_flag = 
		  List.fold_left 
		    (List.fold_left (fun flag (a,b)-> flag^"."^b^"/"^a))
		in 
		let flag' = add_flag (add_flag (add_flag rule.flag hs') left') right' in 
		{rule with 
		  flag = flag' ;
		  hand_side_common = hs ; 
		  mod_left_hand_side = left ;
		  mod_right_hand_side = right }::liste,
		fadd rule.flag flag' flags)
	      liste 
	      mod_right_hand_side')
	  liste 
	  mod_left_hand_side')
      ([],flagmap) 
      hand_side_common' 
  in 
  rule_list 

let print_agent_list log l bool = 
  List.fold_left 
    (fun bool agent -> 
      let _ = if bool then Printf.fprintf log " , " in
      let _ = Pretty_printing.print_agent log agent in
      true)
    bool l 


let check_model line (interface_database:declaration) = 
  match line with 
    INIT_L _   
  | OBS_L _
  | STORY_L _ 
  | DONT_CARE_L _ 
  | GEN_L _ 
  | CONC_L _ -> interface_database
  | RULE_L _ -> failwith "INTERNAL ERROR"
  | PREPROCESSED_RULE (_,y,i) -> check_rule y interface_database i 

let transform_model line interface_database (tail,flagset) = 
  match line with 
    INIT_L _  
  | OBS_L _ 
  | STORY_L _ 
  | DONT_CARE_L _ 
  | GEN_L _ 
  | CONC_L _ -> line::tail,flagset
  | PREPROCESSED_RULE (x, rule,i) -> 
      let a,b = rename_rule rule interface_database flagset in 
      List.fold_left 
	(fun sol l -> PREPROCESSED_RULE (x,l,i)::sol)
	tail 
	(a),b
  | RULE_L _  -> failwith "INTERNAL ERROR"

let rename_obs rule flagset list = 
  match rule with 
    INIT_L _ | DONT_CARE_L _ | GEN_L _ | CONC_L _ | RULE_L _ | PREPROCESSED_RULE _ -> rule::list
  | OBS_L (s,a,i) ->
      (try 
	let l = StringMap.find s flagset in
	List.fold_left 
	  (fun sol l -> (OBS_L(l,a,i))::sol)
	  list l 
      with Not_found -> (rule::list))
  | STORY_L (s,a,i) ->  
      (try 
	let l = StringMap.find s flagset in
	List.fold_left 
	  (fun sol l -> STORY_L(l,a,i)::sol)
	  list l 
      with Not_found -> rule::list)
