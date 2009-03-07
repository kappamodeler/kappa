open Data_structures_metaplx
open Rename_agent 


let add_decl a l = a::l 

let declare_full flag agent interface_database =
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
		    (Some interface,add_decl flag line) 
		    interface_database.concrete_names}
	else 
	  failwith "Incombatible interfaces"
  with 
    Not_found -> 
      Some {interface_database 
	       with 
	     concrete_names = AgentMap.add agent.agent_name (Some interface,add_decl flag []) interface_database.concrete_names}

let check_rule rule interface_database = 
  let f = 
    List.fold_left 
      (fun database ag -> 
	match declare_full (Rule rule.flag) ag database 
	with None -> failwith ("Problem with rule "^rule.flag)
	| Some a -> a)
  in
  f (f interface_database 
       rule.fixed_left_hand_side) 
    rule.fixed_right_hand_side


let rename_rule rule interface_database  = 
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
	      (fun liste (right,right') -> 
		let add_flag = 
		  List.fold_left 
		    (List.fold_left (fun flag (a,b)-> flag^"."^b^"/"^a))
		in 
		{rule with 
		  flag = add_flag (add_flag (add_flag rule.flag hs') left') right' ;
		  hand_side_common = hs ; 
		  mod_left_hand_side = left ;
		  mod_right_hand_side = right }::liste)
	      liste 
	      mod_right_hand_side')
	  liste 
	  mod_left_hand_side')
      [] 
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
  | DONT_CARE_L _ 
  | GEN_L _ 
  | CONC_L _ -> interface_database
  | RULE_L _ -> failwith "INTERNAL ERROR"
  | PREPROCESSED_RULE (_,y) -> check_rule y interface_database

let transform_model line interface_database tail = 
  match line with 
    INIT_L _   
  | DONT_CARE_L _ 
  | GEN_L _ 
  | CONC_L _ -> line::tail 
  | PREPROCESSED_RULE (x, rule) -> 
      List.fold_left 
	(fun sol l -> PREPROCESSED_RULE (x,l)::sol)
	tail 
	(rename_rule rule  interface_database)
  | RULE_L _  -> failwith "INTERNAL ERROR"
