(* 2009/01/08*)
(* Meta language for Kappa *)
(* Jerome Feret*)
(* Agent tree definition*)
(* agent_tree.ml *)

open Error_handler 
open Data_structures_metaplx 


let error i = 
  unsafe_frozen None (Some "agent_interfaces.ml") None (Some ("line "^(string_of_int i))) (fun () -> raise Exit) 

let convert_declaration_into_solved_definition x = 
  let add_count agent k map = 
    let old = 
      try 
	AgentMap.find agent map
      with 
	Not_found -> 
	  0 
    in 
    let k' = k + old in 
    if k'=0 
    then 
      (AgentMap.remove agent map,0)
    else
      (AgentMap.add agent k' map,k')
  in
  (* we compute the set of agent names *)
  let agentset = 
    AgentMap.fold
      (fun a _ -> AgentSet.add a)
      x.concrete_names
      (AgentMap.fold 
	 (fun a _ -> AgentSet.add a)
	 x.definitions
	 AgentSet.empty)
  in
  let fadd_liste a b map = 
    let old =
      try 
	AgentMap.find a map 
      with 
	Not_found -> 
	  [] 
    in
    AgentMap.add a (b::old) map 
  in
  let succ,npred,nsucc,interface_map,roots = 
    AgentMap.fold 
      (fun agent_target (def,line) (succ,npred,nsucc,interface_map,roots) -> 
	match def with Root l -> (succ,npred,nsucc,AgentMap.add agent_target l interface_map,agent_target::roots)
	| Variant (agent_source,_) -> 
	    if AgentSet.mem agent_source agentset 
	    then 
	      (fadd_liste agent_source agent_target succ,
	       fst (add_count agent_target 1 npred),
	       fst (add_count agent_source 1 npred),
	       interface_map,
	       roots)
	    else
	      failwith 
		(
	      (match line with None -> "" | Some i -> "Line: "^(string_of_int i)^" ")
	      ^agent_target^" is introduced as a variant of "^agent_source^" that is not defined"
									      )
		)
      x.definitions 
      (AgentMap.empty,AgentMap.empty,AgentMap.empty,AgentMap.empty,[]) in 
  let deal_with agent (working_list,npred) = 
    let succs = 
      try
	AgentMap.find agent succ 
      with 
	Not_found -> [] 
    in
    List.fold_left 
      (fun (working_list,npred) agent ->  
	let npred,n = add_count agent (-1) npred in 
	(if n=0 
	then 
	  agent::working_list
	else
	  working_list),
	npred)
      (working_list,npred) succs 
  in
  let working_list,npred = 
    List.fold_left 
      (fun state agent -> deal_with agent state)
      ([],npred)
      roots 
  in
  let interface = 
    let rec aux interface_map (working_list,npred) = 
      match working_list with 
	[] -> interface_map 
      |	t::q -> 
	  let def = 
	    try 
	      AgentMap.find t x.definitions
	    with 
	      Not_found -> error 102 
	  in 
	  let agent,decl = 
	    match def with 
	      Variant(a,decl),_ -> a,decl
	    | _ -> error 107 
	  in
	  let old_interface = 
	    try 
	      AgentMap.find agent interface_map 
	    with 
	      Not_found -> error 113 
	  in 
	  let subs = Agent_interfaces.compute_interface old_interface decl in 
	  let new_interface = 
	    fst 
	      (List.fold_left 
		 (fun (interface,black) s -> 
		   List.fold_left 
		     (fun (interface,black) s -> 
		       if SiteSet.mem s black 
		       then 
			 failwith 
			   ("Site "^s^" is defined several time in variant "^t)
		       else 
			 s::interface,SiteSet.add s black)
		     (interface,black) 
		     (subs s))
		 ([],SiteSet.empty)
		 old_interface)
	  in
	  aux 
	    (AgentMap.add t new_interface interface_map)
	    (deal_with t (q,npred))
    in
    aux interface_map (working_list,npred)
  in
  ()
