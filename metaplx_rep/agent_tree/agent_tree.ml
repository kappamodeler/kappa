(* 2009/01/08*)
(* Meta language for Kappa *)
(* Jerome Feret*)
(* Agent tree definition*)
(* agent_tree.ml *)

open Error_handler 
open Data_structures_metaplx 

let trace = false

let error i = 
  let _ = print_string (string_of_int i) in 
  let _ = print_newline () in 
  unsafe_frozen None (Some "agent_interfaces.ml") None (Some ("line "^(string_of_int i))) (fun () -> failwith ("error"^(string_of_int i)))

let convert_declaration_into_solved_definition  x = 
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
	| Variant (agent_source,d) -> 
	    if AgentSet.mem agent_source agentset 
	    then 
	      (fadd_liste agent_source (agent_target,d) succ,
	       fst (add_count agent_target 1 npred),
	       fst (add_count agent_source 1 npred),
	       interface_map,
	       roots)
	    else
	      failwith 
		(
	      (match line with None -> "" | Some i -> "Line: "^(string_of_line i)^" ")
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
      (fun (working_list,npred) (agent,_) ->  
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
  let faiwith s = 
    let _ = print_string s in 
    let _ = print_newline () in 
    failwith s in 
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
	    SiteSet.fold 
	      (fun s interface  -> 
		List.fold_left 
		  (fun interface s -> 
		    if SiteSet.mem s interface
		    then 
		      failwith 
			("Site "^s^" is defined several time in variant "^t)
		       else 
		      SiteSet.add s interface)
		  interface
		  ((Agent_interfaces.abstract subs) s))
	      old_interface 
	      SiteSet.empty
	  in
	  aux 
	    (AgentMap.add t new_interface interface_map)
	    (deal_with t (q,npred))
    in
    aux interface_map (working_list,npred)
  in
  let _ = if trace then print_string "C8\n" in 

  let interface_list = 
    AgentMap.fold 
      (fun a b l -> (a,b)::l)
      interface [] in 
  let concrete_list = 
    AgentMap.fold 
      (fun a b l -> (a,b)::l)
      x.concrete_names [] in 
  let rec check l1 l2 = 
    match l1,l2 with 
      [], _  -> ()
    | _ , [] -> ()
    | (a,b)::q,(c,d)::q' -> 
	let x = compare a c in 
	if x<0 then check q l2
	else if x>0 then check l1 q'
	else 
	  match d with 
	    None,_ -> check q q' 
	  | Some c,_ -> 
	      if SiteSet.equal b c 
	      then check q q'
	      else 
		failwith ("Pb with Agent "^a^" interface")
  in 
  let _ = check interface_list concrete_list in 
  let solve agent map = 
    let rec aux working_list sol = 
      match working_list with 
	(t,d,subs)::q -> 
	  let subs' = Agent_interfaces.compute_subs subs d in 
	  let sol' = (t,subs')::sol in 
	  let q' = 
	    List.fold_left 
	      (fun q (t,d)  -> (t,d,subs')::q)
	      q 
	      (try AgentMap.find t succ with Not_found -> []) 
	       in 
	  aux q' sol' 
      |	[] -> sol 
    in 
    let sol = 
      aux [agent,[],
	    SiteSet.fold 
	      (fun a -> SiteMap.add a [a])
	      (AgentMap.find agent interface)
	      SiteMap.empty 
	  ] [] in
    AgentMap.add 
      agent 
      (List.map 
	 (fun (t,sol) -> 
	   let forbid = 
	     SiteMap.fold 
	       (fun a b c -> 
		 if b = [] then SiteSet.add a c 
		 else c)
	       sol SiteSet.empty
	   in 
	   {target_name=t;
             forbidden_sites=forbid;
	    substitutions=sol})
	 
	 (List.filter 
	    (fun (t,sol) -> 
	      ((try let _ = AgentMap.find t succ in false with Not_found -> true)))
		sol)) map in
  let _ = 
    if trace 
    then 
      let _ = print_string "INTERFACE \n\n" in 
      let _ = 
	AgentMap.iter 
	  (fun a b -> 
	    print_string a;
	    SiteSet.iter print_string b;
	    print_newline ())
	  interface in 
      () in 
  AgentMap.fold 
    (fun a _ map -> 
      solve a map)
    interface 
    AgentMap.empty 

let print_macro_tree handler y  = 
  let _ = handler.string "MACRO TREE \n\n" in 
  let _ = 
    AgentMap.iter 
      (fun a b -> 
	handler.string a;
	handler.line ();
	List.iter 
	  (fun a -> 
	    handler.string a.target_name;
	    handler.string ",";
	    SiteSet.iter handler.string a.forbidden_sites;
	    handler.string ",";
	    SiteMap.iter 
	      (fun a b -> 
		handler.string a;
		handler.string "\\{";
		List.iter handler.string b;
		handler.string "\\}")
	      a.substitutions;
	    handler.line ())
	  b;
	handler.line ();
	handler.line ())
      y
  in ()

let convert_declaration_into_solved_definition x = 
  if trace 
  then 
    let _ = print_declaration print_handler x in
    let y = convert_declaration_into_solved_definition x in
    let _ = print_macro_tree print_handler y in
    y
  else
    convert_declaration_into_solved_definition x


let complete subs = 
  AgentMap.fold 
    (fun a (b,c) subs ->
	  match b with None -> subs
	  | Some interface -> 
	      try let _ = AgentMap.find a subs.definitions in subs 
	      with Not_found -> 
		{subs with 
		  definitions = 
		  AgentMap.add a (Root interface,None) subs.definitions})
    subs.concrete_names 
    subs 
