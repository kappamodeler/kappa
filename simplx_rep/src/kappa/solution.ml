open Error
open Mods2

let pi = print_int
let nl = print_newline

module AA = Array_ext.Make(struct type t = Agent.t let default = Agent.empty end)
module PA = Array_ext.Make(struct type t = (int*string) let default = (-1,"") end)

type marshalized_t = {f_agents:Agent.t IntMap.t ; f_links:(int*string) PortMap.t; f_fresh_id:int}

type t = {agents:int AA.t ; links:(int*string) PA.t; fresh_id:int} (*partager agents --hash des vues--*)

let empty() = {agents = AA.create 100 ; links = PA.create 100 ; fresh_id = 0}

(***********************************)
(*converts imperative type into functional one -for marshalization purpose-*)
let ft_of_t sol = 
  let f_agents = AA.fold (fun id ag m -> IntMap.add id ag m) sol.agents IntMap.empty
  and f_links = PA.fold (fun l1 l2 m -> PortMap.add l1 l2 m) sol.links PortMap.empty
  and f_fresh_id = sol.fresh_id
  in
    {f_agents=f_agents ; f_links = f_links ; f_fresh_id = f_fresh_id}

let t_of_ft f_sol = 
  let agents = IntMap.fold (fun id ag sol -> AA.add id ag sol) f_sol.f_agents (AA.create (IntMap.size f_sol.f_agents))
  and links = PortMap.fold (fun q q' links -> PA.add q q' links) f_sol.f_links (PA.create (PortMap.size f_sol.f_links))
  and fresh_id = f_sol.f_fresh_id
  in
    {agents=agents ; links=links ; fresh_id=fresh_id}

(**marshalization*)
let marshal sol = ft_of_t sol

(**unmarshalization*)
let unmarshal ft_sol = t_of_ft ft_sol 

(***********************************)

let copy sol = 
  let agents' = 
    AA.fold (fun id ag agents' -> AA.add id ag agents') sol.agents (AA.create (AA.size sol.agents))
  and links' = PA.copy sol.links
  in
    {sol with agents=agents';links=links'}

let agents sol = sol.agents

let is_empty sol = AA.is_empty sol.agents

let get_port (i,s) sol = PA.find (i,s) sol.links  

(*Returns the list of agents that composes the solution*)
let elements sol = AA.fold (fun id ag cont -> (ag,id)::cont) sol.agents []

(*Returns current fresh id of the solution*)
let fresh_id sol = sol.fresh_id

let agent_of_id id sol = 
  try AA.find id sol.agents 
  with Not_found ->
    let error = Printf.sprintf "Solution.agent_of_id: %d does not correspond to any agent" id in
      raise (Runtime error)

(*Returns a string which corresponds to the given solution in kappa format*)
let kappa_of_solution ?(full=false) ?whitelist sol =
  let with_whitelist,wl = match whitelist with None -> (false,IntSet.empty) | Some set -> (true,set) in
    if is_empty sol then "" 
    else
      let lnk_id = ref 0 in
      let get_lnk () = (let t = (!lnk_id) in lnk_id := (!lnk_id)+1 ; t) 
      and hsh_link = Hashtbl.create 10 
      in
      let longstr_of_agents agents =
	AA.fold (fun id ag ls ->
		   if Agent.is_empty ag or (with_whitelist && not (IntSet.mem id wl)) then ls
		   else
		     let name = if full then Printf.sprintf "%s#%d" (Agent.name ag) id else Agent.name ag 
		     in
		       LongString.concat ~sep:','
			 (Printf.sprintf "%s(%s)"
			    name
			    (String.concat ","
			       (Agent.fold_interface 
				  (fun site (inf,lnk) cont -> 
				     if site = "_" then cont else
				       let (inf,lnk) = Agent.state ag site in
				       let s_inf = 
					 match inf with
					     Agent.Marked s -> ("~"^s) 
					   | Agent.Wildcard -> ""
					   | (Agent.Bound | Agent.Free) ->
					       let s = "Solution.kappa_of_solution: not a valid mark for a state" in 
						 runtime 
						   (Some "solution.ml",
						    Some 96,
						    Some s)
						   s
				       and s_lnk = 
					 match lnk with
					     Agent.Bound -> "!"
					   | Agent.Free -> ""
					   | Agent.Wildcard -> "?"
					   | Agent.Marked _  ->
					       let s = "Solution.kappa_of_solution: not a valid mark for a link" in 
						 runtime 
						   (Some "solution.ml",
						    Some 106,
						    Some s)
						   s
				       in 
				       let s_site = site^s_inf^s_lnk
				       in
					 try
					   let (id',site') = PA.find (id,site) sol.links in
					     try
					       let n = Hashtbl.find hsh_link (id',site') in
						 (s_site^(string_of_int n))::cont
					     with Not_found ->
					       let n = get_lnk() in
						 Hashtbl.add hsh_link (id,site) n ;
						 (s_site^(string_of_int n))::cont
					 with 
					     Not_found -> 
					       if lnk=Agent.Bound 
					       then (s_site^"_"(*string_of_int (get_lnk())*))::cont 
					       else s_site::cont 
				  ) ag  []
			       )
			    )
			 ) ls
		) agents LongString.empty
      in
	LongString.to_string (longstr_of_agents sol.agents)

let to_dot ?(low_reso=false) sol = 
  let hsh_done = Hashtbl.create 10 in
    String.concat "\n"
      (AA.fold (fun id ag cont ->
		  let s_ag = (Agent.to_str ag)^(string_of_int id)
		  in
		    (Agent.fold_interface 
		       (fun site (inf,_) cont -> 
			 let s_inf = 
			    match inf with
				Agent.Marked s -> ("~"^s) 
			      | Agent.Wildcard -> ""
			      | _ -> raise (Runtime "Solution.dot_of_solution: mark is not well formed")
			  in
			  let s_site = site^s_inf
			  in
			    try
			      let (id',site') = PA.find (id,site) sol.links in
			      let ag'=agent_of_id id' sol in
			      let (inf',_) = Agent.state ag' site' in
			      let s_inf' = 
				match inf' with
				    Agent.Marked s -> ("~"^s) 
				  | Agent.Wildcard -> ""
				  | _ -> raise (Runtime "Solution.dot_of_solution: mark is not well formed")
			      in
			      let s_site' = site'^s_inf'
			      in
			      let s_ag'=(Agent.to_str ag')^(string_of_int id') in
				if Hashtbl.mem hsh_done (s_ag',s_site',s_ag,s_site) then cont
				else (
				  Hashtbl.add hsh_done (s_ag,s_site,s_ag',s_site') () ;
				  Hashtbl.add hsh_done (s_ag',s_site',s_ag,s_site) () ;
				  if low_reso then
				    (Printf.sprintf "\"%s\"->\"%s\"[dir=none]" s_ag s_ag')::cont
				  else
				    (Printf.sprintf 
				       "\"%s\"->\"%s\" [fontcolor=grey,labelfontsize=9,taillabel=\"%s\",headlabel=\"%s\",dir=none]" 
				       s_ag s_ag' s_site s_site')::cont
				)
			    with 
				Not_found -> 
				  if Hashtbl.mem hsh_done (s_ag,"","","") then cont
				  else (
				    Hashtbl.add hsh_done (s_ag,"","","") () ;
				    ("\""^s_ag^"\"[color=black]")::cont 
				  )
		       ) ag  []
		    )@cont
	       ) sol.agents []
      )

(*Binding of two given sites in a solution*)
let bind (id,x) (id',x') sol =
  let ag = agent_of_id id sol
  and ag' = agent_of_id id' sol
  in
  let (inf,lnk) = Agent.state ag x
  and (inf',lnk') = Agent.state ag' x'
  in
    if (lnk=Agent.Bound) or (lnk'=Agent.Bound) then 
      let s = "Solution.bind: sites "^x^" and "^x'^" are already bound" in 
      Error.runtime 
	(Some "solution.ml",
	 Some 200,
	 Some s) 
	s
    else
      let ag1 = Agent.modif ag x 
      and ag2 = Agent.modif ag' x' 
      in
	{sol with 
	   agents = AA.add id' ag2 (AA.add id ag1 sol.agents);
	   links = PA.add (id',x') (id,x) (PA.add (id,x) (id',x') sol.links)
	}  

(*Unbinding of two sites in the solution*)
let unbind (id,x) (id',x') sol =
  try 
    let (id1,x1) = PA.find (id,x) sol.links in
      if (id1=id') && (x1=x') then
	
	let ag = agent_of_id id sol
	and ag' = agent_of_id id' sol
	in
	let ag1 = Agent.modif ag x 
	and ag2 = Agent.modif ag' x'
	in
	  {sol with 
	     agents = AA.add id' ag2 (AA.add id ag1 sol.agents);
	     links = PA.remove (id',x') (PA.remove (id,x) sol.links)
	  }  
      else 
	let s = 
	  ("Solution.unbind: cannot unbind agents which are not linked: "
	   ^(string_of_int id)^","^x^"-"^(string_of_int id1)^","^x') in 
	Error.runtime 
	  (Some "solution.ml",
	   Some 234,
	   Some s)
	  s
  with Not_found -> 
    let s = "Solution.unbind: cannot unbind agents which are not linked" in 
    Error.runtime
      (Some "solution.ml",
       Some 241,
       Some s)
      s
	 

(*Mark the site (id,x) with the string s which must contain only ascii characters*)
let mark (id,x,s) sol =
  {sol with agents = AA.add id (Agent.mark (agent_of_id id sol) x s) sol.agents}

(*Adds a new agent in the solution*)
let add ?(with_id=(-1)) ag sol =
  if with_id<0 then
    let agents = AA.add sol.fresh_id ag sol.agents in
      {sol with agents=agents;fresh_id=sol.fresh_id+1}
  else
    let agents = AA.add with_id ag sol.agents in
      {sol with agents=agents}

(*Removes the agent id from the solution*)
let remove id sol = 
  let warning = ref false in
  let sol =
    let ag = try AA.find id sol.agents with Not_found -> 
      let s = "Solution.remove" in
      Error.runtime 
	(Some "solution.ml",
	 Some 269,
	 Some s)
	s 
	  
    in      
      Agent.fold_interface (fun site _ sol -> 
			try
			  let (i,x) = get_port (id,site) sol in
			    warning := true; (*contextual application of deletion*)
			    unbind (id,site) (i,x) sol
			with Not_found -> sol
		     ) ag  sol
  in
  let agents = AA.remove id sol.agents in
    (!warning,{sol with agents=agents})

(*Instructions to be compiled for testing unification of solutions*)
type inst = Goto of string * int * string | Check of string*Agent.mark*Agent.mark | Test of int * string

(*pretty prints the instructions of a compilation*)
let string_of_recognition reco =
  let str_of_inst i = 
    match i with
	Goto (site,role,name) -> Printf.sprintf "--goto-->%s(%s) with role %d" name site role
      | Test (role,site) -> Printf.sprintf "test bound to site %s with role %d" site role
      | Check (name,inf,lnk) -> 
	  let s_inf = 
	    match inf with
		Agent.Wildcard -> ["any mark"] 
	      |  Agent.Marked s -> ["marked with "^s]
	      |  (Agent.Bound | Agent.Free) -> raise (Error.Runtime "Solution.print_instruction: malformed state")
	  and s_lnk = 
	    match lnk with
		Agent.Bound -> ["bound"] 
	      | Agent.Free ->  ["free"] 
	      | Agent.Wildcard -> ["any link"]
	      | Agent.Marked _ -> raise (Error.Runtime "Solution.print_instruction: malformed state")
	  in
	    Printf.sprintf "in %s check state is %s" name (String.concat " & " (s_lnk@s_inf))
  in
  let l = 
    IntMap.fold 
      (fun role (map_inst_loc,map_inst_rem) cont -> 
	 let cont = 
	   StringMap.fold (
	     fun site inst cont -> 
	       let str = Printf.sprintf "(local check) role:%d, site:%s %s" role site (str_of_inst inst) in
		 str::cont
	   ) map_inst_loc cont 
	 in
	   StringMap.fold (
	     fun site inst cont -> 
	       let str = 
		 Printf.sprintf "(move) role:%d, site:%s %s" role site (str_of_inst inst) 
	       in
		 str::cont
	   ) map_inst_rem cont
      ) reco []
  in
    String.concat "\n" l

(*compiles a solution into a series of instructions. Returns a triple (compil,role_map,fresh_role) where 
 - compil : Map [role -> ( Map [site -> local instructions], Map [site -> remote instructions])]
  local instructions  = {Check(site,state_info,state_link),Test(role,site)} 
  remote instructions = {Goto(site,role,name)} 
 - role_map : Map [pid -> role]
 - fresh_role : next role number to be assigned
*)

let get_instructions id compil role_of_id fresh_role sol =
  let rec span current_id role_of_id compil fresh_role =    

    let rec check_interface interface inst_local inst_remote compil role_of_id fresh_role =
      match interface with
	  site::tl -> (
	    let ag = agent_of_id current_id sol in
	    let (inf,lnk) = Agent.state ag site 
	    and name = Agent.name ag
	    in
	    let inst_local' = StringMap.add site (Check (name,inf,lnk)) inst_local in
	      if lnk = Agent.Bound then 
		try 
		  let (id',site') = PA.find (current_id,site) sol.links in (*may raise Not_found*)
		  let role' = try IntMap.find id' role_of_id with Not_found -> 0 in 
		    if role'=0 then 
		      let (compil',role_of_id',fresh_role') = 
			span id' (IntMap.add id' fresh_role role_of_id) compil (fresh_role+1)
		      in
		      let inst_remote' = 
			StringMap.add site (Goto (site',fresh_role,Agent.name (agent_of_id id' sol))) inst_remote
		      in
			check_interface tl inst_local' inst_remote' compil' role_of_id' fresh_role'
		    else
		      if role'< (IntMap.find current_id role_of_id) then 
			let inst_remote' = 
			  (*Note: some Test instructions could be avoided. Ex: 1:x Goto 2:x' then Test 2:x'--x:1*)
			  (*But it seems complicated to do this here*)
			  StringMap.add site (Test (role',site')) inst_remote
			in 
			  check_interface tl inst_local' inst_remote' compil role_of_id fresh_role
		      else check_interface tl inst_local' inst_remote compil role_of_id fresh_role
		with 
		    Not_found -> check_interface tl inst_local' inst_remote compil role_of_id fresh_role
	      else 
		check_interface tl inst_local' inst_remote compil role_of_id fresh_role
	  )
	| [] -> (inst_local,inst_remote,compil,role_of_id,fresh_role)
	    
    in
    let (inst_local,inst_remote,compil',role_of_id',fresh_role') =
      check_interface 
	(StringSet.elements (Agent.fold_interface 
			       (fun x _ -> StringSet.add x) (agent_of_id current_id sol) StringSet.empty))
	StringMap.empty 
	StringMap.empty 
	compil 
	role_of_id 
	fresh_role
    in
    let compil'' = IntMap.add (IntMap.find current_id role_of_id') (inst_local,inst_remote) compil' 
    in
      (compil'',role_of_id',fresh_role')
  in
    span id (IntMap.add id fresh_role role_of_id) compil (fresh_role+1)

let cc_of_id id sol blacklist =
  let rec f todo_ids cc blacklist =
    match todo_ids with
	id::tl -> 
	  let ag = 
	    try 
	      agent_of_id id sol 
	    with Not_found -> 
	      let s = "Solution.cc_of_id: malformed solution" in
		Error.runtime 
		  (Some "solution.ml",
		   Some 405,
		   Some s)
		  s
	  in
	  let cc = {cc with agents = AA.add id ag cc.agents; fresh_id = max cc.fresh_id (id+1)} in
	  let todo,cc,blacklist = 
	    Agent.fold_interface (fun x (_,lnk) (tl,cc,bl) -> 
				    match lnk with
					Agent.Bound ->
					  begin
					    try
					      let (id',y) = get_port (id,x) sol in
						if IntSet.mem id' bl then 
						  (tl,{cc with 
							 links = PA.add (id',y) (id,x) (PA.add (id,x) (id',y) cc.links) ;
						      },bl)
						else 
						  (id'::tl,{cc with 
							      links = PA.add (id',y) (id,x) (PA.add (id,x) (id',y) cc.links) ;
							   },IntSet.add id' bl)
					    with
						Not_found -> (tl,cc,bl) (*sol is a pattern*)
					  end
				      | _ -> (tl,cc,bl)
				 ) ag (tl,cc,blacklist)
	  in
	    f todo cc blacklist
      | [] -> (cc,blacklist)
  in
    f [id] (empty()) (IntSet.add id blacklist)
    
let paths_of_id id sol = 
  let rec f todo_ids cc paths blacklist exiting_ports= 
    match todo_ids with
	id::tl ->
	  let todo,paths,blacklist,exiting_ports = 
	    let ag = agent_of_id id sol in
	      Agent.fold_interface (fun site (_,lnk) (todo,paths,bl,exiting_ports) ->
				      match lnk with
					  Agent.Bound ->
					    let (id',site') = get_port (id,site) sol in
					    let name = Agent.name (agent_of_id id' sol) in
					      if IntSet.mem id' bl then 
						let paths = 
						  if PortSet.mem (id',site') exiting_ports then paths 
						  else Paths.add name id' site' paths 
						in
						  (todo,paths,bl,PortSet.add (id,site) exiting_ports)
					      else
						let paths = 
						  if PortSet.mem (id',site') exiting_ports then paths 
						  else Paths.add name id' site' paths
						in
						  (id'::todo,paths,IntSet.add id' bl,PortSet.add (id,site) exiting_ports)
					| _ -> (todo,paths,bl,exiting_ports)
				   ) ag (tl,paths,blacklist,exiting_ports)
	  in
	  let cc = IntSet.add id cc
	  in
	    f todo cc paths blacklist exiting_ports
      | [] -> (cc,paths)
  in
    f [id] IntSet.empty (Paths.empty()) (IntSet.singleton id) (PortSet.empty)
  
let rec find_path id map_paths acc = 
  try
    let s,id',s' = IntMap.find id map_paths in find_path id' map_paths ((id,s)::(id',s')::acc)
  with
      Not_found -> acc


let split sol_init = 
  let map_cc,_,_ = 
    AA.fold (fun id _ (map_cc,blacklist,fresh) -> 
	       if IntSet.mem id blacklist then (map_cc,blacklist,fresh)
	       else
		 let cc,blacklist = cc_of_id id sol_init blacklist in
		   (IntMap.add fresh cc map_cc,blacklist,fresh+1)
	    ) sol_init.agents (IntMap.empty,IntSet.empty,0)
  in
    map_cc

(*entries : pid which are roots of a connected component *)
(*compil :  role -> (site -> inst_loc, site -> inst_rem) *)
(*role_map: pid -> role *)

type cc_recognition = (inst StringMap.t * inst StringMap.t) IntMap.t
let empty_reco = IntMap.empty

let recognitions_of_cc ?(rooted=false) sol = 
  if rooted then
    let id,_ = AA.random sol.agents in
    let c,r,f = get_instructions id empty_reco IntMap.empty 1 sol in
    let role_of_id = IntMap.fold (fun role id map -> IntMap.add id role map) r IntMap.empty in
      [(id,c,role_of_id)]
  else
    AA.fold (fun id ag cont -> 
	       let c,r,f = get_instructions id empty_reco IntMap.empty 1 sol in
	       let role_of_id = IntMap.fold (fun role id map -> IntMap.add id role map) r IntMap.empty in
		 (id,c,role_of_id)::cont
	    ) sol.agents []

exception Matching_failed 

(*returns role_map if succeed and Matching_failed if not*)
let match_id_with_role ?(pushout=false) id role compil sol =
  let rec execute compil id role_map sol =
    let role = IntMap.find id role_map in
    let ag = agent_of_id id sol 
    in 
    let name = Agent.name ag 
      (* and environment  = Agent.environment  ag*) 
    in
    let (inst_loc,inst_rem) = IntMap.find role compil in
      if
	(StringMap.fold 
	   (fun site_p instruction b -> 
	      try
		if Agent.mem_interface site_p ag  then
		  match instruction with
		      Check(name_p,inf_p,lnk_p) -> 
			let (inf,lnk) = Agent.state ag site_p in
			  if (name=name_p) 
			    && (Agent.compatible_mark ~strict:true inf inf_p) 
			    && (Agent.compatible_mark ~strict:true lnk lnk_p) 
			  then b
			  else raise Matching_failed
		    | _ -> 
			let s = "Solution.match_id_with_role: invalid instruction" in
			runtime 
			  (Some "solution.ml",
			   Some 536,
			   Some s)
			  s
		else 
		  if pushout then b
		  else raise Matching_failed
	      with Not_found -> 
		let s = "Solution.match_id_with_role: unexpected Not_found exception" in
		runtime
		  (Some "solution.ml",
		   Some 546,
		   Some s)
		  s
		(*raise Matching_failed*)
	   ) inst_loc true
	) 
      then
	(StringMap.fold 
	   (fun site_p instruction role_map -> 
	      (*let ag = agent_of_id id sol in
		let interface = Agent.interface ag in
	      *)
	      if Agent.mem_interface  site_p ag then
		match instruction with
		    Test(role_p',site_p') -> (
		      try
			let (id',site') = PA.find (id,site_p) sol.links in
			let role' = try IntMap.find id' role_map with Not_found -> 0 in
			  if (role'=role_p') (*correction 10 Fev, bug WALTER*) && (site_p'=site') then role_map 
			  else raise Matching_failed
		      with Not_found -> (*(id,site_p) not linked*)
			if pushout then
			  let (_,lnk) = Agent.state ag site_p in
			    match lnk with Agent.Bound -> role_map | _ -> raise Matching_failed
			else raise Matching_failed
		    )
		  | Goto(site_p',role_p',name_p') -> (
		      try
			let (id',site') = PA.find (id,site_p) sol.links in
			let role' = try IntMap.find id' role_map with Not_found -> 0 in
			  if (role'=0) && (site'=site_p') then (*if role not attributed and compatible ports*)
			    let role_map' = IntMap.add id' role_p' role_map in
			      execute compil id' role_map' sol
			  else raise Matching_failed
		      with Not_found -> 
			if pushout then
			  let (_,lnk) = Agent.state ag site_p in
			    match lnk with Agent.Bound -> role_map | _ -> raise Matching_failed
			else 
			  raise Matching_failed
		    )
		  | _ -> raise (Runtime "Solution.match_id_with_role: invalid instruction")
	      else
		if pushout then role_map
		else raise Matching_failed
		  
	   ) inst_rem role_map
	)
      else raise Matching_failed
  in
    execute compil id (IntMap.add id role IntMap.empty) sol

(*list_compil = ((id_p,compil,role_of_id_p),sol_pattern) list*)
(*array_ids is a subset of sol_inst.agents*)
let unify ?(rooted=false) ?(pushout=false) (list_compil,cc_pat) (array_ids,sol_inst) =
  let length,assoc_map = 
    AA.fold (fun id_sol ag_sol (n,assoc_map) ->
	       let list_compil = 
		 if list_compil = [] then (print_string "Warning: empty pre-compilation list\n" ; []) (*should not happen*)
		 else 
		   if rooted then [List.hd list_compil]
		   else list_compil
	       in
		 List.fold_right (fun (id_p,compil,id_of_role) (n,assoc_map) ->
				    try 
				      let ag_p = agent_of_id id_p cc_pat in
					if not ((Agent.name ag_sol) = (Agent.name ag_p)) then raise Matching_failed 
					else
					  let map = 
					    if pushout then 
					      match_id_with_role ~pushout:true id_sol 1 compil sol_inst 
					    else match_id_with_role id_sol 1 compil sol_inst 
					  in
					  let assoc =
					    IntMap.fold (fun id_sol role assoc ->
							   let id_p = 
							     try IntMap.find role id_of_role
							     with Not_found -> 
							       let s =  "Solution.unify : Not_found" in
							       runtime 
								 (Some "solution.ml",
								  Some 627,
								  Some s)
								 s
							   in
							     IntMap.add id_p id_sol assoc
							) map IntMap.empty
					  in
					    if rooted then (n+1,(*IntMap.add*) AssocArray.add n assoc assoc_map) 
					    else
					      let _ = 
						(*IntMap.iter*)
						AssocArray.iter (fun _ assoc' -> 
								   let id_sol' = IntMap.find id_p assoc' in
								     if id_sol' = id_sol then raise False
								     else ()
								) assoc_map
					      in
						(n+1,(*IntMap.add n assoc assoc_map*)AssocArray.add n assoc assoc_map)
				    with Matching_failed | False -> (n,assoc_map)
				 ) list_compil (n,assoc_map)
	    ) array_ids (0,(*IntMap.empty*) (AssocArray.create 1) )
  in
    if length = 0 then raise Matching_failed
    else assoc_map


(*Tests whether an agent #id is instanciated (no semi-links)*)
exception Not_instanciated
let is_instanciated id sol =
  let ag = agent_of_id id sol in
  (*let interface = Agent.interface ag in*)
    try
      Agent.fold_interface 
	(fun site (inf,lnk) b ->
	  if (* inf = Agent.Wildcard or*) (lnk = Agent.Wildcard) then raise Not_instanciated
	  else
	    if lnk = Agent.Bound then
	      if PA.mem (id,site) sol.links then b
	      else raise Not_instanciated
	    else b
		) ag true
    with Not_instanciated -> false

type action =   Bind of (string*int*string) 
		| Break of (string*int*string)
		| Mark of (string*string)
		| Modify of string (*break a semi-link*)
		| Remove 

let get_binding act_msg_list = 
  let rec f l acc = 
    match l with
	[] -> acc
      | (act,_)::tl -> 
	  match act with
	      Bind triple -> f tl (triple::acc)
	    | _ -> f tl acc
  in
    f act_msg_list []


let string_of_action id act =
  match act with
      Bind (s,i,s') -> Printf.sprintf "BND (#%d,%s) (#%d,%s)" id s i s'  
    | Break (s,i,s') -> Printf.sprintf "BRK (#%d,%s) (#%d,%s)" id s i s'  
    | Mark (s,mk) -> Printf.sprintf "MOD (#%d,%s) with %s" id s mk  
    | Modify s -> Printf.sprintf "BRK (#%d,%s) _" id s
    | Remove -> Printf.sprintf "DEL #%d" id


(*module ActSet = Set.Make (struct type t=action let compare = compare end)*)

type msg = Warning of string | OK
let r_ok = 1
and r_sem_break_rhs = 1 (*breaking a semi link on rhs*)
and r_rem = 1 (*removing an agent*)
and r_add = 1 (*adding a new agent*)
and r_rem_sem = 2 (*removing an agent with semi-links*)
and r_realloc = 2 (*reallocation of a link*)

(*ce qui disparait/aparait est instancie*)

exception Interrupt of IntSet.t
let diff sol1 sol2 =
  let (kept:IntSet.t) = (*longest invariant prefix of the lhs*)
    try
      AA.fold (fun id ag1 (kept:IntSet.t) -> 
		 try
		   let ag2 = AA.find id sol2.agents in
		     if ((Agent.name ag2) = (Agent.name ag1)) 
		       && ((Agent.interface ag2) = (Agent.interface ag1)) 
		     then IntSet.add id kept else (raise (Interrupt kept))
		 with Not_found -> (raise (Interrupt kept))
	      ) sol1.agents IntSet.empty
    with Interrupt kept -> kept
  in
  let n_rm = sol1.fresh_id - (IntSet.cardinal kept) in
  let assign id =
    if IntSet.mem id kept then id else (id+n_rm)
  in
    (*computing instructions*)
  let modif rate n =
    if (rate = (-1)) then (-1) else (rate+n)
  in
  let map,add_sol,rate = 
    AA.fold
      (fun id ag2 (m,add_sol,rate) ->
	 (*if Agent.is_empty ag2 then (m,add_sol,rate) else*)
	   (*let interface' = Agent.interface ag2 in*)
	   if IntSet.mem id kept then 
	     let ag = agent_of_id id sol1 in 
	       Agent.fold2iz_interface  
		 (fun site (inf,lnk) (inf',lnk') (m,add_sol,rate) -> 
		    let (mod_inf,rate) =
		      if not (inf'=inf) then
			match inf' with
			    Agent.Marked s -> 
			      if inf = Agent.Wildcard then
				([(Mark (site,s),Warning "Rule instanciates an undefined internal state")],modif rate r_ok)
			      else
				([(Mark (site,s),OK)],modif rate r_ok)     (*Activate*)
			  | Agent.Wildcard -> 
			      raise (Found 
				       ("Site state not specified for "
					^site
					^" in agent "^(Agent.to_str ag2))
				    )
			  | (Agent.Bound|Agent.Free) -> raise (Runtime "Solution.diff: malformed state")
		      else ([],rate)
		    in

		    let (mod_lnk,rate) =
		      if not (lnk'=lnk) then
			if lnk = Agent.Wildcard then raise (Found "rule cannot remove a wildcard")
			else
			  match lnk' with
			      Agent.Bound -> ( (*agent is bound in sol2*)
				try
				  let (id2,site2) = PA.find (id,site) sol2.links in
				    (*site=Bound & Bind site with (id'',x)*)
				    (*binding counts as one operation*)
				  let id2 = assign id2 in (*shifts id2 if necessary*)
				    if (id2<id) or (id=id2 && (compare site site2 =(-1))) (*only binds to a smaller id*)
				    then ([(Bind (site,id2,site2),OK)],modif rate r_ok)
				    else ([],rate)
				with Not_found -> raise (Found "cannot create a semi-link on rhs")
			      )
			    | Agent.Free -> ( 
				try
				  let (id',x) = PA.find (id,site) sol1.links in
				    if (id<id') or (id=id' && (compare site x =(-1))) then
				      ([(Break(site,id',x),OK)],modif rate r_ok)
				    else ([],rate) 
				with Not_found -> (*W:modifying a dangling link.. site 2 becomes Free*)
				  ([(Modify site,Warning "Breaking a semi-link on rhs")],r_sem_break_rhs)
			      )
			    | Agent.Wildcard -> raise (Found "rule cannot add a wildcard")
			    | Agent.Marked _  -> raise (Runtime "Solution.diff: malformed state")

		      else (*lnk=lnk' meaning the link state does not change*)
			if lnk = Agent.Bound then (*lnk=lnk'=Bound*)
			  if PA.mem (id,site) sol2.links then (*not a dangling link in sol2*)
			    let (id2,x2) = PA.find (id,site) sol2.links (*cannot fail*)
			    in
			    let id2 = assign id2 in (*shifts id2 if necessary*)
			      try
		 		let (id1,x1) = PA.find (id,site) sol1.links
				in (*not a dangling link in sol1*)
				  if (id1=id2 && x1=x2) then ([],rate) (*link is unchanged*)
				  else
				    let msg = "link on right hand side is a reallocation of a link in the left hand side"
				    in
				    let inst1 = 
				      if (id<id1) then [(Break(site,id1,x1),Warning msg)] (*break with bigger ids*)
				      else []
				    and inst2 =
				      if (id>id2) then [(Bind(site,id2,x2),Warning msg)] (*bind with smaller ids*)
				      else []
				    in
				      (inst2@inst1,modif rate r_realloc)
			      with Not_found ->  (*dangling link in sol1*)
				let s = "link on right hand side is a reallocation of a semi link in the left hand side" in 
				found 
				  (Some "solution.ml",
				   Some 811,
				   Some s)
				  s
			  else 
			    if PA.mem (id,site) sol1.links then (*not a dangling link in sol1*)
			      (*but dangling in sol2*)
			      let s = "cannot add semi-link on the right hand side" in
			      found 
				(Some "solution.ml",
				 Some 820,
				 Some s)
				s
			    else ([],rate) (*link is dangling in both rhs and lhs*)
			else (*lnk'=lnk=Free|Wildcard*)
			  ([],rate) 
		    in
		    let l = try IntMap.find id m with Not_found -> [] 
		    in
		      (*modification on sites of id as well as new binding or unbind operations*)
		    let l = (mod_inf@mod_lnk@l) in
		      if l = [] then (m,add_sol,rate)
		      else
			(IntMap.add id l m,add_sol,rate)
		 ) 
		 ag ag2 
		 (m,add_sol,rate)

	   else (*id in add*)
	     if is_instanciated id sol2 then 
	       let m = 
		 Agent.fold_interface (fun site (_,lnk) m ->
					 if lnk = Agent.Bound then (*added agent should be immediately bound*)
					   try
					     let (id2,site2) = get_port (id,site) sol2
					     in
					     let id2 = assign id2 in
					     let l = try IntMap.find (assign id) m with Not_found -> [] in
					       if id2<(assign id) then 
						 IntMap.add (assign id) ((Bind(site,id2,site2),OK)::l) m
					       else m
					   with Not_found -> 
					     let s = "Solution.diff: trying to add an agent with a semi-link" in
					     runtime
					       (Some "solution.ml",
						Some 855,
						Some s)
					       s
					 else m (*no action to do on added agent*)
				      ) (agent_of_id id sol2) m
	       in
		 (m,IntMap.add (assign id) (Agent.detach (agent_of_id id sol2)) add_sol, modif rate r_add) 
	     else 
	       let s= "adding an agent with a non instanciated link" 
	       in 
	       found
		 (Some "solution.ml",
		  Some 867,
		  Some s)
		 s
      ) sol2.agents (IntMap.empty,IntMap.empty,0)
  in
  let (map,rate,n_rm) =
    AA.fold (fun id ag (m,rate,n_rm) -> (*listing agents to be removed*)
	       if not (IntSet.mem id kept) (*&& not (Agent.is_empty ag)*) then 
		 let add_warn = 
		   if not (add_sol = IntMap.empty) then 
		     "\nBoth removing and adding agents..."
		     ^" Maybe arguments sequence in the lhs does not match the rhs?"
		   else "" 
		 and n_rm = n_rm +1
		 in
		   if is_instanciated id sol1 then
		     let warn_msg = Printf.sprintf "Removing agent %s#%d%s" (Agent.name ag) id add_warn in
		       (IntMap.add id [(Remove,Warning warn_msg)] m,modif rate r_rem,n_rm)
		   else
		     let warn_msg = Printf.sprintf "Removing agent %s#%d which is not fully instanciated %s" 
		       (Agent.name ag) id add_warn in
		       (IntMap.add id [(Remove,Warning warn_msg)] m,modif rate r_rem_sem,n_rm)
	       else (m,rate,n_rm)
	    ) sol1.agents (map,rate,0)
  in
  let corr = (IntMap.size add_sol) - n_rm in
    (map,add_sol,rate,corr)

    
    
let get_ports sol =
  AA.fold (fun i ag cont -> 
	     Agent.fold_interface  (fun s _ cont -> (i,s)::cont) ag  cont
	  ) sol.agents []

(*let rec satisfy constraints assoc sol = try match constraints with
  CC (i,joints,disjoints,sep)::tl -> begin let i_sol = IntMap.find i
  assoc and joints_sol = IntSet.fold (fun i set -> IntSet.add
  (IntMap.find i assoc) set) joints IntSet.empty (*and disjoints_sol =
  IntSet.fold (fun i set -> IntSet.add (IntMap.find i assoc) set)
  disjoints IntSet.empty*) in let excl_set = IntSet.fold (fun i set ->
  let i' = IntMap.find i assoc in connected_names i' sol i_sol set )
  disjoints sep in try (*allowing blacklisted agent types to be
  accepted if corresponding agent is in the injection of the lhs*) let
  cc_i_sol = connected_component i_sol sol ~exclude:excl_set in if
  (IntSet.subset joints_sol cc_i_sol) (*&& (IntSet.is_empty
  (IntSet.inter disjoints_sol cc_i_sol)) *) then satisfy tl assoc sol
  else false with False -> false (*can be returned by
  connected_component*) end | [] -> true with Not_found ->
  Error.runtime "Solution.satisfy: assoc invariant violation"
*)

let compose sol1 sol2 = 
  let renaming,sol = AA.fold (fun id ag (renaming,sol2) -> 
				let id',renaming,fresh_id = 
				  try (IntMap.find id renaming,renaming,sol2.fresh_id) 
				  with Not_found -> 
				    (sol2.fresh_id,IntMap.add id sol2.fresh_id renaming,sol2.fresh_id+1) 
				in
				let agents = AA.add id' ag sol2.agents
				in
				let links,renaming,fresh_id = 
				  Agent.fold_interface (fun site _ (links,renaming,fresh_id) ->
							  try
							    let (i,s) = get_port (id,site) sol1 in
							    let i',renaming,fresh_id =
							      try (IntMap.find i renaming,renaming,fresh_id) 
							      with Not_found -> 
								(fresh_id,IntMap.add i fresh_id renaming,fresh_id+1) 
							    in
							    let links = PA.add (id',site) (i',s) links in
							      (links,renaming,fresh_id)
							  with Not_found -> (links,renaming,fresh_id)
						       ) ag (sol2.links,renaming,fresh_id)

				in
				  (renaming,{agents = agents ; links = links ; fresh_id = fresh_id})
			     ) sol1.agents (IntMap.empty,sol2)
  in
    sol

let multiply sol n = 
  if n = 0 then Printf.printf "Warning %s is rescaled to 0!\n" (kappa_of_solution sol) ; flush stdout ;
  let rec f sol n acc = 
    if n = 0 then acc
    else f sol (n-1) (compose sol acc)
  in
    f sol n (empty())
      

let pushout cc1 cc2 =
  let rec match_ag hsh (id1,ag1) (id2,ag2) = 
    if not (Agent.name ag1 = Agent.name ag2) then false 
    else
      (*let intf2 = Agent.interface ag2 in*)
	try
	  Agent.fold_interface  
	    (fun site1 (inf1,lnk2) _ ->
			    if Agent.mem_interface site1 ag2 then 
			      let inf1,lnk1 = Agent.state ag1 site1 in
			      let inf2,lnk2 = Agent.state ag2 site1 in
				if (Agent.compatible_mark inf1 inf2) 
				  && (Agent.compatible_mark lnk1 lnk2) 
				then
				  try
				    let (i1,s1) = get_port (id1,site1) cc1 in
				    let (i2,s2) = get_port (id2,site1) cc2 in
				    let ag1' = agent_of_id i1 cc1
				    and ag2' = agent_of_id i2 cc2 in
				      if (s1 = s2) && (Agent.name ag1') = (Agent.name ag2') then
					if
					  Hashtbl.mem hsh (i1,i2) then () 
					else (
					  let _ = Hashtbl.add hsh (id1,id2) () in
					    if match_ag hsh (i1,ag1') (i2,ag2') then ()
					    else raise False
					)
				      else raise False
				  with
				      Not_found -> ()
				else raise False
			    else ()
			 ) ag1 () ; true
	with False -> false
  in
  let assoc_list = 
    List.fold_right (fun assoc assoc_list ->
		       AA.fold (fun id1 ag1 assoc_list ->
				  AA.fold (fun id2 ag2 assoc_list ->
					     if match_ag (Hashtbl.create 1) (id1,ag1) (id2,ag2) 
					     then (IntMap.add id1 id2 assoc)::assoc_list
					     else assoc_list
					  ) cc2.agents assoc_list
			       ) cc1.agents assoc_list
		    ) [IntMap.empty] []
  in
    if assoc_list = [] then None else (Some assoc_list)

let fuse_cc map = IntMap.fold (fun _ cc_i sol -> compose cc_i sol) map (empty())

let injections_product list_assoc_map = 
  let rec f = function
      [] -> [IntSet.empty,IntMap.empty]
    | assoc_map::tl -> 
	let tl_assoc_map = f tl in
	  List.fold_right (fun (ids,m) cont ->
			     (*IntMap.fold*) 
			     AssocArray.fold (fun _
						assoc cont ->
						  try 
						    let (ids',m') = 
						      IntMap.fold (fun id id' (ids,m) ->
								     if IntSet.mem id' ids then raise False
								     else (IntSet.add id' ids,
									   IntMap.add id id' m)
								  ) assoc (ids,m)
						    in
						      (ids',m')::cont
						  with False -> cont
					     ) assoc_map cont
			  ) tl_assoc_map []
  in
  let _,assoc_map = 
    List.fold_right (fun (ids,m) (n,cont) -> (n+1, IntMap.add n m cont)) (f list_assoc_map) (0,IntMap.empty)
  in
    assoc_map

let to_dot2 title sol_map = 
  let graph_label = "\""^title^"\"" in
  let hsh_done = Hashtbl.create 20 in
  let clusters,edges,fakes =
    let clusters,edges,fakes,_ =
      IntMap.fold (fun _ cc_i (clusters,edges,fake_sites,fresh) -> 
		     let clusters',edges,fake_sites,fresh=
		       AA.fold (fun id ag (clusters,edges,fake_sites,fresh) ->
				  let op_cluster = [Printf.sprintf "subgraph cluster%d{" id]
				  and node_style = ["node [style=filled,color=white];"]
				  and cluster_style = ["style=filled;"]
				  and cluster_color = ["color=lightgrey;"]
				  and cluster_label = ["label=\""^Agent.name ag^"#"^(string_of_int id)^"\""]
				  and cl_cluster = ["}"] 
				  in
				  let nodes,edges,fake_sites,fresh = 
				    Agent.fold_interface  
				      (fun site (inf,lnk) (nodes,edges,fake_sites,fresh) ->
					 if site = "_" && not (Agent.is_empty_intf ag) then
					   (nodes,edges,fake_sites,fresh)
					 else
					   let loc_site = site^(string_of_int id) in
					   let edge,fake_sites,fresh = 
					     match lnk with
						 Agent.Wildcard -> 
						   let fake = "fake"^(string_of_int fresh) in
						   let edge = 
						     Printf.sprintf
						       "\"%s\"->\"%s\" [dir=none,style=dotted]" loc_site fake
						   in
						     ([edge],fake::fake_sites,fresh+1)
					       | Agent.Free -> ([],fake_sites,fresh)
					       | Agent.Bound -> (
						   try 
						     let (id',site') = get_port (id,site) cc_i in
						       if (id' < id) && (Hashtbl.mem hsh_done (id',site',id,site))
						       then ([],fake_sites,fresh) 
						       else
							 if Hashtbl.mem hsh_done (id,site,id',site') 
							 then
							   ([],fake_sites,fresh) 
							 else
							   let loc_site' = site'^(string_of_int id') in
							   let edge = Printf.sprintf "%s->%s [dir=none]" 
							     loc_site loc_site'
							   in
							     if id<id' then 
							       Hashtbl.replace hsh_done (id,site,id',site') ()
							     else
							       Hashtbl.replace hsh_done (id',site',id,site) () ;
							     ([edge],fake_sites,fresh)
						   with Not_found ->
						     let fake = "fake"^(string_of_int fresh) in
						     let edge = Printf.sprintf
						       "\"%s\"->\"%s\" [dir=none]" loc_site fake 
						     in
						       ([edge],fake::fake_sites,fresh+1)
						 )
					       | _ -> invalid_arg "Rule.to_dot"
					   in
					   let node_label = [Printf.sprintf "label=\"%s\"" site]
					   and node_color = 
					     match inf with
						 Agent.Wildcard -> []
					       | Agent.Marked mrk -> 
						   [Printf.sprintf "color=\"%s\"" (color_of_mrk mrk)]
					       | _ -> invalid_arg "Rule.to_dot"
					   in
					   let node_opt = String.concat "," (node_label@node_color) in
					   let node = Printf.sprintf "%s [%s]" loc_site node_opt in
					     (node::nodes,edge@edges,fake_sites,fresh)
				      ) ag ([],edges,fake_sites,fresh)
				  in
				  let cluster_ag = 
				    op_cluster@node_style@cluster_style@cluster_color
				    @cluster_label@nodes@cl_cluster
				  in 
				    (cluster_ag@clusters,edges,fake_sites,fresh)
			       ) cc_i.agents (clusters,edges,fake_sites,fresh)
		     in
		       (clusters'@clusters,edges,fake_sites,fresh)
		  ) sol_map ([],[],[],0)
    in
      (clusters,edges,fakes)
  in
  let fake_clusters = 
    let fake_sites = List.map (fun fake_site -> Printf.sprintf "%s [label=\"\"]" fake_site) fakes
    in
    let l,_ = 
      List.fold_right (fun str (cont,cpt) -> 
			 let node = "node [style=filled,color=white];"
			 and style= "style=filled;"
			 and color= "color=lightgrey;"
			 and label = "label=\"\";"
			 in 
			 let opt = String.concat "\n" [node;style;color;label] in
			   ((Printf.sprintf "subgraph cluster_fake%d{%s %s}" cpt opt str)::cont,cpt+1)
		      ) fake_sites ([],0)
    in
      String.concat "\n" l
  in
    Printf.sprintf "digraph G {size=\"5,4\" ; \n label=%s \n %s \n %s %s\n}" 
      graph_label (String.concat "\n" clusters) fake_clusters (String.concat "\n" edges)

type observation = Concentration of (string * t) | Occurrence of string | Story of string

type marshalized_obs = FConcentration of (string * marshalized_t) | FOccurrence of string | FStory of string

let marshal_obs obs = match obs with
    Concentration (s,sol) -> FConcentration (s,marshal sol)
  | Occurrence s -> FOccurrence s
  | Story s -> FStory s

let unmarshal_obs obs = match obs with
    FConcentration (s,f_sol) -> Concentration (s,unmarshal f_sol)
  | FOccurrence s -> Occurrence s
  | FStory s -> Story s


let str_of_obs obs = 
  match obs with
      Concentration (s,sol) -> Printf.sprintf "%s:%s" s (kappa_of_solution sol)
    | Occurrence s -> s
    | Story s -> s

let sol_of_init no_mult init_list = 
  let sol = empty() in
  let sol =
    List.fold_left (fun acc (sol,n) -> 
		      let sol' = 
			if no_mult then sol
			else
			  multiply (copy sol) n 
		      in
		      let acc = compose sol' acc in (*compose folds on first argument*)
			acc
		   ) sol init_list 
  in
    add Agent.empty sol (*adding the NIL agent to the initial solution*)

let insert_empty_agent sol = (*size of the link map but just used for rules*)
  let shift sol = 
    let agents = AA.fold (fun id ag agents -> AA.add (id+1) ag agents) sol.agents (AA.create (AA.size sol.agents))
    and links = PA.fold (fun (id,s) (id',s') links -> PA.add (id+1,s) (id'+1,s') links) sol.links (PA.create (PA.size sol.links))
    and fresh_id = sol.fresh_id+1 
    in
      {agents=agents ; links = links ; fresh_id = fresh_id}
  in
  let sol = shift sol in
  let sol = 
    {sol with agents = AA.add 0 Agent.empty sol.agents} 
  in
    sol

let remove_empty_agents sol =
  {sol 
  with agents = 
    Tools2.generic_filter 
      (fun ag -> not (Agent.factice ag))
      sol.agents
      (AA.fold,
       AA.add,
       (AA.create 1))
  } 
