open Data_structures_metaplx 
open Agent_tree 

let rename_agent agent subs = 
 let cases = try AgentMap.find agent.agent_name  subs with Not_found -> [] in 
 let interface_list = List.map fst agent.interface in 
 let interface_set = List.fold_left (fun set a -> SiteSet.add a set) SiteSet.empty interface_list in 
 let cases = 
   List.filter 
     (fun a -> SiteSet.is_empty (SiteSet.inter interface_set a.forbidden_sites))
     cases in 
 List.fold_left 
   (fun sol case -> 
     let agent_name = case.target_name in 
     let interface_set = 
       List.fold_left 
	 (fun prefix_set ((site:site),image)  -> 
	   let (newsites:site list) = 
	     try SiteMap.find site case.substitutions
	     with Not_found -> [] in
	   let extended_prefix_set = 
	     List.fold_left 
	       (fun set prefix  -> 
		 List.fold_left 
		   (fun set (site:site)  -> 
		     (((site:site),
		       image)
		      ::prefix)
		     ::set)
		   set (newsites:site list))
	       [] prefix_set 
	   in 
	   extended_prefix_set)
	[[]] agent.interface  in 
     List.fold_left  
       (fun sol int -> {agent_name=agent_name;interface=int}::sol) 
       sol interface_set )
   [] cases 

let print_agent a = 
  print_string a.agent_name;
  print_string ":";
  List.iter 
    (fun s -> print_string (fst s);print_string ",")
    a.interface ;
  print_newline ()

