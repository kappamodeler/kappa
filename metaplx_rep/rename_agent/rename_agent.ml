open Data_structures_metaplx 
open Agent_tree 

let check_agent agent = 
  let interface_list = List.map fst agent.interface in 
  let rec scan l black = 
    match l with 
      t::q when SiteSet.mem t black -> false
    | t::q -> scan q (SiteSet.add t black)
    | [] -> true
  in
  scan interface_list SiteSet.empty  

let rename_agent agent subs = 
 let name = agent.agent_name in 
 try 
   begin 
     let cases = AgentMap.find name subs in 
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
	     (fun prefix_set (oldsite,image)  -> 
	       let newsites = 
		 try SiteMap.find oldsite case.substitutions
		 with Not_found -> [] in
	       let extended_prefix_set = 
		 List.fold_left 
		   (fun set (eti,prefix,siteset)  -> 
		     List.fold_left 
		       (fun set site  -> 
			 if SiteSet.mem site siteset then set
			 else
			   ((oldsite,site)::eti,
			    (site,image)::prefix,
			    SiteSet.add site siteset)
			 ::set)
		       set (newsites:site list))
		   [] prefix_set 
	       in 
	       extended_prefix_set)
	     [[name,agent_name],[],SiteSet.empty] agent.interface  in 
	 List.fold_left  
	   (fun sol (annotation,int,_) -> (List.rev annotation,{agent_name=agent_name;interface=List.rev int})::sol) 
	   sol interface_set )
       [] cases 
   end
     with Not_found -> 
       if check_agent agent then [[],agent] else [] 



