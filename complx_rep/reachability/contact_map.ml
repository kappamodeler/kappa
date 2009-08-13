open Data_structures 
open Pb_sig 
open Tools 
open Error_handler 

let error i x t y = 
    unsafe
      (Some x) 
      (Some "Complx")
      (Some "contact_map.ml") 
      (Some t) 
      (Some i) 
      y

let error_frozen i x t y = 
    unsafe_frozen
      (Some x) 
      (Some "Complx")
      (Some "contact_map.ml") 
      (Some t) 
      (Some i)
      y


let contact_map_init = 
  {link_of_site=String2Map.empty;
   possible_linksb=[];
   relation_set = String22Set.empty;
   relation_list = [];
   access = String2Map.empty;
   live_agents = StringSet.empty}
  
let oriente (a1,s1) (a2,s2) = 
  match (l((a1,a1,s1),(a2,a2,s2)))
  with L((a,_,a'),(b,_,b')) -> ((a,a'),(b,b'))
  | _ -> failwith "oriente_in_contact_map.ml"



let add_contact with_dots (a1,s1) (a2,s2) cm = 
  let fadd_access k1 k2 acc = 
    let fadd k1 k2 acc = 
      let old = try String2Map.find k1 acc with Not_found -> String2Set.empty in
      String2Map.add k1 (String2Set.add k2 old) acc in 
    fadd k1 k2 (fadd k2 k1 acc) in 
   let fadd_contact k1 k2 acc = 
    let fadd k1 (k2,s2) acc = 
      let old = try String2Map.find k1 acc with Not_found -> [] in
      String2Map.add k1 ((k2,k2,s2)::old) acc in 
    if k1=k2 then 
      fadd k1 k2 acc 
    else
      fadd k1 k2 (fadd k2 k1 acc) 
   in 
  let k = oriente (a1,s1) (a2,s2) in 
  let (k1,k2),(k3,k4)=k in
  if String22Set.mem k cm.relation_set 
  then cm
  else 
    { relation_set = String22Set.add k cm.relation_set;
      live_agents = StringSet.add a1 (StringSet.add a2 cm.live_agents);
      relation_list = k::(cm.relation_list);
      possible_linksb = (L((k1,k1,k2),(k3,k3,k4)))::cm.possible_linksb;
      link_of_site = fadd_contact  (a1,s1) (a2,s2) cm.link_of_site;
      access = 
      if with_dots 
      then 
	let set1 = String2Set.add (a1,s1) (try String2Map.find (a1,s1) cm.access with Not_found -> String2Set.empty) in 
	let set2 = String2Set.add (a2,s2) (try String2Map.find (a2,s2) cm.access with Not_found -> String2Set.empty) in 
	String2Set.fold 
	  (fun k1 acc -> 
	    String2Set.fold (fun k2 acc -> fadd_access k1 k2 acc) set1 acc)
	  set2 cm.access 
      else cm.access} 

let are_connected (id1,ig1) (id2,ig2) cm sites_of_ig1 = 
  List.fold_left 
    (fun sol s1 -> 
      String2Set.fold 
	(fun (a,b) (sol1,sol2) -> 
	  if a=ig2 then (id1,s1)::sol1,(id2,b)::sol2
	  else sol1,sol2)
	(try (String2Map.find (ig1,s1) cm.access)
	with Not_found -> String2Set.empty)
	sol)
    ([],[])
    sites_of_ig1
	  


let empty_drawer = 
  {
    tested_agent_to_rules = StringMap.empty;
    tested_sites_to_rules = String2Map.empty;
    tested_edges_to_rules = String22Map.empty;
    mod_agent_to_rules = StringMap.empty;
    mod_sites_to_rules = String2Map.empty;
    mod_edges_to_rules = String22Map.empty;
} 

let build_drawer p rules contact = 
  let fadd_mod_agents_to_rules drawer agent r_list  = 
     let old = 
       try 
	 StringMap.find agent drawer.mod_agent_to_rules 
       with 
	 Not_found -> 
	   IntSet.empty in
     let image = 
       List.fold_left 
	 (fun set  (a,_,_) -> 
	   List.fold_left 
	     (fun set  r_id -> 
	       if p r_id then 
		 IntSet.add r_id.r_simplx.Rule.id set
	       else set)
	     set a)
	 old r_list.cpb_guard in 
     {drawer with mod_agent_to_rules = StringMap.add agent image drawer.mod_agent_to_rules} 
  in 
  let fadd_mod_sites_to_rules drawer site r_list = 
      let old = 
       try 
	 String2Map.find site drawer.mod_sites_to_rules
       with 
	 Not_found -> 
	   IntSet.empty in
     let image = 
       List.fold_left 
	 (fun set  (a,_,_) -> 
	   List.fold_left 
	     (fun set  r_id -> 
	       if p r_id then 
		 IntSet.add r_id.r_simplx.Rule.id set
	       else set)
	     set a)
	 old r_list.cpb_guard in 
     {drawer with mod_sites_to_rules  = String2Map.add site image drawer.mod_sites_to_rules} 
  in 
  let fadd_mod_edges_to_rules drawer edge r_list = 
    let edge = 
      let ((a,b),(c,d)) = edge in 
      if compare (a,b) (c,d) < 0 then 
	edge 
      else 
	((c,d),(a,b)) in 
    let old = 
       try 
	 String22Map.find edge drawer.mod_edges_to_rules
       with 
	 Not_found -> 
	   IntSet.empty in
     let image = 
       List.fold_left 
	 (fun set  (a,_,_) -> 
	   List.fold_left 
	     (fun set  r_id -> 
	       if p r_id then 
		 IntSet.add r_id.r_simplx.Rule.id set
	       else set)
	     set a)
	 old r_list.cpb_guard in 
     {drawer with mod_edges_to_rules  = String22Map.add edge image drawer.mod_edges_to_rules} 
  in 
  let fadd_tested_agents_to_rules drawer agent r_list  = 
     let old = 
       try 
	 StringMap.find agent drawer.tested_agent_to_rules 
       with 
	 Not_found -> 
	   IntSet.empty in
     let image = 
       List.fold_left 
	 (fun set  (a,_,_) -> 
	   List.fold_left 
	     (fun set  r_id -> 
	       if p r_id then 
		 IntSet.add r_id.r_simplx.Rule.id set
	       else set)
	     set a)
	 old r_list.cpb_guard in 
     {drawer with tested_agent_to_rules = StringMap.add agent image drawer.tested_agent_to_rules} 
  in 
  let fadd_tested_sites_to_rules drawer site r_list = 
      let old = 
       try 
	 String2Map.find site drawer.tested_sites_to_rules
       with 
	 Not_found -> 
	   IntSet.empty in
     let image = 
       List.fold_left 
	 (fun set  (a,_,_) -> 
	   List.fold_left 
	     (fun set  r_id -> 
	       if p r_id then 
		 IntSet.add r_id.r_simplx.Rule.id set
	       else set)
	     set a)
	 old r_list.cpb_guard in 
     {drawer with tested_sites_to_rules  = String2Map.add site image drawer.tested_sites_to_rules} 
  in 
  let fadd_tested_edges_to_rules drawer edge r_list = 
    let edge = 
      let ((a,b),(c,d)) = edge in 
      if compare (a,b) (c,d) < 0 then 
	edge 
      else 
	((c,d),(a,b)) in 
    let old = 
       try 
	 String22Map.find edge drawer.tested_edges_to_rules
       with 
	 Not_found -> 
	   IntSet.empty in
     let image = 
       List.fold_left 
	 (fun set  (a,_,_) -> 
	   List.fold_left 
	     (fun set  r_id -> 
	       if p r_id then 
		 IntSet.add r_id.r_simplx.Rule.id set
	       else set)
	     set a)
	 old r_list.cpb_guard in 
     {drawer with tested_edges_to_rules  = String22Map.add edge image drawer.tested_edges_to_rules} 
  in 
  
  List.fold_left 
    (fun drawer  r -> 
      let control = r.cpb_control in
      let test = 
	match r.cpb_guard 
	with 
	    [_,_,a] -> a 
	  | _ -> error "line : 231" "illegal cpb (some rules have been  gathered)" "build_drawer" [] 
      in 
      let id = 
	List.fold_left 
	  (fun map (i,a) -> IntMap.add i a map) 
	  IntMap.empty 
	  r.cpb_r_species 
      in
      let get_id i = 
	try 
	  IntMap.find i id
	with 
	  Not_found -> error "line : 98" "get_id has failed" "build_drawer" "" in
      let drawer = 
	IntSet.fold 
	  (fun i drawer -> 
	    let ag = get_id i in 
	    fadd_mod_agents_to_rules drawer ag r)
	  control.cpb_create drawer in 
      let drawer =
	IntSet.fold 
	  (fun i drawer -> 
	    let ag = get_id i in 
	    fadd_mod_agents_to_rules drawer ag r)
	  control.cpb_remove drawer in 
      let drawer = 
	List.fold_left 
	  (fun drawer action -> 
	    match action with 
	      No_Pol | No_Helix -> drawer 
	    | Bind ((i,s),(i',s')) -> 
		let ag = get_id i in 
		let ag' = get_id i' in 
		fadd_mod_edges_to_rules drawer ((ag,s),(ag',s')) r 
	    | Mark ((i,s),m) -> 
		let ag = get_id i in
		fadd_mod_sites_to_rules drawer (ag,s) r
	    | Release ((i,s),(i',s')) -> 
		let ag = get_id i in
		let ag'= get_id i' in 
		fadd_mod_edges_to_rules drawer ((ag,s),(ag',s')) r 
	    | Break_half (i,s) -> 
		let ag = get_id i in 
		List.fold_left 
		  (fun drawer (ag',s') ->
		    fadd_mod_edges_to_rules drawer ((ag,s),(ag',s')) r)
		  drawer
		  (try String2Map.find (ag,s) contact
		  with 
		     Not_found -> [])
	    | Check_choice _ | Check _ -> drawer 
	    )
	  drawer
	  control.cpb_update in
      let drawer = 
	List.fold_left 
	  (fun drawer test -> 
	     match test with 
		 Is_here i ->
		   let ag = get_id i in 
		     fadd_tested_agents_to_rules drawer ag r
	       |  Is_forbid _ | Is_disjoint _ | Is_connected _ -> drawer  
	       | Is_marked ((i,s),_) -> 
		   let ag = get_id i in 
		     fadd_tested_sites_to_rules drawer (ag,s) r 
	       | Is_bound (i,s) | Is_free (i,s) ->
		   let ag = get_id i in 
		     List.fold_left 
		       (fun drawer (ag',s') ->
			    fadd_tested_edges_to_rules drawer ((ag,s),(ag',s')) r)
		       drawer
		       (try String2Map.find (ag,s) contact
			with 
			    Not_found -> [])
	       | Is_related ((i,s),(i',s')) -> 
		   let ag = get_id i in 
		   let ag' = get_id i' in
		     fadd_tested_edges_to_rules drawer ((ag,s),(ag',s')) r 
	  )
	  drawer 
	  test
      in drawer 
    )
    empty_drawer 
    rules 
	  
	
