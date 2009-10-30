open Data_structures
open Pb_sig 
open Tools 
open Output_contact_map 
open Ode_print_sig
open Ode_print 
open Error_handler 
open Annotated_contact_map 

let debug = false



let error i = 
  unsafe_frozen None (Some "Complx") (Some "Annotated_contact_map.ml") None (Some ("line "^(string_of_int i))) (fun () -> raise Exit)


let keep_this_link (a,b) (c,d) skeleton = 
  (*a<>c 
    && *)
  (String22Set.mem ((a,b),(c,d))  skeleton.solid_edges 
   or 
   String22Set.mem ((c,d),(a,b)) skeleton.solid_edges)


let dump_template print  x =
  let _ = pprint_string print "Passing sites: \n" in
  let _ = 
    String22Set.iter
      (fun ((a,b),(c,d)) -> 
	pprint_string print  a;
	pprint_string print  ".";
	pprint_string print  b;
	pprint_string print  "--";
	pprint_string print  d;
	pprint_string print  ".";
	pprint_string print  c;
	pprint_newline print)
      x.solid_edges in 
  let _ = pprint_string print  "\nTemplates: \n\n" in
  let _ = 
    StringMap.iter 
      (fun x l -> 
	let _ = pprint_string print  "Agent: " in
	let _ = pprint_string print   x in
	let _ = pprint_newline print in
	let _ = 
	  List.iter 
	    (fun x -> 
	        let _ = pprint_string print   "Template\nSites\n" in
		let _ = 
		  StringSet.iter 
		    (fun x -> 
		      pprint_string print   x;
		      pprint_string print  ",")
		    x.kept_sites in
		let _ = pprint_newline print in 
		let _ = pprint_newline print in
		let _ = pprint_newline print in 
		())
	    l in
	let _ = pprint_newline print   
	in ())
      x.subviews 
  in ()

let sub_template a b = StringSet.subset a.kept_sites b.kept_sites 

let normalize_site_list l = List.sort compare l 


let get_sitesets_of_solution t agentmap = 
  Solution.AA.fold 
    (fun _ a agentmap -> 
       let interface = 
	 Agent.fold_interface 
	 (fun s (m1,m2) int ->
	    match s,m1,m2 with "_",_,_ | _,Agent.Wildcard,Agent.Wildcard -> int
	      | _ -> s::int)
	 a []
       in
       let interface = normalize_site_list interface in 
       let agent_name = Agent.name a in 
       let old = 
	 try 
	   StringMap.find agent_name agentmap 
	 with 
	     Not_found -> 
	       StringListSet.empty 
       in 
	 StringMap.add agent_name (StringListSet.add interface old) 
	   agentmap)
    t.Solution.agents  agentmap

let get_links_of_solution t solid_edges = 
  let speciemap = 
    Solution.AA.fold 
      (fun i a  -> IntMap.add i (Agent.name a))  
      t.Solution.agents 
      IntMap.empty in 
    Solution.PA.fold 
      (fun (i,s) (i',s') solid_edges -> 
	 let a = IntMap.find i speciemap in 
	 let a' = IntMap.find i' speciemap in 
	 String22Set.add ((a,s),(a',s'))
	   (String22Set.add ((a',s'),(a,s))
	      solid_edges))
      t.Solution.links 
      solid_edges

	 
let compute_annotated_contact_map_init cpb  = 
  List.fold_left 
    (fun (local_map,site_map) (a,b,c) -> 
      let set = 
	List.fold_left
	  (fun set a -> StringSet.add a set)
	  (List.fold_left 
	     (fun set a -> StringSet.add a set)
	     StringSet.empty 
	     b)
	  c in
      let list,list2 = 
	StringSet.fold 
	  (fun x (l,l2) -> 
	    {kept_sites = StringSet.singleton x}::l,x::l2)
	  set
	  ([{kept_sites = StringSet.empty}],[])
      in
      StringMap.add a list local_map,
      StringMap.add a list2 site_map)
    (StringMap.empty,StringMap.empty)
    cpb.Pb_sig.cpb_interface 
  
  
let compute_annotated_contact_map_in_approximated_mode system cpb contact_map  =
  let local_map,_ = compute_annotated_contact_map_init cpb in 
    
  List.fold_left  
    (fun (sol,passing_sites) rs ->
      let passive = rs.Pb_sig.passive_species in 
      let passing_sites = 
	List.fold_left 
	  (fun sol ((a,b,c),(d,e,f)) ->
	    let fadd x y  sol = 
	      if 
		List.exists 
		  (fun a -> 
		    (List.exists 
		       (fun b -> 
			 match b with 
			   M((a',b',_),_),_ when x=(a',b') -> true
			 | B(a',b',c'),_ | AL((a',b',c'),_),_ when x=(a',b') && c'<>y -> true 
			 | L((a',b',c'),_),_ when x=(a',b') && c'<>y -> true 
			 | L(_,(a',b',c')),_  when x=(a',b') && c'<>y -> true 
			 | _ -> false
			       ) a.Pb_sig.injective_guard)) rs.Pb_sig.rules 
	      then 
		String2Set.add (snd x,y) sol
	      else
		sol
	    in
	    
	    fadd (a,b) c (fadd (d,e) f passing_sites))
	  passing_sites passive in
      let local_map  = 
	List.fold_left 
	  (fun sol a ->
	    List.fold_left 
	      (fun sol a -> 
		
		let fadd x y sol = 
		  let old' = 
		    try String2Map.find x sol 
		    with 
		      Not_found -> StringSet.empty 
		  in
		  String2Map.add x 
		    (StringSet.add y old') sol in
		match a with
		  H (x,x'),true -> 
		    (try 
		      (let _ = String2Map.find (x,x') sol in sol)
		    with 
		      Not_found -> 
			(String2Map.add (x,x') StringSet.empty sol))
		| H _,_ -> sol
		| Connected _,_ -> sol 
		| B(a,a',b),_ ->  fadd (a,a') b sol
		| AL((a,a',b),_),_ -> fadd (a,a') b sol
		| L((a,a',b),(c,c',d)),_ -> fadd (a,a') b (fadd (c,c') d sol)
		| M((a,a',b),c),_ -> fadd (a,a') b sol
		| _ -> error 923 )
	      sol a.Pb_sig.injective_guard)
	  String2Map.empty rs.Pb_sig.rules
      in  
      String2Map.fold
	(fun (_,a) b sol ->
	  let old = 
	    try StringMap.find a sol 
	    with Not_found -> [] 
	  in
	  StringMap.add a ({kept_sites=b}::old) sol)
	local_map sol,passing_sites 
	)  
    (local_map,String2Set.empty)
    system

let trivial_rule rs = 
  let control = rs.Pb_sig.control in 
  let context = control.Pb_sig.context_update in 
  let uncontext = control.Pb_sig.uncontext_update in 
  match context,uncontext with 
    _,[a,b,c] -> 
      begin
	(List.for_all 
	  (fun rule -> 
	    List.for_all 
	      (fun (bb,bool) -> 
		match bb,bool  with 
		  B(x,y,z),true when x=a && y=b && z=c -> true 
		| H(_),_ -> true 
		| _ -> false
		      )
	      rule.Pb_sig.injective_guard
	      )
	   rs.Pb_sig.rules )
	  && 
	(List.for_all 
	   (fun (bb,bool) -> 
	     match (bb,bool) with 
	       B(x,y,z),false when x=a && y=b && z=c -> true 
	     | _ -> false)
	   context)
      end
  | _,[] ->
      begin
	let rec aux context rep = 
	  match context with 
	    (AL(_),_)::q | (B(_),_)::q-> aux q rep
	    | (L((a,b,c),(d,e,f)),_)::q  -> 
		begin
		  match rep with 
		    None -> aux q (Some((a,b,c),(d,e,f)))
		  | Some _ -> false,None
		end
	    | [] -> 
		begin
		  match rep with 
		    None -> false,None
		  | x -> true,x 
		end
	    | _ -> false,None
	  in 
	let b,binding = aux context None in 
	if b then 
	  match binding with 
	    None -> error 958
	  | Some ((a,b,c),(d,e,f)) -> 
	      List.for_all 
		(fun rule -> 
		  List.for_all 
		    (fun (bb,bool) -> 
		      match bb,bool  with 
			B(x,y,z),_ when (x=a && y=b && z=c) or (x=d && y=e && z=f) -> true 
		      |	AL((x,y,z),(xx,yy)),_ when (x=a && y=b && z=c && xx=e && yy=f) or (x=d && y=e && z=f && xx=b & yy=c) -> true 
		      |	L((x,y,z),(xx,yy,zz)),_ when (x=a && y=b && z=c && xx=d && yy=e && zz=f) or (xx=a && yy=b && zz=c && x=d && y=e && z=f) -> true 
		      | H(x,y),_ when x=a or x = d -> true 
		      | _ -> false
			    )
		    rule.Pb_sig.injective_guard
		    )
		rs.Pb_sig.rules
	else false
      end
  |	_ -> false 

type trstoc = Half of string*string | Unbind of string*string*string*string  | Bind of string*string*string*string | Remove of string 
	


let is_trivial_deletion rs = 
  (List.length rs.Pb_sig.control.Pb_sig.remove = 1)
  && 
    (List.for_all 
       (fun rule -> 
	  List.length rule.Pb_sig.injective_guard = 1)
       rs.Pb_sig.rules)
  && 
    (rs.Pb_sig.control.Pb_sig.add = [] )
    
let is_trivial_rule_stoc  rs = (trivial_rule  rs) or is_trivial_deletion rs 


    
let compute_annotated_contact_map_in_stoc_mode system cpb contact_map  =
  let local_map,site_map  = compute_annotated_contact_map_init cpb in
  let classes = 
    StringMap.map 
      (fun l -> 
	 List.fold_left 
	   (fun sol a -> StringListSet.add 
	      (StringSet.elements a.kept_sites)
	      sol)
	   StringListSet.empty l)
      local_map
  in 
  let system = List.filter (fun x -> not (is_trivial_rule_stoc x)) system in 
  let fadd ag x y map = 
    let old1,old2 = 
      try StringMap.find ag map 
      with Not_found -> (StringMap.empty,StringMap.empty) in
    let old1_image = 
	  try StringMap.find x old1 
	  with Not_found -> StringSet.empty in
	let new1_image = 
	  StringSet.add y old1_image in
	let old2_image = 
	  try StringMap.find y old2 
	  with Not_found -> StringSet.empty in
	let new2_image = 
	  StringSet.add x old2_image in
	let new1_map = 
	  StringMap.add x new1_image old1 in
	let new2_map = 
	  StringMap.add y new2_image old2 in 
	StringMap.add ag (new1_map,new2_map)  map 
  in
  
  let site_relation,classes,dangerous_sites = 
    List.fold_left
      (fun (relation,classes,dangerous_sites)  rs -> 
	 let modified_sites = 
	   let fadd a b sol = 
	     let old = 
		   try 
		     String2Map.find a sol 
		   with Not_found -> StringSet.empty 
	     in 
	       String2Map.add a (StringSet.add b old) sol 
	   in
	   let modif = 
	     List.fold_left 
	       (fun modif a -> 
		  match a with
		      H _,_  -> modif
		    | Connected _,_ -> modif 
		    |  B(a,a',b),_ ->  fadd (a,a') b modif
		    | AL((a,a',b),_),_ -> fadd (a,a') b modif
		    | L((a,a',b),(c,c',d)),_ -> fadd (a,a') b (fadd (c,c') d modif)
			| M((a,a',b),c),_ -> fadd (a,a') b modif
			| _ -> error 964 )
	       String2Map.empty  
	       rs.Pb_sig.control.Pb_sig.context_update
	   in 
	   let modif = 
	     List.fold_left 
	       (fun modif (a,b,c) -> fadd (a,b) c modif)
	       modif
	       rs.Pb_sig.control.Pb_sig.uncontext_update 
	   in 
	   let modif = 
	     List.fold_left 
	       (fun modif ((a,b,c),_) -> 
		  fadd (a,b) c modif)
	       modif rs.passive_species in
	     modif 
	 in 
	   List.fold_left 
	     (fun (relation,classes,dangerous_sites) rule -> 
		let tested_sites,free_sites = 
		  List.fold_left 
		    (fun (sol1,sol2) a -> 
		       let fadd x y sol = 
			 let old' = 
			   try String2Map.find x sol 
			   with 
			       Not_found -> StringSet.empty 
			 in
			   String2Map.add x 
			     (StringSet.add y old') sol in
			 match a with
			     H (x,x'),true -> 
			       (try 
				  (let _ = String2Map.find (x,x') sol1 in sol1,sol2)
				with 
				    Not_found -> 
				      (String2Map.add (x,x') StringSet.empty sol1,sol2))
			   | H _,_ -> sol1,sol2
			   | Connected _,_ -> sol1,sol2 
			   | B(a,a',b),false  ->  fadd (a,a') b sol1,fadd (a,a') b sol2
			   | B(a,a',b),_ -> fadd (a,a') b sol1,sol2
			   | AL((a,a',b),_),true -> fadd (a,a') b sol1,fadd (a,a') b sol2
			   | AL((a,a',b),_),_ -> fadd (a,a') b sol1,sol2
			   |	L((a,a',b),(c,c',d)),true -> fadd (a,a') b (fadd (c,c') d sol1),fadd (a,a') b (fadd (c,c') d sol2)
			   | L((a,a',b),(c,c',d)),_ -> fadd (a,a') b (fadd (c,c') d sol1),sol2
			   | M((a,a',b),c),_ -> fadd (a,a') b sol1,sol2
			   | _ -> error 964 )
		    (String2Map.empty,String2Map.empty)  
		    rule.Pb_sig.injective_guard 
		in
		let classes = 
		  String2Map.fold
		    (fun (_,a) set classes -> 
		       let tested_list = StringSet.elements set in 
		       let old = 
			 try 
			   StringMap.find a classes 
			 with 
			     Not_found -> StringListSet.empty 
		       in 
			 StringMap.add a (StringListSet.add tested_list old) classes 
		    )
		    tested_sites classes in  
		let relation,dangerous_sites = 
		      List.fold_left 
			(fun (relation,dangerous_sites) a_id -> 
			   let a = 
			     try 
			       StringMap.find 
				 a_id 
				 rs.Pb_sig.specie_of_id 
			     with 
				 Not_found -> error 394 
			   in 
			   let tested = 
			     try String2Map.find (a_id,a) tested_sites
			     with Not_found -> StringSet.empty in 
			   let relation = 
			     StringSet.fold
			       (fun site relation -> 
				  StringSet.fold 
				    (fun site' relation -> 
				       if site==site' 
				       then relation
				       else
					 (fadd a site site' relation))
				    (  try 
					 List.fold_left 
					   (fun sol s -> StringSet.add s sol)
					   (StringSet.empty)
					   (StringMap.find a site_map)
				       with 
					   Not_found -> StringSet.empty )
				    relation)
			       tested relation 
			   in 
			   let dangerous_sites = 
			     let free_sites = 
			       try 
				 String2Map.find (a_id,a) free_sites
			       with 
				   Not_found -> StringSet.empty 
			     in 
			     let sites = 
			       try 
				 List.fold_left 
				   (fun sol s -> StringSet.add s sol)
				   (StringSet.empty)
				   (StringMap.find a site_map)
			       with 
				   Not_found -> StringSet.empty 
			     in 
			     let diff = StringSet.diff sites free_sites in 
			       StringSet.fold 
				 (fun s -> String2Set.add (a,s)) diff dangerous_sites
			   in 
			     
			     relation,dangerous_sites)
		    (relation,dangerous_sites)
		    rs.Pb_sig.control.Pb_sig.remove 
		in

		let relation = 
		  String2Map.fold2
		    (fun _ _ x -> x)
		    (fun _ _ x -> x)
		    (fun (a,a') tested modified rel -> 
                       let tested=StringSet.union tested modified in 
                       let modified=StringSet.union tested modified in 
		      StringSet.fold 
			(fun test  rel -> 
			  StringSet.fold
			    (fun control rel -> 
			      if test=control then rel
			      else
				fadd a' test control rel)
			    modified rel)
			tested rel)
		    tested_sites
		    modified_sites 
		    relation 
		in 
		relation,classes,dangerous_sites)
	      (relation,classes,dangerous_sites)
	      rs.Pb_sig.rules)
	  (StringMap.empty,classes,String2Set.empty)
	  system 
      in 
  let local_map = 
    StringMap.map 
      (fun lset -> 
	 StringListSet.fold
	   (fun a b -> {kept_sites=(List.fold_left (fun set b -> StringSet.add b set) StringSet.empty a)}::b)
	   lset [])
      classes 
  in 
	   
	 
(*      let classes = 
	List.fold_left
	  (fun classes obs -> get_sitesets_of_solution obs classes)
	  classes obs 
      in *)
      let _ = 
	if debug 
	then
	  let _ = StringMap.iter 
	      (fun a (map1,map2) -> 
		let _ = print_string "AGENT " in
		let _ = print_string a in 
		let _ = print_newline () in 
		let _ = 
		  StringMap.iter
		    (fun b s -> 
		      let _ = print_string "map1\n " in
		      let _ = 
			StringSet.iter 
			  (fun c -> 
			    print_string b;
			    print_string "->";
			    print_string c;
			    print_newline ())
			  s in ())
		    map1 in 
		let _ = 
		  StringMap.iter
		    (fun b s -> 
		      let _ = print_string "map2\n" in
		      let _ = 
			StringSet.iter 
			  (fun c -> 
			    print_string b;
				  print_string "->";
			    print_string c;
			    print_newline ())
			  s in ())
		    map2 in ())
	      site_relation in () in 
	
    
      let local_map = 
	StringMap.fold 
	  (fun a (map1,map2) local_map -> 
	     let _ = 
	       if debug then 
		    let _ = print_string a in
		    let _ = 
		      StringMap.iter
			(fun b s -> 
			  let _ = print_string "map2\n" in
			  let _ = 
			    StringSet.iter 
			      (fun c -> 
				print_string b;
				print_string "->";
				print_string c;
				print_newline ())
			      s in ())
			map2 
		    in 
		    let _ = print_newline () in () 
		in 
	         let rec aux to_visit rep  =
		   match to_visit with 
		       [] -> rep
		     |  t::q when StringSet.mem t rep -> aux q rep
		     |  t::q -> 
			  let rep = StringSet.add t rep in
			  let to_visit = 
			    StringSet.fold 
			      (fun a l -> a::l)
			      (try StringMap.find t map2
			       with Not_found -> StringSet.empty)
			      to_visit
			  in
			    aux to_visit rep in
		 let covering_classes = 
		  StringMap.fold 
		    (fun a _ l -> 
		      ({kept_sites= aux [a] StringSet.empty})::l)
	            map2 [] in
		 let l2 = StringMap.find a classes in 
		 let covering_classes = 
		   StringListSet.fold 
		     (fun a l  -> ({kept_sites = aux a StringSet.empty})::l)
		     l2 
		     covering_classes 
		 in
		let old = 
		  try StringMap.find a local_map 
		  with Not_found -> [] in 
		StringMap.add a (covering_classes@old) local_map)
	      site_relation
	      local_map 
      in
      let solid_edges = String22Set.empty in 
(*      let solid_edges = 
	List.fold_left 
	  (fun solid_edges obs -> get_links_of_solution obs solid_edges)
	  solid_edges 
	  obs
      in 
  *)
      let solid_edges = 
	List.fold_left 
	  (fun solid_edges rs -> 
	    if trivial_rule rs 
	    then solid_edges 
	    else 
              let guard = rs.Pb_sig.rules in 
              let solid_edges =
                List.fold_left 
                  (fun solid_edges guard -> 
                     List.fold_left 
                       (fun solid_edges x -> 
                          match x 
                          with 
                              L((_,b,c),(_,e,f)),_
                            |AL((_,b,c),(e,f)),true 
                                -> String22Set.add ((b,c),(e,f)) solid_edges
                            | _ -> solid_edges)
                       solid_edges guard.injective_guard)
                  solid_edges guard
              in 
	      let solid_edges = 
		List.fold_left 
		  (fun solid_edges b -> 
		    match b with 
		      L((a,b,c),(d,e,f)),_ -> 
			String22Set.add ((b,c),(e,f))
			  (String22Set.add ((e,f),(b,c)) solid_edges)
		    | _ -> solid_edges)
		  solid_edges 
		  rs.Pb_sig.control.Pb_sig.context_update 
	      in 
	      let solid_edges = 
		List.fold_left 
		  (fun solid_edges (a,b,c) -> 
		    List.fold_left 
                      (fun solid_edges (_,d,e) -> 
			String22Set.add ((b,c),(d,e))
			  (String22Set.add ((d,e),(b,c)) solid_edges))
		      solid_edges 
		      (contact_map (b,c)))
		  solid_edges 
		  rs.Pb_sig.control.Pb_sig.uncontext_update 
	      in 
	      let solid_edges = 
		String2Set.fold 
		  (fun (b,c) solid_edges -> 
		    List.fold_left 
                      (fun solid_edges (_,d,e) -> 
			String22Set.add ((b,c),(d,e))
			  (String22Set.add ((d,e),(b,c)) solid_edges))
		      solid_edges 
		      (contact_map (b,c)))
		  dangerous_sites 
		  solid_edges  
	      in 
	      solid_edges)
	  
	  solid_edges system 
      in 
      (local_map,solid_edges) 


let compute_annotated_contact_map_in_flat_mode system cpb contact_map  = 
  let passing_sites = 
    List.fold_left 
      (fun sol (a,b,c) ->
	List.fold_left 
	  (fun sol x -> String2Set.add (a,x) sol)
	  sol c)
      String2Set.empty
      cpb.cpb_interface in 
  let local_views = 
    List.fold_left 
      (fun sol (a,b,c) -> 
	let site_set = 
	  List.fold_left
	    (fun sol x -> StringSet.add x sol)
	    (List.fold_left
	       (fun sol x -> StringSet.add x sol)
	       StringSet.empty 
	       c)
	    b
	in 
	StringMap.add a [{kept_sites=site_set}] sol)
      StringMap.empty 
      cpb.cpb_interface in
  (local_views,passing_sites) 

let upgrade (x,y) cpb = 
  (x,
   String2Map.fold
     (fun (a,b) l sol -> 
       List.fold_left
	 (fun sol (c,d) -> 
	   if String2Set.mem (a,b) y
	       && String2Set.mem (c,d) y 
	   then 
	     String22Set.add ((a,b),(c,d)) sol
	   else 
	     sol) 
	 sol l) 
     (match cpb.cpb_contact 
     with None -> error 1327
     | Some a -> a) 
     String22Set.empty )
    
