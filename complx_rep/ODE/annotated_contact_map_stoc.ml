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
  let fadd ag x y map = 
    let old = 
      try StringMap.find ag map 
      with Not_found -> StringMap.empty 
    in
    let oldx_image = 
      try StringMap.find x old
      with Not_found -> StringSet.empty in
    let newx_image = 
      StringSet.add y oldx_image in
    let oldy_image = 
      try StringMap.find y old
      with Not_found -> StringSet.empty in
    let newy_image = 
      StringSet.add x oldy_image in
    let new_map = 
	  StringMap.add x newx_image old in
    let new_map = 
      StringMap.add y newy_image new_map in 
    StringMap.add ag new_map  map 
  in
  let site_relation = 
    List.fold_left
      (fun relation rs -> 
	let sites_in_rule = 
	  let fadd a b sol = 
	     let old = 
	       try 
		 StringMap.find a sol 
	       with Not_found -> StringSet.empty 
	     in 
	     StringMap.add a (StringSet.add b old) sol 
	  in
	  let modif = 
	    List.fold_left 
	      (fun modif a -> 
		match a with
		    H _,_  -> modif
		  | Connected _,_ -> modif 
		  |  B(a,a',b),_ ->  fadd a b modif
		  | AL((a,a',b),_),_ -> fadd a' b modif
		  | L((a,a',b),(c,c',d)),_ -> fadd a' b (fadd c d modif)
		  | M((a,a',b),c),_ -> fadd a' b modif
		  | _ -> error 964 )
	       StringMap.empty  
	       rs.Pb_sig.control.Pb_sig.context_update
	   in 
	   let modif = 
	     List.fold_left 
	       (fun modif (a,b,c) -> fadd a c modif)
	       modif
	       rs.Pb_sig.control.Pb_sig.uncontext_update 
	   in 
	   let modif = 
	     List.fold_left 
	       (fun modif ((a,b,c),_) -> 
		  fadd a c modif)
	       modif rs.passive_species in
	     modif 
	in 
	   List.fold_left 
	     (fun relation rule -> 
		let sites_in_rule = 
		  List.fold_left 
		    (fun sol a -> 
		       let fadd x y sol = 
			 let old' = 
			   try StringMap.find x sol 
			   with 
			       Not_found -> StringSet.empty 
			 in
			   StringMap.add x 
			     (StringSet.add y old') sol in
		       match a with
			     H (x,x'),true -> 
			       (try 
				  (let _ = StringMap.find x' sol in sol)
				with 
				    Not_found -> 
				      (StringMap.add x' StringSet.empty sol))
			   | H _,_ -> sol
			   | Connected _,_ -> sol
			   | B(a,a',b),_  ->  fadd a' b sol
			   | AL((a,a',b),_),_-> fadd a' b sol
			   | L((a,a',b),(c,c',d)),_ -> fadd a' b (fadd c' d sol)
			   | M((a,a',b),c),_ -> fadd a' b sol
			   | _ -> error 964 )
		    sites_in_rule
		    rule.Pb_sig.injective_guard 
		in
		let relation = 
		  StringMap.fold
		    (fun a' modified rel -> 
		       StringSet.fold 
			 (fun test  rel -> 
			   StringSet.fold
			     (fun control rel -> 
			       if test=control then rel
			       else
				 fadd a' test control rel)
			    modified rel)
			 modified 
                         rel)
		    sites_in_rule 
		    relation 
		in 
		relation)
	     (relation)
	      rs.Pb_sig.rules)
	  StringMap.empty
	  system 
      in 
  let local_map = 
    StringMap.map 
      (fun lset -> 
	StringListSet.fold
	  (fun a b -> 
            {
              kept_sites=
                (List.fold_left (fun set b -> StringSet.add b set) StringSet.empty a)}::b)
	   lset [])
      classes 
  in 
  let _ = 
    if debug 
    then
      let _ = StringMap.iter 
	(fun a map -> 
	  let _ = print_string "AGENT " in
	  let _ = print_string a in 
	  let _ = print_newline () in 
	  let _ = 
	    StringMap.iter
	      (fun b s -> 
		let _ = print_string "map\n " in
		let _ = 
		  StringSet.iter 
		    (fun c -> 
		      print_string b;
		      print_string "->";
		      print_string c;
		      print_newline ())
		    s in ())
	      map in 
	  ())
        site_relation in () in 
  let local_map = 
    StringMap.fold 
      (fun a map local_map -> 
	let _ = 
	  if debug then 
	    let _ = print_string a in
	    let _ = 
	      StringMap.iter
		(fun b s -> 
		  let _ = print_string "map\n" in
		  let _ = 
		    StringSet.iter 
		      (fun c -> 
			print_string b;
			print_string "->";
			print_string c;
			print_newline ())
		      s in ())
		map 
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
		  (try StringMap.find t map
		   with Not_found -> StringSet.empty)
		  to_visit
	      in
	      aux to_visit rep in
	let covering_classes = 
	  StringMap.fold 
	    (fun a _ l -> 
	      ({kept_sites= aux [a] StringSet.empty})::l)
	    map [] in
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
  let singleton (a,b) = 
    let s = 
      begin 
        try 
          let l=StringMap.find a local_map in 
          let rec aux l = 
            match 
              l 
            with 
              | t::q -> if StringSet.mem b t.kept_sites then t.kept_sites else aux q
              | [] -> StringSet.empty
          in aux l 
        with 
          | Not_found -> StringSet.empty
      end
    in 
    StringSet.equal (StringSet.singleton b) s
      && 
      (try 
         match cpb.Pb_sig.cpb_mark_site 
         with 
           | None -> false
           | Some map -> 
         String2Map.find (a,b) map=[]
       with Not_found -> true)
  in 
  let _ = Printf.fprintf stdout "TEST SOLID_EDGE\n" in 
   let solid_edges = 
    String2Map.fold
      (fun (a,b) l  y -> 
        List.fold_right
          (fun (_,c,d) y -> 
            if (singleton (a,b) or singleton (c,d))
            then y 
            else 
              String22Set.add ((a,b),(c,d)) y)
          l y)
      contact_map.Pb_sig.link_of_site
      solid_edges
  in 
  let _ = 
    if debug 
    then 
      let _ = Printf.fprintf stdout "SOLID EDGES\n" in 
      let _ = 
        String22Set.iter 
          (fun ((a,b),(c,d)) -> 
            Printf.fprintf stdout "%s.%s <-> %s.%s\n" a b c d)
          solid_edges 
      in ()
  in 
  (local_map,solid_edges) 


(*let upgrade (x,y) cpb = 
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
*)  
