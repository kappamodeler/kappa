open Data_structures
open Pb_sig 
open Tools 
open Output_contact_map 
open Ode_print_sig
open Ode_print 
open Error_handler 

let debug = false

type template_piece = {kept_sites:StringSet.t}

type ode_skeleton = 
    {subviews:(template_piece list) StringMap.t;
      solid_edges: String22Set.t }



type compression_mode = Compressed | Flat | Approximated 


let error i = 
  unsafe_frozen None (Some "Annotated_contact_map.ml") None (Some ("line "^(string_of_int i))) (fun () -> raise Exit)


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
  
let compute_annotated_contact_map_init cpb  = 
  List.fold_left 
    (fun local_map (a,b,c) -> 
      let set = 
	List.fold_left
	  (fun set a -> StringSet.add a set)
	  (List.fold_left 
	     (fun set a -> StringSet.add a set)
	     StringSet.empty 
	     b)
	  c in
      let list = 
	StringSet.fold 
	  (fun x l -> 
	    {kept_sites = StringSet.singleton x}::l)
	  set
	  [{kept_sites = StringSet.empty}] in
      StringMap.add a list local_map)
    StringMap.empty 
    cpb.Pb_sig.cpb_interface 
  
  
let compute_annotated_contact_map_in_approximated_mode system cpb contact_map  =
  let local_map = compute_annotated_contact_map_init cpb in 
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
		      (String2Map.find (x,x') sol;sol)
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
	    | (L((a,b,c),(d,e,f)),false)::q  -> 
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
			B(x,y,z),true when (x=a && y=b && z=c) or (x=d && y=e && z=f) -> true 
		      |	AL((x,y,z),(xx,yy)),true when (x=a && y=b && z=c && xx=e && yy=f) or (x=d && y=e && z=f && xx=b & yy=c) -> true 
		      |	L((x,y,z),(xx,yy,zz)),true when (x=a && y=b && z=c && xx=d && yy=e && zz=f) or (xx=a && yy=b && zz=c && x=d && y=e && z=f) -> true 
		      | H(_),_ -> true 
		      | _ -> false
			    )
		    rule.Pb_sig.injective_guard
		    )
		rs.Pb_sig.rules
	else false
      end
  |	_ -> false 

type tr = Half of string*string | Unbind of string*string*string*string 
	

let which_trivial_rule rs = 
  let control = rs.Pb_sig.control in 
  let context = control.Pb_sig.context_update in 
  let uncontext = control.Pb_sig.uncontext_update in 
  match context,uncontext with 
    _,[a,b,c] -> Half(b,c)
  | _,[] ->
      begin
	let rec aux context rep = 
	  match context with 
	    (AL(_),_)::q | (B(_),_)::q-> aux q rep
	    | (L((a,b,c),(d,e,f)),false)::q  -> 
		begin
		  match rep with 
		    None -> Some (Unbind(b,c,e,f))
		  | Some _ -> None
		end
	    | _ -> None
	  in 
	(match aux context None  
	with None -> error 256
	| Some a -> a )
      end
  |	_ -> error 259 

let trivial_rule2 (contact,keep_this_link) rule = 
  trivial_rule rule
    && 
  begin 
    match which_trivial_rule rule
    with 
      Half(a,b) -> 
	List.for_all 
	  (fun (_,c,d) -> not (keep_this_link (a,b) (c,d)))
	  (contact (a,b))
    | Unbind(a,b,c,d) -> not (keep_this_link (a,b) (c,d))
  end
    
let compute_annotated_contact_map_in_compression_mode system cpb contact_map =
  let local_map = compute_annotated_contact_map_init cpb in
  let site_map = 
    List.fold_left 
      (fun map (a,b,c) -> 
	let set = 
	  List.fold_left 
	    (fun set b -> StringSet.add b set)
	    (List.fold_left 
	       (fun set c -> StringSet.add c set)
	       StringSet.empty 
	       c)
	    b in
	StringMap.add a set map) 
      StringMap.empty cpb.cpb_interface in 
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
	StringMap.add ag (new1_map,new2_map)  map in
      
      let site_relation = 
	List.fold_left
	  (fun relation rs -> 
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
	    let destroyed_agent = String2Set.empty in 
	    List.fold_left 
	      (fun relation rule -> 
		let tested_sites = 
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
			    (String2Map.find (x,x') sol;sol)
			  with 
			    Not_found -> 
			      (String2Map.add (x,x') StringSet.empty sol))
		      |  H _,_ -> sol
		      | Connected _,_ -> sol 
		      |  B(a,a',b),_ ->  fadd (a,a') b sol
		      | AL((a,a',b),_),_ -> fadd (a,a') b sol
		      | L((a,a',b),(c,c',d)),_ -> fadd (a,a') b (fadd (c,c') d sol)
		      | M((a,a',b),c),_ -> fadd (a,a') b sol
		      | _ -> error 964 )
		    String2Map.empty  rule.Pb_sig.injective_guard in
	     (*  	let relation = 
		  String2Set.fold
		    (fun (a,a') relation -> 
		      let tested = 
			try String2Map.find (a,a') tested_sites
			with Not_found -> StringSet.empty in 
		      StringSet.fold
			(fun site relation -> 
			  StringSet.fold 
			    (fun site' relation -> 
			      if site==site' 
			      then relation 
			      else
				fadd a' site site' 
				  (fadd a' site' site relation))
			    tested relation)
			tested relation)
		    destroyed_agent relation in *)
		let relation = 
		  String2Set.fold 
		    (fun (a,a') relation -> 
		      let set = 
			try StringMap.find a' site_map 
			with 
			  Not_found -> StringSet.empty 
		      in 
		      if StringSet.is_empty set then relation
		      else
			let min = StringSet.min_elt set in 
			StringSet.fold 
			  (fun x relation -> 
			    fadd a' min x 
			      (fadd a' x min relation))
			  set relation)
		    destroyed_agent relation in 
		  let relation = 
		  String2Map.fold2
		    (fun _ _ x -> x)
		    (fun _ _ x -> x)
		    (fun (a,a') tested modified rel -> 
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
		    relation in 
		relation)
	      relation 
	      rs.Pb_sig.rules)
	  StringMap.empty 
	  system 
      in 
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
			  
		    
		let rec aux to_visit rep =
		  match to_visit with 
		    [] -> rep
		  |  t::q when StringSet.mem t rep -> aux q rep
		  |  t::q -> 
		      let rep = StringSet.add t rep in
		      let to_visit = 
			StringSet.fold 
			  (fun a l -> a::l)
			  (try StringMap.find t map2 with Not_found -> StringSet.empty)
			  to_visit
		      in
		      aux to_visit rep in
		let aux a b = 
		  let rep = aux a b in 
		  rep in 
		let covering_classes = 
		  StringMap.fold 
		    (fun a _ l -> 
		      ({kept_sites= aux [a] StringSet.empty})::l)
	        map2 [] in 
		let old = 
		  try StringMap.find a local_map 
		  with Not_found -> [] in 
		StringMap.add a (covering_classes@old) local_map)
	      site_relation
	      local_map 
      in
      let solid_edges = String22Set.empty in 
      let solid_edges = 
	List.fold_left 
	  (fun solid_edges rs -> 
	    if trivial_rule rs 
	    then solid_edges 
	    else 
	      let passive = rs.Pb_sig.passive_species in 
	      let solid_edges = 
		List.fold_left 
		  (fun solid_edges ((a,b,c),(d,e,f)) ->
		    String22Set.add ((b,c),(e,f)) 
			(String22Set.add ((e,f),(b,c)) solid_edges))
		  solid_edges passive in
	      let solid_edges = 
		List.fold_left 
		  (fun solid_edges b -> 
		    match b with 
		      L((a,b,c),(d,e,f)),false -> 
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
	      solid_edges)
	  
	  solid_edges system 
      in 
      (local_map,solid_edges) 


let compute_annotated_contact_map_in_flat_mode system cpb contact_map = 
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
    
