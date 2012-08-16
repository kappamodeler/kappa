open Printf
open Config_complx
open Tools
open Data_structures
open Pb_sig
open Kleenean_expr
open Abstract_expr_sig
open Comment_sig  
open Tools 
open Output_contact_map 
open Ode_print_sig
open Ode_print 
open Error_handler 
open Annotated_contact_map 


let debug = false



let error i = 
  unsafe_frozen None (Some "Complx") (Some "Annotated_contact_map.ml") None (Some ("line "^(string_of_int i))) (fun () -> raise Exit)


let succ x = 
  match 
    x
  with 
    | None -> None 
    | Some x -> Some (x+1)

let output_renamed file pb local_map var_of_b varset_empty varset_add build_kleenean print_kleenean = 
  let local_map = local_map.subviews in 
  let channel = 
    if file = ""
    then 
      stdout
    else 
      open_out file 
  in 
  let cpb = 
    match pb.intermediate_encoding 
    with None -> error 6 
    | Some a -> a 
  in
  let boolean_encoding = 
    match pb.boolean_encoding 
    with None -> error 7 
    | Some boolean_encoding -> boolean_encoding 
  in
  let rule_system = boolean_encoding.system in 
  let agent,sites = 
    let agent,sites = 
      StringMap.fold 
        (fun a l (agent,sites) -> 
          let size = List.length l in 
          let agent,sites,_ = 
            List.fold_left 
              (fun (agent,sites,bool) mall-> 
                let mall = mall.kept_sites  in 
                let a' = 
                  if size > 1 
                  then StringSet.fold (fun site s -> s^"."^site) mall (a^".") 
                  else a
                in 
                let agent = 
                  if bool 
                  then 
                    StringMap.add a a' agent
                  else 
                    agent
                in 
                let sites = 
                  StringSet.fold 
                    (fun s -> String2Map.add (a,s) a')
                    mall sites 
                in 
                agent,sites,false)
              (agent,sites,true)
              l
          in 
          agent,sites)
        local_map
        (StringMap.empty,String2Map.empty) 
    in 
    (fun a -> 
      try StringMap.find a agent
      with Not_found -> ""),
    (fun s -> 
      try String2Map.find s sites
      with Not_found -> "")
  in 
  let _ = 
    List.iter 
      (fun rule_class -> 
        let inj = rule_class.rules in 
        List.iter 
          (fun rule -> 
            begin 
              let see (a,x) map = 
                try 
                  let _ = StringMap.find a map in map
                with
                  | Not_found -> StringMap.add a (sites (a,x)) map
              in 
              let get a map =
                try
                  StringMap.find a map
                with 
                  | Not_found -> agent a 
              in 
              let site_of_agent_type = 
                List.fold_left 
                  (fun map (b,bool) -> 
                    match 
                      b 
                    with 
                      | L((_,a,s),(_,a',s')) -> see (a,s) (see (a',s') map)
                      | B(_,a,s) | AL((_,a,s),_) | M((_,a,s),_) -> see (a,s) map
                      | _ -> map)
                  StringMap.empty 
                  rule.injective_guard 
              in 
              let labels,control,guard = 
                rule.labels,
                rule_class.control,
                rule.injective_guard 
              in
              let rename_agent = 
                (fun a -> 
                  try
                    StringMap.find a site_of_agent_type
                  with 
                      Not_found -> agent a)
              in 
              let original_string_of_id = 
                List.fold_left 
                  (fun map (pred,_) -> 
                      match pred 
                      with 
                        | L((a,a',_),(b,b',_)) -> 
                          StringMap.add a a' (StringMap.add b b' map)
                        | B(a,a',_) 
                        | M((a,a',_),_) 
                        | H(a,a') -> 
                          StringMap.add a a' map
                        | _ -> map)
                  StringMap.empty 
                  guard
              in 
              let guard = 
                List.map 
                  (fun (pred,bool) -> 
                    match pred 
                    with 
                    | L((a,a',s),(b,b',s')) -> L((a,rename_agent a',s),(b,rename_agent b',s')),bool
                    | B(a,a',s) -> B(a,rename_agent a',s),bool
                    | M((a,a',s),m) -> M((a,rename_agent a',s),m),bool
                    | H(a,a') -> H(a,rename_agent a'),bool
                    | _ -> pred,bool)
                  guard 
              in 
              let control = 
                {control 
                 with 
                   uncontext_update=
                    List.map (fun (a,a',s) -> a,rename_agent a',s) control.uncontext_update ;
                  context_update=
                    List.map (fun (b,bool) -> 
                      match b with 
                            | L((a,a',s),(b,b',s')) -> L((a,rename_agent a',s),(b,rename_agent b',s')),bool
                    | B(a,a',s) -> B(a,rename_agent a',s),bool
                    | M((a,a',s),m) -> M((a,rename_agent a',s),m),bool
                    | H(a,a') -> H(a,rename_agent a'),bool
                    | _ -> b,bool)
                      control.context_update}
              in 
              let string_of_id = 
                List.fold_left 
                  (fun map (pred,_) -> 
                      match pred 
                      with 
                        | L((a,a',_),(b,b',_)) -> 
                          StringMap.add a a' (StringMap.add b b' map)
                        | B(a,a',_) 
                        | M((a,a',_),_) 
                        | H(a,a') -> 
                          StringMap.add a a' map
                        | _ -> map)
                  StringMap.empty 
                  guard
              in 
              let string_of_id,guard,remove,l,_ = 
                List.fold_left
                  (fun (string_of_id,g,remove,l,n) id -> 
                      let a = StringMap.find id original_string_of_id in 
                      let a' = StringMap.find id string_of_id in 
                      let list = StringMap.find a local_map in 
                      if List.length list > 1 
                      then 
                        List.fold_left 
                          (fun (string_of_id,g,remove,l,n) s -> 
                            let mall = s.kept_sites  in 
                            let a'' = 
                              StringSet.fold (fun site s -> s^"."^site) mall (a^".") in 
                            if a''=a'
                            then (string_of_id,g,remove,l,n)
                            else 
                              let id = a^"%%"^(string_of_int n) in 
                              (StringMap.add id a'' string_of_id,
                               ((H(id,a'')),true)::g,
                               id::remove,
                               a''::l,
                               n+1)
                          )
                          (string_of_id,g,remove,l,n) list
                      else 
                        (string_of_id,g,remove,l,n))
                  (string_of_id,guard,control.remove,[],1)
                  control.remove 
              in 
              let control = {control with remove = remove} in 
              let guard = 
                List.map (fun (pred,bool) -> var_of_b pred,bool) guard 
              in 
              let string_of_id a = 
                try 
                  StringMap.find a string_of_id 
                with 
                  | Not_found -> a
              in 
              let rs = [labels,control,guard] in 
              let vars = 
                List.fold_left 
                  (fun sol (_,_,b) -> 
	            List.fold_left 
	              (fun sol (a,_) -> varset_add a sol)
	              sol b)
                  varset_empty 
                  rs in
              let s = build_kleenean rs vars in 
              let s = 
                 print_kleenean 
                   string_txt
                   (fun x->true) 
                   (fun x -> 
	             (match x.r_simplx.Rule.flag 
	              with None -> x.r_id 
	                | Some a -> a))
                   (fun x -> 3) (IntSet.empty) 
                   s 
                   (Some "()") 
                   (fun x->x) 
                   string_of_id 
                   true 
                   None 
                   None in 
               let _ = 
                 List.iter 
                   (fun (a,b) -> 
	             match a with 
	                 [r] -> let rid = r.Pb_sig.r_id in 
	                        let old = name_of_rule r in 
		                let flag = if string_prefix old rid then rid else old in
		                if r.Pb_sig.r_clone  then () else 
		                  let _ = print_string "'" in
		                  let _ = print_string flag in
		                  let _ = print_string "' " in
		                  let _ = List.iter print_string (List.rev b) in
		   (* let _ = print_string " @ " in
		      let _ = print_string (Printf.sprintf  "%f" kynetic) in*)
		                  let _ = print_newline () in 
		                  ()
	               | _ -> error 947 ) 
                   s
               in 
               ()
            end 
          )
          inj
      )
      (List.rev rule_system)
  in 
  if file = ""
  then ()
  else 
    close_out channel

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

let compute_annotated_contact_map_in_stoc_mode system pb cpb contact_map  =
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

