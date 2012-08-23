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

let facto n = 
  let rec aux k output = 
    if k>n then output
    else aux (k+1) (output*k)
  in aux 1 1 
  

let error i = 
  unsafe_frozen None (Some "Complx") (Some "Annotated_contact_map.ml") None (Some ("line "^(string_of_int i))) (fun () -> raise Exit)


let succ x = 
  match 
    x
  with 
    | None -> None 
    | Some x -> Some (x+1)

let output_renamed file handler empty pb local_map var_of_b varset_empty varset_add build_kleenean print_kleenean = 
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
  let is_access = 
    match pb.unreachable_rules with 
	None -> (fun x -> true)
      | Some a -> (fun x -> not (RuleIdSet.mem x a)) 
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
                  then StringSet.fold (fun site s -> s^"_"^site) mall (a^"_") 
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
  let proj_solution solution = 
    let specie_map = 
      Solution.AA.fold 
	(fun i a  -> IntMap.add i (Agent.name a))  
	solution.Solution.agents 
	IntMap.empty 
    in 
    let tuple_map = 
      Solution.AA.fold 
	(fun i a tuple_map -> 
	  let ag = IntMap.find i specie_map in
          let tuple_map' = 
            Agent.fold_interface  
	      (fun s (m1,m2) tuple_map -> 
                if s="_" 
                then 
                  tuple_map 
                else 
                  let ag = sites(ag,s) in 
                  let tuple = 
                    try (StringMap.find ag tuple_map)
                    with 
                        Not_found -> StringMap.empty 
                  in 
                  let tup = 
                    try 
                      (StringMap.find s tuple)
                    with Not_found -> tuple_bot
                  in 
	          let tup = 
		    match m1 
		    with Agent.Wildcard -> tup
		      | Agent.Marked m -> 
		        {tup with mark = Init m}
		      | _ -> error 2431 
	          in 
	          let tup = 
		    match m2 with Agent.Wildcard -> tup
		      | Agent.Free -> 
		    {tup with is_bound = Init false}
		      | Agent.Bound -> tup
		      | _ -> error 2439 
	          in 
	          StringMap.add ag (StringMap.add s tup tuple) tuple_map)
              a tuple_map 
          in 
          if tuple_map == tuple_map' 
          then 
            let ag = agent ag in 
            StringMap.add ag StringMap.empty tuple_map
          else
            tuple_map')
	solution.Solution.agents StringMap.empty 
    in 
    let tuple_map,_ =
	  Solution.PA.fold
	    (fun (i,s) (i',s') (tuple_map,n) -> 
	      let ag = IntMap.find i specie_map in 
              let ag = sites (ag,s) in 
	      let ag'= IntMap.find i' specie_map in 
              let ag' = sites (ag',s') in 
              let tuple = 
                try 
                  StringMap.find ag tuple_map
                with 
                  | Not_found -> StringMap.empty
              in 
              let tup = 
                try 
                  StringMap.find s tuple 
                with 
                  | Not_found -> tuple_bot 
              in 
              let tuple_map = StringMap.add ag (StringMap.add s {tup with link = Init (bound_of_number n)} tuple) tuple_map 
              in 
              let tuple' = 
                try 
                  StringMap.find ag' tuple_map
                with 
                  | Not_found -> StringMap.empty
              in 
              let tup' = 
                try 
                  StringMap.find s' tuple'
                with Not_found -> tuple_bot 
              in 
              let tuple_map = StringMap.add ag' (StringMap.add s' {tup' with link = Init (bound_of_number n)} tuple') tuple_map 
              in 
              tuple_map,n+1)
	    solution.Solution.links 
            (tuple_map,0)
    in
    tuple_map 
  in 
  let init = 
    (match pb.Pb_sig.simplx_encoding with Some (a,b,b2,c,d) -> List.rev b2
      | None -> error 2809 )
  in 
  let obs = 
     (match pb.Pb_sig.simplx_encoding with Some (a,b,b2,c,d) -> List.rev c
      | None -> error 2809 )
  in 
  let l_obs = 
    List.fold_left 
      (fun list obs -> 
          match 
            obs
          with
            | Solution.Concentration (s,t) -> t::list
            | _ -> list)
      [] 
      (List.rev obs)
  in 
  let print_solution a = 
     let tuple_map = 
       proj_solution a
     in 
     StringMap.fold
       (fun ag tuple bool -> 
         let pretty = StringMap.add ag  tuple StringMap.empty in 
	 let l = 
	   print_pretty 
	     handler 
	     ag
	     (fun x->true)
	     ((pretty,pretty),0)
	     tuple_known
	     empty
	     (if bool then handler.agent_separator () else "")
	     (fun x->x) 
	     (fun x->x) 
	     None 
	     None in 
	 let _ = 
	   List.iter 
	     (Printf.fprintf channel "%s")
	     (List.rev 
	        ((fun (_,a,_) -> a) 
	            l)) in
	      true 
	    )
            tuple_map false
  in 
  let print_init (a,k) =
    let _ = Printf.fprintf channel "%sinit:" "%" in 
    let _ = if k<>1 then Printf.fprintf channel "%i * " k in 
    let _ = print_solution a in 
    let _ = Printf.fprintf channel "\n" in () 
  in 
  let rule_map,l = 
    List.fold_left 
      (fun (rule_map,list_rule) rule_class -> 
        let inj = rule_class.rules in 
        List.fold_left 
          (fun (rule_map,list_rule) rule -> 
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
                              StringSet.fold (fun site s -> s^"_"^site) mall (a^"_") in 
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
              let l = List.sort compare l in 
              let smash l = 
                let rec aux l counter old output = 
                  match 
                    l
                  with
                    | t::q when t=old -> aux q (counter+1) old output
                    | t::q -> aux q 1 t ((old,counter)::output)
                    | []  -> (old,counter)::output
                in 
                begin 
                  match l 
                  with 
                    | []  -> []
                    | t::q -> aux q 1 t []
                end 
              in 
              let l = smash (List.rev l)  in 
              let comment_rule = 
                if l = [] then "" 
                else 
                  let s,_,n =
                    List.fold_left 
                      (fun (s,bool,n) (a,k) -> 
                          ("["^a^"()]"^(if bool then "*" else "")^s,true,n*(facto k))
                      )
                      ("",false,1)
                      l 
                  in 
                  if n=1 
                  then 
                    "the rate should be divided by "^s
                  else 
                    "the rate should be divided by "^s^" and multiplied by "^(string_of_int n)
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
                   (fun x -> 3) 
                   (IntSet.empty) 
                   s 
                   (Some "()") 
                   (fun x->x) 
                   string_of_id 
                   true 
                   None 
                   None 
              in 
                 List.fold_left  
                   (fun (rule_map,l) (a,b) -> 
	             match a with 
	                 [r] -> let rid = r.Pb_sig.r_id in 
	                        let old = name_of_rule r in 
		                let flag = if string_prefix old rid then rid else old in
                                  if r.Pb_sig.r_clone  then rule_map,l else 
		                
                 		    IntMap.add r.r_simplx.Rule.id (a,b,comment_rule) rule_map , r::list_rule
	               | _ -> error 947 ) 
                   (rule_map,list_rule) 
                   s
            end 
          )
          (rule_map,list_rule) 
          inj
      )
      (IntMap.empty,[]) 
      rule_system
  in 
  let print s = Printf.fprintf channel s in 
  let print_opt = print  "%s" in 
  let rec aux cl l_rule l_init l_obs  = 
    match cl with 
        [] -> ()
      | (Decl a)::q -> 
        (
          print "%s" a;
          print "\n";
          aux q l_rule l_init l_obs)
      | (Mutt a)::q -> 
        (print_opt a;print_opt "\n";aux q  l_rule l_init l_obs)
      | (Init_line a)::q -> 
        let _ = print_opt !Config_complx.comment in 
        let _ = print_opt a in 
        let _ = print_opt "\n" in 
        let sol = List.hd l_init in 
        let _ = print_init sol in 
        aux q l_rule (List.tl l_init) l_obs
      | (Obs_line a)::q -> 
        let _ = print_opt !Config_complx.comment in 
        let _ = print_opt a in 
        let _ = print_opt "\n" in 
        let sol = List.hd l_obs in
        let _ = print "%sobs: " "%" in 
        let _ = print_solution sol in 
        let _ = print "\n" in 
        aux q l_rule l_init (List.tl l_obs)
      | (Rgl a)::q -> (
        let name = 
          try ( 
            let id = List.hd l_rule in 
            let list = IntMap.find (id.Pb_sig.r_simplx.Rule.id) rule_map in 
            let (l1,l2,l3) = list in 
            List.fold_left 
              (fun s rid -> 
		if s = "" 
		then 
		  "'"^(name_of_rule rid)^"'"
		else 
		  "'"^(name_of_rule rid)^"',"^s)
	      "" l1)
	  with _ -> "" in 
	let nspace = 
	  let rec aux k = 
	    try (match String.get  a.lhs k with 
		' ' -> aux (k+1)
	      | _ -> k)
	    with _ -> k in aux 0 in 
	print_opt !Config_complx.comment;
	print_opt "former rule:";
	print_opt "\n";
	let oldflaglength = 
	  1+(if nspace = 0 then 1 else nspace)
	  + (max 1 (match a.flag with None -> 0
	    | Some s -> 2+(String.length (s))))
	  + String.length a.pref 
	in 
	let _ = print_opt a.pref in 
	let _ = print_opt !Config_complx.comment in 
	let _ = 
	  match a.flag with 
	      None -> (print_opt !Config_complx.comment)
	    | Some s -> (print_opt "'";
			 print_opt s;
			 print_opt "'") in 
	let _ = (if nspace=0 then print_opt " ") in 
	let _ = print_opt  a.lhs in 
	let _ = print_opt  (a.arrow) in 
	let _ = print_opt  a.rhs in 
	let _ = print_opt  a.comments in 
	let _ = print_opt "\n" in 
	let rule=a in 
	try (
	  let f lid ext = 
	    try ( 
	      let id,lid = match lid with t::q -> t,q | [] -> raise Exit in 
              if is_access id 
              then 
                begin 
                  let id = id.Pb_sig.r_simplx.Rule.id in 
            	  let (a,b,c) = 
		    try IntMap.find id rule_map 
                    with Not_found -> error 865  
                  in 
	          let _ = print_opt !Config_complx.comment in 
	          let _ = print_opt " rule on fragments:" in 
	          let _ = print_opt "\n" in 
	          let pref1  = "" in 
	          let pref2  = "" in 
                  let _ = 
	            match rule.flag with 
	                None -> ()
	              | Some s -> (print_opt "'";
			           print_opt s;
			           print_opt "' ") 
                  in
	          let s = Tools.concat_list_string (List.rev b) in 
	          let _  = 
		    (print "%s%s%s" pref1 pref2 s)
	          in 
	          print_opt " ";
	          let _ = 
                    if c="" then 
                      ()
                    else 
                      (print_opt !Config_complx.comment;print_opt c)
                  in 
                  
              (*			   print_opt id.r_id;*)
	          print "\n";lid
                end 
              else 
                let _ = print_opt !Config_complx.comment in 
                let _ = print_opt "The rule is ignored because it cannot be applied.\n" in 
              
                lid)
              
	    with Not_found -> (
              let _ = print_opt !Config_complx.comment in 
              let _ = print_opt "The rule is ignored because it cannot be applied.\n" in 
              let id,lid = List.hd l_rule,List.tl l_rule in   lid) in 
	  ((let l_rule = 
	      if a.dir = 1 then f l_rule "" else 
		(let lid = f l_rule "" in f lid "_op") in aux q l_rule l_init l_obs)))
	with _ -> 
	  error_frozen "431" (fun () -> raise Exit)
      )
	
  in 
  let cl =  
    match pb.txt_lines with 
	Some l -> l 
      | None -> [] in
  
  let _ = aux 
    cl 
    l 
    init
    l_obs 
  in 
  let _ = print_opt "\n" in 
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
		  |  B(a,a',b),_ ->  fadd a' b modif
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
			   | B(a,a',b),_  ->  
                             let _ = Printf.fprintf stdout "%s %s %s \n" a a' b in 
                             fadd a' b sol
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

