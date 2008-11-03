(* 22/02/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Translation between Jean's parser and Compiled Bng format *)
(* translate.ml *)
  
  
open Data_structures 
open Pb_sig
open Tools 
open Error_handler 

let error (*i*) x (*t*) y = 
    unsafe
      (Some x) 
      (Some "cbng.ml") 
      None (*(Some t)*) 
      None (*(Some i)*) 
      y

let error_frozen (*i*) x (*t*) y = 
    unsafe_frozen
      (Some x) 
      (Some "cbng.ml") 
      None (*(Some t)*) 
      None (*(Some i)*) 
      y

  
let fresh cpt logn = 
  let rep1 = string_of_int cpt in 
  (String.make (logn-String.length rep1) ' ')^rep1,cpt+1 

    
let enrich_ms a ms = 
  let ig = Agent.name a in 
  Agent.fold_interface 
    (fun s (m1,m2) ms -> 
      if m1 =  Agent.Wildcard
      then 
	ms 
      else 
	begin
	  let old = try (StringMap.find ig ms) with Not_found -> StringSet.empty in
	  StringMap.add ig (StringSet.add s old) ms
	end
	  )
    a
    ms
    
let translate_init_elt t (agents,marks,markable_sites,linkable_sites,mark_site_rel,cpt,contact) messages logn = 
  let _ = trace_print "BEGIN_TRANSLATE_INIT\n" in 
  let speciemap = 
    Solution.AA.fold 
      (fun i a  -> IntMap.add i (Agent.name a))  
      t.Solution.agents 
      IntMap.empty in 
  let fadd i x m = 
    let ig = 
      try (IntMap.find i speciemap) 
      with Not_found -> 
	(error_frozen "translate.44" (fun () -> raise Exit)) in
    let old = 
      try (StringMap.find ig m) 
      with Not_found -> 
	StringSet.empty in 
      StringMap.add ig (StringSet.add x old) m in 
  let fadd_contact a b graph = 
    let old = 
      try (String2Map.find a graph) 
      with Not_found -> String2Set.empty in 
      String2Map.add a (String2Set.add b old) graph in
  let fadd_contact a b graph = 
    let a,b = 
      (((try (IntMap.find (fst a) speciemap) 
	 with Not_found -> 
	   (error_frozen  "translate.59" (fun () -> raise Exit))),
	snd a),
	((try (IntMap.find (fst b) speciemap) 
	 with Not_found -> 
	   (error_frozen "translate.63" (fun () -> raise Exit))),
	 snd b))
	 in  fadd_contact a b (fadd_contact b a graph) 
  in 
  let fadd_mark_site (a,s) m graph = 
      let iga = 
	try (IntMap.find a speciemap)
	with Not_found -> 
	  (error_frozen "fadd_mark_site" (fun () -> raise Exit)) in 
      let old =
	try (String2Map.find (iga,s) graph) 
	with Not_found -> StringSet.empty in 
	String2Map.add (iga,s) (StringSet.add m old) graph in
	    
  let fadd_test i t m = 
    let old = 
      try (IntMap.find i m) 
      with Not_found -> [] 
    in 
      IntMap.add i (t::old) m in
  
  let test,agents,marks,markable_sites,linkable_sites,mark_site_rel = 
    Solution.AA.fold 
      (fun i a (test,agents,marks,markable_sites,linkable_sites,mark_site_rel) ->
	 (
           Agent.fold_interface  
	     (fun s (m1,m2) (test,agents,marks,markable_sites,linkable_sites,mark_site_rel)-> 
		let test,marks,markable_sites,mark_site_rel = 
		  match m1 
		  with Agent.Wildcard -> 
		    (test,marks,markable_sites,mark_site_rel)
		    | Agent.Marked m -> 
			(fadd_test i (Pb_sig.S_mark(s,m)) test,
			 StringSet.add m marks,
			 fadd i s markable_sites,
			 fadd_mark_site (i,s) m mark_site_rel)
		       | _ -> (error_frozen "translate translate_init" frozen_exit)
		in 
		let test,linkable_sites = 
		  match m2 with Agent.Wildcard -> test,linkable_sites
		    | Agent.Free -> 
			   (fadd_test i (Pb_sig.S_free s) test,
			    fadd i s linkable_sites
			   )
		    | Agent.Bound ->  
			(test,
			 fadd i s linkable_sites)
		    | _ -> (error_frozen  "translate.95" frozen_exit)
		in 
		  test,agents,marks,markable_sites,linkable_sites,mark_site_rel)
	     a
	     (test,StringSet.add (Agent.name a) agents,marks,markable_sites,linkable_sites,mark_site_rel)))
      t.Solution.agents
      (IntMap.empty,agents,marks,markable_sites,linkable_sites,mark_site_rel)
  in 
  let test,contact = 
    Solution.PA.fold 
      (fun (i1,s1) (i2,s2) (test,contact) -> 
	let ig1 = 
	  try (IntMap.find i1 speciemap) 
	  with Not_found -> 
	    (error "IG1.translate" (raise Not_found)) in
	let ig2 = 
	  try (IntMap.find i2 speciemap) 
	  with Not_found -> (error "IG2.translate" (raise Not_found)) 
	in 
	  fadd_test 
	    i1 
	    (Pb_sig.S_bound(s1,(ig2,s2)))
	    (fadd_test 
	       i2 
	       (Pb_sig.S_bound(s2,(ig1,s1))) 
	       test),
	 fadd_contact (i1,s1) (i2,s2) contact)
      t.Solution.links (test,contact)  in 
  (IntMap.fold (fun i s l -> 
		       let ig = IntMap.find i speciemap in 
			 (ig,s)::l) test []),
  (agents,marks,markable_sites,linkable_sites,mark_site_rel,cpt,contact),
  messages
    
let translate_init t sol  messages logn =     
  let a,b,c= 
    List.fold_left 
      (fun (l,sol,messages) (current,n) -> 
	  if n=0 then l,sol,messages
	  else 
	    let l',sol,messages = 
	      translate_init_elt 
		current 
		sol 
		messages 
		logn in 
	    l'@l,sol,messages)
	    
  ([],sol,messages) t 
  in
  Some a,b,c
     
    
let translate_rule t flags fset (agents,marks,markable_sites,linkable_sites,mark_site_rel,cpt,contact) messages logn = 
  let speciemap,maxi =     
    Mods2.IntMap.fold
      (fun _ cc -> 
	 Solution.AA.fold 
	   (fun i a (map,maxi) -> 
	      IntMap.add i (Agent.name a) map,max maxi i) 
	   cc.Solution.agents)   
      t.Rule.lhs (IntMap.empty,0) in 
  let sigma,speciemap,_ = 
    let l = 
      IntMap.fold 
	(fun i s l -> (i,s)::l)
	speciemap [] in
    let l = 
      List.sort 
	(fun (i1,s1) (i2,s2) -> 
	  let comp = compare s1 s2 in
	  if comp = 0 
	  then 
	    compare i1 i2 
	  else 
	    comp)
	l in
    List.fold_left
      (fun (sigma,map,n) (i,s) ->
	(IntMap.add i n sigma,
         IntMap.add n s map,
         n+1))
      (IntMap.empty,IntMap.empty,0)
      l
  in
  let sigma x = 
    try 
      IntMap.find x sigma 
    with 
      Not_found -> x in 
  let speciemap = 
    Mods2.IntMap.fold
      (fun i a -> 
	IntMap.add (sigma i) (Agent.name a))
      t.Rule.add speciemap in   
  let fadd i x m = 
    let ig = 
      try (IntMap.find i speciemap) with Not_found -> (error_frozen  "translate.141" frozen_exit) in
    let old = try (StringMap.find ig m) with Not_found -> StringSet.empty in 
      StringMap.add ig (StringSet.add x old) m in 
  let fadd_g a b graph = 
    let old = 
      try (IntMap.find a graph) 
      with Not_found -> IntSet.empty in 
      IntMap.add a (IntSet.add b old) graph in
  let fadd_g a b g = fadd_g a b (fadd_g b a g) in
  let fadd_contact a b graph = 
    let old = 
      try (String2Map.find a graph) 
      with Not_found -> String2Set.empty in 
      String2Map.add a (String2Set.add b old) graph in
  let fadd_contact a b graph = 
    let a,b = 
      (((try (IntMap.find (fst a) speciemap) with Not_found -> (error_frozen "translate.157" frozen_exit)),snd a),
       ((try (IntMap.find (fst b) speciemap) with Not_found -> (error_frozen "translate.158" frozen_exit)),snd b)) in 
      fadd_contact a b (fadd_contact b a graph) in 
  let fadd_mark_site (a,s) m graph = 
    let iga = 
      try (IntMap.find a speciemap)
      with Not_found -> 
	(error_frozen "fadd_mark_site2" (fun () -> raise Exit)) in 
    let old =
      try (String2Map.find (iga,s) graph) 
      with Not_found -> StringSet.empty 
    in 
      String2Map.add (iga,s) (StringSet.add m old) graph 
  in
  let test=[] in 
  let test,graph,contact,bs = 
    Mods2.IntMap.fold
      (fun _ cc -> 
	 Solution.PA.fold 
	   (fun (i1,s1) (i2,s2) (test,g,c,bs) ->
	     let i1 = sigma i1 in
	     let i2 = sigma i2 in 
	      (if compare (i1,s1) (i2,s2) < 0 then 
		 (Pb_sig.Is_related((i1,s1),(i2,s2))::test,
		  fadd_g i1 i2 g,
		  fadd_contact (i1,s1) (i2,s2) c,
		  IntStringSet.add (i1,s1) (IntStringSet.add (i2,s2) bs))
	       else 
		 (test,g,c,bs)))
	   cc.Solution.links)
      t.Rule.lhs  (test,IntMap.empty,contact,IntStringSet.empty)
  in
  let test,agents,marks,markable_sites,linkable_sites,mark_site_rel = 
    Mods2.IntMap.fold 
      (fun _ cc -> 
	Solution.AA.fold 
	  (fun i a (test,agents,marks,markable_sites,linkable_sites,mark_site_rel) ->
	    let i = sigma i in 
	    let test = (Pb_sig.Is_here(i))::test in 
	    Agent.fold_interface 
	      (fun s (m1,m2) (test,agents,marks,markable_sites,linkable_sites,mark_site_rel)-> 
		let test,marks,markable_sites,mark_site_rel = 
		  match m1 with Agent.Wildcard -> (test,marks,markable_sites,mark_site_rel)
		  | Agent.Marked m -> 
		      (Pb_sig.Is_marked((i,s),m)::test,
		       StringSet.add m marks,
		       fadd i s markable_sites,
		       fadd_mark_site (i,s) m mark_site_rel)
		  | _ -> (print_string "translate";print_newline ();raise Exit)
				    in 
		let test,linkable_sites = 
		  match m2 with Agent.Wildcard -> test,linkable_sites
		  | Agent.Free -> 
		      if s="_" 
		      then (test,linkable_sites) 
		      else 
			((Pb_sig.Is_free (i,s)):: test,linkable_sites)
		  | Agent.Bound ->  
		      if IntStringSet.mem (i,s) bs
		      then (test,fadd i s linkable_sites)
		      else 
					      ((Pb_sig.Is_bound (i,s))::test,
					       fadd i s linkable_sites)
		  | _ -> (print_string "translate.184";print_newline ();raise Exit)
				    in 
		test,agents,marks,markable_sites,linkable_sites,mark_site_rel)
	      a
	      (test,StringSet.add (Agent.name a) agents,marks,markable_sites,linkable_sites,mark_site_rel))
	  cc.Solution.agents)
      t.Rule.lhs 
      (test,agents,marks,markable_sites,linkable_sites,mark_site_rel)
  in 
  
  let permute (i1,s1) (i2,s2) =
    let ag1 = IntMap.find i1 speciemap in
    let ag2 = IntMap.find i2 speciemap in 
    let comp = compare ag1 ag2 in 
    if comp < 0 then (i1,s1),(i2,s2)
    else if comp > 0 then (i2,s2),(i1,s1)
    else 
      let comp2 = compare s1 s2 in
      if comp2 < 0 then (i1,s1),(i2,s2)
      else if comp2 > 0 then (i2,s2),(i1,s1)
      else (i1,s1),(i2,s2) in 

  let control,agents,marks,markable_sites,linkable_sites,roots,contact,mark_site_rel  = 
    Mods2.IntMap.fold
      (fun i1 a ((control,control_r),agents,m,ms,ls,roots,contact,mark_site_tel) -> 
	let i1 = sigma i1 in 
	 list_fold 
	   (fun (a,_) ((control,control_r),agents,m,ms,ls,roots,contact,mark_site_rel) -> 
	      match a with 
		  Solution.Bind(s1,i2,s2) -> 
		    (let i2 = sigma i2 in 
		    (
		    (let a1,a2 = permute (i1,s1) (i2,s2) in 
		    (Pb_sig.Bind(a1,a2))::
			    (Pb_sig.Check(i1))::
			    (Pb_sig.Check(i2))::
			    control),control_r),agents,m,ms,fadd i1 s1 (fadd i2 s2 ls),
		     IntSet.add i1 (IntSet.add i2 roots),
		     fadd_contact (i1,s1) (i2,s2) contact,
		     mark_site_rel)
		| Solution.Break(s1,i2,s2) ->

		    let i2 = sigma i2 in 
		    (((Pb_sig.Check_choice [i1;i2])::(Pb_sig.Release((i1,s1),(i2,s2)))::control,
		      control_r),agents,m,ms,fadd i1 s1 (fadd i2 s2 ls),
		     IntSet.add i1 (IntSet.add i1  roots),
		     fadd_contact (i1,s1) (i2,s2) contact,
		     mark_site_rel)
		| Solution.Mark(s,nm) -> 
		    (((Pb_sig.Mark((i1,s),nm))::(Pb_sig.Check(i1))::control,
		      control_r),agents,
		     StringSet.add nm m,
		     fadd i1 s ms,
		     ls,
		     IntSet.add i1 roots,contact,
		     fadd_mark_site 
		       (i1,s) 
		       nm 
		       mark_site_rel)
		| Solution.Modify s -> 
		    (((Pb_sig.Break_half(i1,s))::(Pb_sig.Check(i1))::control,
                      control_r),agents,
                     m,ms,fadd i1 s ls,
                     IntSet.add i1 roots,contact,
		     mark_site_rel)
		| Solution.Remove -> 
		    ((control,IntSet.add i1 control_r),agents,m,ms,ls,
		     IntSet.add i1 roots,contact,
		     mark_site_rel)
	   )
	   a ((control,control_r),agents,m,ms,ls,roots,contact,mark_site_rel))
      t.Rule.actions
      (([],IntSet.empty),agents,marks,markable_sites,linkable_sites,IntSet.empty,contact,mark_site_rel) in 
  let id,cpt = fresh cpt logn in 

    
  let test,control,agents,markable_sites,mark_site_rel,linkable_sites,contact = 
    let c1,c2 = control in
    let c1 = 
      List.fold_left 
	(fun c1 x -> 
	  match x with 
	    Rule.NO_POLY -> Pb_sig.No_Pol::c1
	  | Rule.NO_HELIX -> Pb_sig.No_Helix::c1)
	c1 t.Rule.constraints in 
    let (c1,c3),agents,cr_agents,ms,mark_site_rel  = 
      Mods2.IntMap.fold
	(fun i a (((c1:Pb_sig.action list),c3),agents,cr_agents,ms,mark_site_rel) -> 
	  let i = sigma i in 
	  let ig = Agent.name a in 
	  (let a1,marks_site_rel = 
	    (Agent.fold_interface 
	       (fun s (m1,m2) (ms,mark_site_rel) ->  
		 if s = "_" then ms,mark_site_rel
		 else 
		   match m1 with 
		     Agent.Marked x -> (Pb_sig.Mark((i,s),x))::ms,
		       fadd_mark_site (i,s) x mark_site_rel
		   | Agent.Wildcard -> ms,mark_site_rel
		   | Agent.Bound -> error "BOUND: translate.ml Added complex" (ms,mark_site_rel)
		   | Agent.Free -> error "BOUND: translate.ml Added complex" 
			 (ms,mark_site_rel)
			 )
	       a 
	       (c1,mark_site_rel)) in 
	  (a1, 
	   IntSet.add i c3),
          StringSet.add ig agents,
	  IntSet.add i cr_agents,
	  enrich_ms a ms,
	  mark_site_rel))
	t.Rule.add 
	((c1,IntSet.empty),agents,IntSet.empty,markable_sites,mark_site_rel) in 
    let rec aux (bl,wl,sol) = 
      match wl with [] -> sol 
      | t::q -> 
	  aux (
	  let s = try (IntMap.find t graph) with Not_found -> IntSet.empty  in
	  IntSet.fold 
	    (fun i (bl,wl,sol) ->
	      if IntSet.mem i bl then (bl,wl,sol)
	      else (IntSet.add i bl,
		    i::wl,
		    IntSet.add i sol))
	    s (bl,q,sol)) in 
    let close = aux (roots,IntSet.fold (fun i sol -> i::sol) roots [],roots) in
    let control,close = 
      Mods2.IntMap.fold
	(fun _ cc (control,close) -> 
	  Solution.AA.fold 
	    (fun i _ (control,close) -> 
	      let i = sigma i in 
	      if IntSet.mem i close
	      then control,close
	      else (
		let new_comp = aux (IntSet.singleton i,[i],IntSet.singleton i) in 
		((Pb_sig.Check_choice(IntSet.elements new_comp)::control),
		 IntSet.union close new_comp)))
	    cc.Solution.agents (control,close))
	t.Rule.lhs (c1,close)
    in test,(control,c2,c3),agents,ms,mark_site_rel,linkable_sites,contact
  in
  
  let a = match t.Rule.flag with None -> id | Some a -> a in 
  let f= 
    let rec aux n = 
      let fl = 
	if n=0 then a else a^"."^(string_of_int n) in 
      if StringSet.mem fl fset then aux (n+1)
      else (fl)
    in aux 0 in 
  let fset = StringSet.add f fset in 
  let flags = StringMap.add id f flags in 
  let c,cr,ca = control in 
  let c,linkable_sites = 
    IntSet.fold 
      (fun i (c,ls) -> 
	list_fold 
	  (fun b (c,s) -> 
	    match b with Pb_sig.Is_related((i1,s1),(i2,s2)) when i1=i or i2 =i -> 
	      (Pb_sig.Check_choice([i1;i2]))::
	      Pb_sig.Release((i1,s1),(i2,s2))::c,fadd i1 s1 (fadd i2 s2 s)
	    | _ -> (c,s))
	  test (c,ls))
      cr (c,linkable_sites) in 
    {
      Pb_sig.cpb_quarks = None;
      Pb_sig.cpb_equal = [];
      Pb_sig.cpb_r_species=IntMap.fold (fun i s l -> (i,s)::l) speciemap []; 
      Pb_sig.cpb_passive= None;
      Pb_sig.cpb_guard=[[{r_id=id;
			  r_clone=false;
			  r_simplx=t}],IntSet.empty,test];
      Pb_sig.cpb_dots = IntSet.empty; (*TODO*)
      (*list_fold (fun (Solution.CC(i,a,b)) s -> 
        (Mods2.IntSet.fold 
        (fun a s -> IntSet2.add (i,a) s)
        t.Rule.constraints Int2Set.empty;*)
      Pb_sig.cpb_control=
	{Pb_sig.cpb_create=ca;
	 Pb_sig.cpb_update = c;
         Pb_sig.cpb_remove = cr}
    } ,flags,fset,(agents,marks,markable_sites,linkable_sites,mark_site_rel,cpt,contact),messages
    
let clean_rule_system rl linkable_sites =
    List.rev
      (List.rev_map 
	 (fun r -> 
	    let ig x  = 
	      let rec aux l = 
		match l with (a,b)::q when a=x -> b
		  | _::q -> aux q
		  | [] -> error "TRANSLATE.381" "" in 
		aux r.Pb_sig.cpb_r_species in 
              {r with Pb_sig.cpb_guard = 
		  List.rev 
		    (List.rev_map 
		       (fun (a,c,b) -> 
			  (a,c,List.filter 
			     (fun a -> 
				match a with Pb_sig.Is_bound(a,b) | Pb_sig.Is_free(a,b) -> 
				  StringSet.mem b 
				    (try (StringMap.find (ig a) linkable_sites)
				     with Not_found -> StringSet.empty)
				  | _ -> true) b))
		       r.Pb_sig.cpb_guard)})
		rl)
  
let translate_rule_list l init interface  messages = 
  let n = List.length l in 
  let logn = String.length (string_of_int n) in 
  let flags,fset,rl,(agents,marks,markable_sites,linkable_sites,mark_site_rel,cpt,contact),messages,with_dots  = 
    list_fold 
      (fun a (flags,fset,rl,context,messages,with_dots) -> 
	let r,flags,fset,context,messages = translate_rule  a flags fset context messages logn in 
	let with_dots = with_dots or (not ([]=a.Rule.constraints)) in 
	(flags,fset,r::rl,context,messages,with_dots))
      l
      (StringMap.empty,StringSet.empty,[],(StringSet.empty,StringSet.empty,StringMap.empty,StringMap.empty,String2Map.empty,1,String2Map.empty),messages,false) in 
  let init,(agents,marks,markable_sites,linkable_sites,mark_site_rel,cpt,contact),messages = 
    translate_init init (agents,marks,markable_sites,linkable_sites,mark_site_rel,cpt,contact) messages logn  in 
  let _ = trace_print "TRANSLATE_INIT DONE\n" in
  let (agents,markable_sites,linkable_sites) = 
    match interface 
    with None -> 
      agents,markable_sites,linkable_sites
    | Some interface -> 
	List.fold_left 
	  (fun (agents,markable_sites,linkable_sites) (s,b,c) -> 
	    (StringSet.add s agents),
	    (let old = 
	      try 
		StringMap.find s markable_sites 
	      with 
		Not_found -> StringSet.empty 
	    in
	    StringMap.add s (List.fold_left (fun set a -> StringSet.add a set) old b) markable_sites),
	    let old = 
	      try 
		StringMap.find s linkable_sites 
	      with 
		Not_found -> StringSet.empty 
	    in
	    StringMap.add s (List.fold_left (fun set a -> StringSet.add a set) old c) linkable_sites)
	  (agents,markable_sites,linkable_sites) 
	  interface 
  in
  let species_set = agents in 
  let species,interface = 
    StringSet.fold 
      (fun s (species,interface) ->  let _ = trace_print "FOLD" in
	(s::species,
	 (s,StringSet.fold (fun a b -> a::b) 
	    (try (StringMap.find s markable_sites) with Not_found -> StringSet.empty) [],
	  StringSet.fold (fun a b -> a::b) 
	    (try (StringMap.find s linkable_sites) with Not_found -> StringSet.empty) [])::interface))
      species_set ([],[]) in 
  
 
  
  {Pb_sig.cpb_marks = StringSet.fold (fun a b -> a::b) marks [];
    Pb_sig.cpb_species = species ;
    Pb_sig.cpb_with_dots = with_dots;
    Pb_sig.cpb_interface = interface;
    Pb_sig.cpb_interface_of_agent = 
    List.fold_left 
      (fun map (a,b,c) -> StringMap.add a (b,c) map)
      StringMap.empty 
      interface;
   Pb_sig.cpb_contact = Some 
      (String2Map.map (fun x -> 
			String2Set.fold (fun a b -> a::b) x []) contact) ;
   Pb_sig.cpb_mark_site =
      Some
	(String2Map.map (fun x -> 
			   StringSet.fold (fun a b -> a::b) x []) mark_site_rel);
   Pb_sig.cpb_rules = clean_rule_system  rl linkable_sites;
   Pb_sig.cpb_flags = flags;
   Pb_sig.cpb_init = init
	   },messages




(*
let tr fic fic_output messages = 
  Data.forward:= (!Config_complx.forward);
  let (a,(b:(Solution.t*int)list),c,d) = Lexer.compile  fic  in 
  let _ = trace_print "COMPILATION DONE" in
  let rep,messages = translate_rule_list (List.rev a) b messages in 
  let _ = trace_print "TRANSLATE DONE" in
  let _ = Cbng_sig.cbng_dump fic_output rep in   
 rep,messages
*)
