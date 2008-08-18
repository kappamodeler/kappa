(* 11/05/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Reachability analysis *)
(* reachability.ml *)

open Config_complx
open Tools
open Tools2
open Data_structures
open Abstract_expr_sig
open Pb_sig
open Contact_map

let smart_lens = true

let error i x t y = 
    unsafe
      (Some x) 
      (Some "reachability.ml") 
      (Some t) 
      (Some i) 
      y

let error_frozen i x t y = 
    unsafe_frozen
      (Some x) 
      (Some "reachability.ml") 
      (Some t) 
      (Some i) 
      y

module Iterateur=
  (functor (BDD:AbstractExprBool) ->
    struct
     
      module AE=BDD
      type solution_binding = AE.abstract_expr StringMap.t
      type instance_binding = AE.abstract_expr StringMap.t


      let forall2 f g h a b = 
	try (StringMap.iter2 
	       (fun x a -> if not (f x a) then raise Exit)
	       (fun x a -> if not (g x b) then raise Exit)
	       (fun x a b -> if not (h x a b) then raise Exit) a b ;true)
	with Exit -> false
	       

      let relation = 
	if !Config_complx.build_relationships_among_species then 
	  (fun x -> "0")
	else 
	  (fun x -> x) 

      let  teste_rule pb case guard cm sb  f  = 
	let cpb = 
	  match pb.intermediate_encoding with 
	    None -> error_frozen "line 59" "intermediar encoding is missing" "teste_rule" (fun () -> raise Exit)
	  | Some a -> a in 
	let sl = 
	  list_fold 
	    (fun (a,b) sl -> 
	      match (AE.K.E.V.b_of_var a,b) 
	      with 
		Connected ((id1,ig1),(id2,ig2)),_ ->
		  (let l1,l2 = 
		    are_connected (id1,ig1) (id2,ig2) cm 
		      (fst (find_empty 
			      StringMap.find 
			      ig1 
			      cpb.cpb_interface_of_agent 
			      ([],[]))) in 
		  match l1,l2 with [a],[b] -> 
		    let l1 = list_b_g (B(((id1:string),(ig1:string),(snd a:string))),true) in 
		    let l2 = list_b_g (B((id2,ig2,snd b)),true) in 
		    list_fold 
			(fun (a,b) sl -> 
			  let top = f (topic_of_b a) in 
			  let l = 
			    try (StringMap.find top sl) 
			    with Not_found -> [] in 
			  let l' = (a,b)::l in 
			  StringMap.add top l' sl)
		    (l1@l2) sl | _ -> sl)
	      | t,b  -> 
		  let l = list_b_g (t,b)   in 
		  list_fold 
			(fun (a,b) sl -> 
			  let top = f (topic_of_b a) in 
			  let l = 
			    try (StringMap.find top sl) 
			    with Not_found -> [] in 
			  let l' = (a,b)::l in 
			  StringMap.add top l' sl)
		    l sl)
	    guard StringMap.empty 
	in 
	try (Some (
	  StringMap.fold 
	    (fun a l sb' ->
	      let ae'=AE.conj (StringMap.find a sb) (AE.list_conj l) in 
	      if AE.is_bot ae' 
	      then (raise Exit)
	      else 
		StringMap.add 
		  a ae'
		sb')
	  sl sb))
	    with Exit -> None


      let apply_control_list pb case control sb f  potential_binding = 
	let cpb = 
	  match pb.intermediate_encoding with 
	    None -> error_frozen "line 116" "intermediar encoding is missing" "apply_control_list" (fun () -> raise Exit)
	  | Some a -> a in 
	let specie_of_id = case.specie_of_id in 
	let apply_control_set control sb specie_of_id  f = 
	  let fadd a b sol = 
	    let l =
	      try (StringMap.find a sol)
	      with Not_found -> AE.K.E.V.varmap_empty in
	    StringMap.add a (AE.K.E.V.varmap_add (fst b) (snd b) l) sol in 
	  let control' = 
	    list_fold 
	      (fun (a,b) sb -> 
		let l = list_b_g (a,b)  in 
	      list_fold 
		  (fun (a,b) sb -> 
		    let top = f (topic_of_b a) in 
		    fadd top (AE.K.E.V.var_of_b a,b) sb)
		  l sb)
	      control 
	      StringMap.empty in 
	  StringMap.fold 
	    (fun a l sb -> 
	      StringMap.add 
		a
		(AE.set (StringMap.find a sb) l)
		sb)
	    control' sb in 
	apply_control_set control sb specie_of_id  f,
	list_fold (fun a pot_bind ->
	    match  a  with 
		L((a,ga,a'),(b,gb,b')),_ -> 
		add_contact cpb.cpb_with_dots (ga,a') (gb,b') pot_bind 
	    | _ -> pot_bind)
 	  control potential_binding 


      let apply_control_context_update pb case = apply_control_list pb case (case.control.context_update) 
	  

      let apply_control_uncontext_update pb case sb f  potential_binding = 
	let contact = 
	  match pb.contact_map with 
	    None -> error "line 158" "contact map is missing" "apply_control_uncontext_update" (raise Exit)
	  | Some a -> a in 
	let control = case.control.uncontext_update in 
	list_fold 
	  (fun (i1,ig1,s1) (sb,l) -> 
             let lid = 
	       find_empty 
		 String2Map.find
		 (ig1,s1) 
		 contact.Pb_sig.link_of_site 
		 [] in 
	     let sid = 
	       list_fold 
                 (fun (i2,_,s2) sol ->
                    let b =  AL((i1,ig1,s1),(i2,s2)) in 
		      AE.K.E.V.varset_add (AE.K.E.V.var_of_b b) sol)
		 lid AE.K.E.V.varset_empty in 
	     let sid = AE.may_be_true (StringMap.find i1 sb) sid in 
	     let sb',_ = 
	       apply_control_list 
		 pb case 
                 (AE.K.E.V.fold_vars 
		    (fun a sol -> (AE.K.E.V.b_of_var a,false)::sol)
		    sid [])
                 sb f potential_binding in 
	     sb',(
	     	     let fadd b sl = 
		       match b with 
			   AL((id,gid,s),(c,d)) -> 
			     let old = try (StringMap.find c sl) with Not_found -> [] in 
                               StringMap.add c (AL((c,c,d),(gid,s))::old) sl
			 |	 _ -> sl in 
		       
		       AE.K.E.V.fold_vars (fun v l   -> fadd (AE.K.E.V.b_of_var v) l)
			 sid l))
	  control
	  (sb,StringMap.empty) 
	

	
	    
      let apply_control_remove (b_of_id,case) s sb = 
	(*let specie_of_id = case.specie_of_id in*)
	list_fold StringMap.remove s sb,
	let set = list_fold 
	  (fun id l -> 
	    let lid = b_of_id id in 
	    let sid = 
	      list_fold 
                (fun b sol -> 
		  match b with AL(_,_) -> 
		    AE.K.E.V.varset_add (AE.K.E.V.var_of_b b) sol | _ -> sol)
		lid AE.K.E.V.varset_empty in 
	    let sid = AE.may_be_true (StringMap.find id sb) sid in 
	    let fadd b m = 
	      match b with 
		AL((id,gid,b),(c,d)) -> 
		  let a=gid in 
		  let old = try (StringMap.find c m) with Not_found -> [] in 
		  StringMap.add c (AL((c,c,d),(a,b))::old) m
	      |	 _ -> m in 
	    
	    AE.K.E.V.fold_vars (fun v l   -> fadd (AE.K.E.V.b_of_var v) l)
	      sid l)
	  s StringMap.empty in 
	  set

      let apply_control_add pb case sb f  potential_binding = 
	sb,potential_binding
	  
      let close_semi_links  sb l = (* TO DO CORRECT THE BUG *)
	StringMap.fold 
	  (fun a l sb -> 
	    let old = StringMap.find a sb in
	    let new' = 
	      list_fold 
		(fun b current -> 
		  let m = 
		    match b with AL((a1,ga1,s1),(a2,s2)) -> 
		      AE.K.E.V.varmap_add 
			(AE.K.E.V.var_of_b b) false
			(AE.K.E.V.varmap_add
			   (AE.K.E.V.var_of_b (B(a1,ga1,s1))) false
			   AE.K.E.V.varmap_empty)
		    | _ -> AE.K.E.V.varmap_empty
		  in 
		  AE.union current (AE.set 
				      (AE.conj  
					 current 
					 (AE.atom_pos 
					    (AE.K.E.V.var_of_b b) 
					    ))


					 m))
		l old in 
	    if old == new' then sb 
	    else StringMap.add a new' sb)
	  l sb 
      

      let instancie_rule case sb g sp = 
	let _ = trace_print "BEGIN INSTANCIE" in 
	let _ = List.iter
             (fun a -> List.iter 
		(fun x -> trace_print (match x.r_simplx.Rule.flag
                                       with None -> "" | Some a -> a))
                a.labels)
                
	     case.rules in 
	
	try (Some(
	let fadd k expr map = 
	  if !Config_complx.build_relationships_among_species then 
	    begin
	      let k' = g k in 
	      try (
		let expr0 = StringMap.find k' map in 
		StringMap.add k' (AE.conj expr expr0) map)
	      with Not_found -> StringMap.add k' expr map
	    end
	  else
	    StringMap.add (g k) expr map
	in
	  (list_fold 
	     (fun (a,b) sol  -> 
		let s0 = StringMap.find b sp in 
		let s1 = try (AE.union s0 (StringMap.find b sb)) with _ -> s0 in 
	       fadd a  
		 (let ae = 
		   if a=b 
		   then s1
		   else 
		     AE.increasing_renaming 
		       s1
		       (fun _ -> a) in ae)
		 sol)
      	     case.id
	     StringMap.empty)))
	with Not_found -> (error "line 297" "BUG" "instancie_rule" None)
	  
	  
      let desinstancie_rule case sb' b_of_id sb f sp control = 
	list_fold 
	  (fun ((a:string),(b:string)) sol  -> 
(*	    if not (List.exists (fun r -> StringSet.mem a r.target) case.rules) 

	    then sol
	    else *)
	      let ae = 
		try (Some (StringMap.find b sol))
		with Not_found -> None  in 
	      try (
		let newae = 
		  if a=b then (AE.conj (AE.list_conj [H(f a,b),true]) (StringMap.find (f a) sb')) 
		  else	 
		    (AE.increasing_renaming 
		       (let nae = 
			 ((StringMap.find (f a) sb')) in 
			 
		       let nae = AE.conj (AE.list_conj [H(f a,b), true]) nae 
		       in 
		       if !Config_complx.build_relationships_among_species then 
			 (AE.project 
			    nae 
			    (fun x -> (topic_of_b (AE.K.E.V.b_of_var x)) = a))
		       else nae)
		       (fun _ -> b)) in 
		match ae 
		with Some ae' when  AE.is_included newae ae' -> sol 
                | Some ae' -> 
		    StringMap.add b (AE.union ae' newae) sol
		| None -> StringMap.add b newae sol)
	      with Not_found -> sol)
	  case.id
	  sb
	  
      let print_sb sb pb (log:out_channel option) = 
	StringMap.fold  (fun a b sol -> 
	  print_option empty_prefix log ("\n AGENT "^a^"\n\n");
	  let rep = AE.print b a pb tuple_known  (Some "()") log in 
	  (a,rep)::sol
	    ) sb []
	  
      let apply_rule abstract_lens id pb case inv  guard (sb,potential_binding)  sp = 
	let control = case.control in 
	let b_of_id = case.b_of_id in 
	let a = instancie_rule case sb relation sp in 
	match a with None -> (RuleIdListMap.add id None abstract_lens,(false,potential_binding),sb)
	| Some a -> 
	    let _ = trace_print "INSTANCIED3" in 
	    let a = if !Config_complx.refine_after_instanciation then inv a (fun x -> x) else a in 
	    let _ = trace_print "GUARD" in 
	    let rep = (teste_rule pb case guard potential_binding  a  relation) in 
	    let abstract_lens = RuleIdListMap.add id rep abstract_lens in 
	    match rep
	    with None -> (abstract_lens,(false,potential_binding),sb)
	      |Some b ->  
		begin
		  let _ = trace_print "GUARD2" in 
		  let b = if !Config_complx.refine_after_guard then inv b relation else b in
		  let b,pot_b = apply_control_add pb case b relation potential_binding in 
		  let c,pot_b = apply_control_context_update pb case  b  relation pot_b  in 
		  let c,semi_links1 = apply_control_uncontext_update pb case c relation potential_binding in 
		  let _ = trace_print "CONTROL" in 
		  let c = if !Config_complx.refine_after_control then inv c relation else c in 
		  let d,semi_links2 = apply_control_remove 
   ((fun x -> StringMap.find x b_of_id),case)
   control.remove 
   c in 
		  let e = desinstancie_rule  case d  b_of_id sb relation sp control in 
		  let _ = trace_print "DESINTANCIED" in 
		  let (b,pot_b),sb = 
		    if e==sb or forall2 (fun _ _  -> false) (fun _ _ -> true) (fun _ x y -> AE.is_included x y && AE.is_included y x) e sb 
		    then ((false,potential_binding),sb)
		    else ((true,pot_b),e) in 
		  let sb2 = close_semi_links sb semi_links1   in 
		  let sb2 = close_semi_links sb2 semi_links2 in 
		  if b then 
	    abstract_lens,(b,pot_b),sb2
	  else if sb==sb2 or forall2 (fun _ _ -> false) (fun _ _ -> true) (fun _ x y -> AE.is_included x y && AE.is_included y x) sb sb2 then 
	    abstract_lens,(false,pot_b),sb
	  else 
	    abstract_lens,(true,pot_b),sb2
	end


      let marks_of_site cpb (a,x) = 
	match cpb.cpb_mark_site with
	  None -> cpb.cpb_marks
	| Some map  -> 
	    try String2Map.find (a,x) map
	    with Not_found -> (print_string "M";print_string a;print_string x;[])

      let binding_of_site cpb (a,x) = 
	match cpb.cpb_contact with 
	  None -> 
	    List.fold_left
	      (fun sol (a,b,c) -> 
		List.fold_left 
		  (fun sol x -> 
		    (a,x)::sol)
		  sol c)
	      [] 
	      cpb.cpb_interface
	| Some map -> 
	    try String2Map.find (a,x) map 
	    with Not_found -> (print_string "INT";print_string a;print_string x;print_newline ();[]) 

      let init_false pb = 
	let cpb = 
	  match pb.intermediate_encoding with 
	    None -> error "line 410" "intermediate encoding is missing" "init_false"  (raise Exit)
	  | Some a -> a in 
	list_fold 
	  (fun (a,b,c) sol -> 
	    StringMap.add a 
	      (AE.list_conj 
		 (list_fold 
		    (fun b l1 ->  
		      list_fold 
			(fun m l2 ->
			  ((M((a,a,b),m),false)::l2))
			(marks_of_site cpb (a,b)) 
                        (l1)
			)
		    b
		    (list_fold 
		       (fun c l2 -> 
			 list_fold 
			   (fun (a',c') l3 -> 
			     let b = (AL((a,a,c),(a',c')),false) in 
	      		      b::l3)
			   (binding_of_site cpb (a,c))
			   ((B(a,a,c),false)::l2))
                     c [H(a,a),false])))  sol)
	  cpb.cpb_interface
	  StringMap.empty
	    
      let init pb = 
	let cpb = 
	  match pb.intermediate_encoding with 
	    None -> error "line 440" "intermediate_encoding is missing" "init" (raise Exit)
	  | Some a -> a in 
	let init = 
	  match pb.boolean_encoding with 
	    None -> 
	      (match pb.gathered_boolean_encoding with 
		None -> None
	      |Some a ->  a.init)
	  | Some a ->  a.init in 
	match init with None -> 
	  begin
	    list_fold 
	      (fun (a,b,c) sol -> 
		StringMap.add a 
		  (AE.list_conj 
		     (list_fold 
			(fun b l1 ->  
		      list_fold 
			    (fun m l2 ->
			      if m="u" 
			      then ((M((a,a,b),m),true)::l2)
			      else ((M((a,a,b),m),false)::l2))
			    (marks_of_site cpb (a,b))
                            l1)
			b
		    (list_fold 
		       (fun c l2 -> 
			 list_fold 
			   (fun (a',c') l3 -> 
			     let b = (AL((a,a,c),(a',c')),false) in 
	      			 b::l3)
			   (binding_of_site cpb (a,c))
			   ((B(a,a,c),false)::l2))
                       c [H(a,a),true])))  sol)
	      cpb.cpb_interface
	      StringMap.empty,contact_map_init
	  end
	|  Some init -> 
	    let init_def = 
	      list_fold 
		(fun (a,b,c) sol -> 
		  StringMap.add a 
		    (list_fold 
		       (fun b l1 ->  
			 list_fold 
			   (fun m l2 -> BMap.add (M((a,a,b),m)) false l2)
			   (marks_of_site cpb (a,b))
                           l1)
		       b
		       (list_fold 
			  (fun c l2 -> 
			    list_fold 
			      (fun (a',c') l3 -> 
				BMap.add (AL((a,a,c),(a',c'))) false l3)
				
			      (binding_of_site cpb (a,c))
			      (BMap.add (B(a,a,c)) false l2))
			  c (BMap.add (H(a,a)) false BMap.empty)))  sol)
		cpb.cpb_interface
		StringMap.empty in
	    let init_f = StringMap.empty in 
	    let fadd a new' m = 
	      try (let old = StringMap.find a m in 
	      StringMap.add a (AE.union old new') m) 
	      with Not_found -> StringMap.add a new' m in 
	    let init = 
	      list_fold
		(fun (a,s) (sol,pot_bind) -> 
		  let def = StringMap.find a init_def in 
		  let m = list_fold (fun (b,bool) -> BMap.add b bool) s def in
		  let pot_bind = 
		    list_fold 
		      (fun (b,bool) pot_bind -> 
			match b,bool with 
			  AL((a,ga,sa),(gb,sb)),true -> 
			    add_contact cpb.cpb_with_dots (ga,sa) (gb,sb) pot_bind
			| _ -> pot_bind)
		      s pot_bind in 
		  let ae = 
		    AE.list_conj 
		      (BMap.fold (fun b bool l -> (b,bool)::l) m [])
		  in 
		  fadd a ae sol,pot_bind)
		init 
		(init_f,
		 contact_map_init) in 
	    init
		  
	  
     let print_subviews rep pb = 
       let output =  open_file (!Config_complx.output_pack_value_file) in 
       let pack_val  = print_sb rep pb output in
       let _ = close_file output in 
       pack_val

     let print_result (rep,potential_binding) pb  = 
       let output = open_file (!Config_complx.output_specie_map) in 
       let print s = print_option empty_prefix output s in 
       let specie_map = 
	   StringMap.fold
	     (fun k ae sol -> 
	       let _ = print "AGENT: " in 
	       let _ = print (k^"\n\n") in 
	       let rep = AE.print_reachable_states (AE.reachable_states_of_abstract_expr  ae)  (fun k -> k) pb (None)  output in
	       let _ = print "\n" in 
	       (k,rep)::sol)
	     rep [] in 
       let _ = close_file output in  
       (specie_map)

      let inv a e = a 

      let itere pb messages = 
        let system  = 
	  match pb.boolean_encoding with 
	    None -> 
	      (match pb.gathered_boolean_encoding with 
		None -> error "line 557" "gathered boolean encoding is missing" "itere" []
	      |Some a ->  a.system)
	  | Some a ->  a.system in 
	let _ = trace_print "PARSED_sig" in 	
	let abstract_lens = RuleIdListMap.empty in 
	let s0,(potential_binding:contact_map) = init pb in 
	let sp = init_false pb in 
	let s0 = StringMap.map2iz 
	             (fun _ -> AE.union) s0 sp in 
	let _ = trace_print "INIT" in
	let _ = if !Config_complx.trace then let _ = print_sb s0 pb (Some stdout) in ()  in 
	let _ = trace_print "PARSED_rule" in 
	let system = 
	  list_fold 
	    (fun case sol -> 
	       let lid = 
		 list_fold 
		   (fun r sol -> 
		      let llab = 
			list_fold 
			  (fun id sol -> if id.r_clone then sol else id::sol)
			  r.labels [] 
		      in 
			if llab = [] then sol else {r with labels = llab}::sol)
		   case.rules [] 
	       in
		 match lid with [] -> sol 
		   |_ -> {case with rules = lid}::sol)
	    system [] in 
	       
	let rec aux (abstract_lens,s,potential_binding) = 
	  if !Config_complx.trace_iteration_number then (print_string "iteration";print_newline ();flush stdout);
	  let abstract_lens,s,b =
	    list_fold
	      (fun case (abstract_lens,sb,bool) -> 
		list_fold

		  (fun rule (abstract_lens,sb,b) -> 
		   let g = rule.injective_guard in 
		   let abstract_lens,(b',pt),sb' = 
		     apply_rule 
		       abstract_lens 
		       rule.labels 
		       pb 
		       case 
		       inv 
		       (List.rev_map (fun (a,b) -> AE.K.E.V.var_of_b a,b) g) (sb,snd b)  sp in
		    if !Config_complx.trace_rule_iteration (*&& b' *)
		    then (let _ = list_fold  (fun r b -> print_string (if b then (","^(name_of_rule r)) else (name_of_rule r));true) rule.labels false in 
		    print_string " updated";print_newline ();flush stdout)
			;
		    (abstract_lens,sb',(b' or (fst b),pt))) 
		  case.rules 
		  (abstract_lens,sb,bool)
	      )
	      system 
	      (abstract_lens,s,(false,potential_binding)) in
	  if (fst b) then (aux (abstract_lens,s,snd b)) else  (abstract_lens,s,snd b)
	in 
	let rep = aux (abstract_lens,s0,potential_binding) in 
	rep,sp,pb,messages
	  

      let export pb (rep,potential_binding)  sp messages = 
	 let system1  = 
	  match pb.gathered_boolean_encoding with 
	    None -> []
	  | Some a -> a.system in 
	 let system2 = 
	    match pb.boolean_encoding with 
	    None -> []
	  | Some a -> a.system in 
	 let f x  = 
	   List.rev_map 
	     (fun case -> 
               let a = instancie_rule case rep (fun x -> x) sp in 
	       let _ = trace_print "INSTANCIED" in 
	       let g = 
		 match case.vars,smart_lens  with None,_ | _,false  -> (fun _ -> true)
		 |	Some a,true -> (fun x -> BSet.mem x a) in
	
	       match a with 
		 None -> StringMap.empty 
	       | Some a -> 
		   
		   let a = if !Config_complx.refine_during_export then inv a (fun (x:StringMap.key) -> x) else a in 
		   StringMap.map 
		     (fun a -> 
		       let _ = trace_print "CONJ" in 
                       (AE.reachable_states_of_abstract_expr 
			  (AE.project 
			     ((*AE.conj*) a (*(AE.list_conj [H(b,b),true])*))
			     (fun x -> 
			       let y = AE.K.E.V.b_of_var x in  
	
			       match y with  H _ | B _  | M _ | AL _ -> g y | _ -> false))))
		     a)
	     x in 
	let _ = trace_print "END_EXPORT" in  
	 (Some {potential_binding  with relation_list = List.sort compare potential_binding.relation_list}
	    
	  ,f system1,f system2),messages

      let refine_boolean_encoding system pb (rep,potential_binding)  sp messages = 
	 let f x  = 
	   List.rev_map (fun case -> 
            let a = instancie_rule case rep (fun x -> x) sp in 
	    let _ = trace_print "INSTANCIED" in
	    let g = 
	      match case.vars,smart_lens with None,_ | _,false -> (fun _ -> true)
	      |	Some a,_ -> (fun x -> BSet.mem x a) in
	    let rep = 
	    match a with 
	      None -> StringMap.empty
	    | Some a -> 
		
		let a = if !Config_complx.refine_during_export then inv a (fun (x:StringMap.key) -> x) else a in 
		let rep = 
		  StringMap.map 
		      (fun a  -> 
			 let _ = trace_print "CONJ" in 
                   (* AE.r_conj*) (AE.reachable_states_of_abstract_expr 
				 (AE.project a 
				    (fun x -> 
				      let y = AE.K.E.V.b_of_var x in
				       match y with  H _ | B _  | M _ | AL _ -> 
					 g y | _ -> false))) (*expr*))
		      a (*AE.r_ae_true*) 
		in rep in 
	    	  {(case:'a Pb_sig.rule_class)
		   with Pb_sig.abstract_lens = Some (rep)})
	     x in 
	   f system,messages

      let refine_pb rep pb sp messages = 
         let (c,r1,r2),messages  = export pb rep sp messages in
       	 let refine_pb (c,r1,r2)  pb = 
            {{
	      {pb 
	      with contact_map = c }
             with gathered_boolean_encoding  = 
		    match pb.gathered_boolean_encoding with 
		      None -> None 
		    | Some a -> Some {a with system = 
				  List.map2 (fun a b -> {a with abstract_lens = Some b}) a.system (List.rev r1)}}
		 
		 
	    with boolean_encoding = 
	      match pb.boolean_encoding with 
		None -> None 
	      | Some a -> Some {a with system = 
			    List.map2 (fun a b -> {a with abstract_lens = Some b}) a.system (List.rev r2)}} in 
         let pb = refine_pb (c,r1,r2) pb in 
         pb,messages
	
      let dump_concretization  pb (rep,possible_links) messages = 
	let prelist = 
	  StringMap.fold 
	    (fun k ae l -> 
	      let tmp = AE.export_ae (AE.conj (AE.list_conj [H(k,k),true]) ae)
                         k pb tuple_init in 
	      tmp::l) (fst rep) []
	 in prelist,messages
    end)
