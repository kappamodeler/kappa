(* 22/02/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Intermediar code *)
(* cbng *)

open Pb_sig
open Data_structures
open Tools 
open Config_complx 
open Cbng_sig
open Error_handler 


let error i x (*t*) y = 
    unsafe
      (Some x) 
      (Some "Complx")
      (Some "cbng.ml") 
      None (*(Some t)*) 
      (Some (string_of_int i)) 
      y

let error_frozen i x (*t*) y = 
    unsafe_frozen
      (Some x) 
      (Some "Complx")
      (Some "cbng.ml") 
      None (*(Some t)*) 
      (Some (string_of_int i))
      y

module CBnG = 
  (struct 
    type  rule_internal_representation = 
	{id_list: id list;
	 id_mapping: id -> string*string;
	   agent_mapping: string -> id list;
	     rule:cpb_rule;
       }
	  
    let keep_id = false

    let rule_precomputation  pb  rule = 
      let _ = trace_print "BEGIN PRECOMPUTATION" in 
      let l,m0,m1,c = 
	list_fold (fun (i,s) (l,m',m'',c) -> 
	  let c' = if keep_id then i else 
	  try (StringMap.find s c)
	  with Not_found -> 1 in
	   (i::l,
	    IntMap.add i (s^"%"^(string_of_int c'),s) m',
	    StringMap.add s 
	      (i::(try (StringMap.find s m'') with Not_found -> [])) m'', 
	    StringMap.add s (c'+1) c))
	   rule.cpb_r_species 
	   ([],IntMap.empty,StringMap.empty,StringMap.empty)
       in 
       let m0 = if keep_id then m0 else 
	 StringMap.fold 
	   (fun s c m -> 
	     if c=2 
	     then 
	       (try (IntMap.add 
		 (match StringMap.find s m1 
                  with [i] -> (i:int) 
                       | _ -> (error 65 "PRECOMPUTATION_CBNG" (-1) ))
                 ) 
                with Not_found -> (error 67 "BUG_PRECOMPUTATION_CBNG" (fun  x y -> m))) 

             (s,s) m 
	     else m) c m0 in 
       let f0 i = try (IntMap.find i m0) with Not_found -> (error 71 ("F0_CBNG.ml"^(string_of_int i)) ((string_of_int i),(string_of_int i))) in 
       let am = 
	 IntMap.fold (fun i (s1,s2) sol -> 
	   let old = try (StringMap.find s2 sol) with Not_found -> [] in 
	   StringMap.add s2 (i::old) sol) 
	   m0 StringMap.empty in 
       let fm i = try (StringMap.find i am)  with Not_found -> (error 77 "FM_CBNG.ml" []) in  
       {id_list=l;
        id_mapping=f0;
	agent_mapping=fm;
	rule=rule}

   

    let translate_test pb irule t (sol,map) = 
      let fadd (i,b) m map =
	let old = try (IntStringMap.find (i,b) map) with Not_found -> StringSet.empty  in 
	  IntStringMap.add (i,b) (StringSet.add m old) map in 
      match t with 
	Is_connected (i,j) -> 
	  (Connected(irule.id_mapping i,irule.id_mapping j),true)::sol,map
      |	Is_here i -> let i,ig = irule.id_mapping i in 
	             (H(i,ig),true)::sol,map
      | Is_bound (i,s) -> 
	  let i',ig' = irule.id_mapping i in 
	  (*(H(i',ig'),true)::*)(B(i',ig',s),true)::sol,map
      | Is_free  (i,s) -> 
	  let i',ig' = irule.id_mapping i in 
	
	    ((*(H(i',ig'),true)::*)(B(i',ig',s),false)::sol),map
      |	Is_disjoint (i,i') ->
	  let i,ig = irule.id_mapping i in
	  let i',ig'= irule.id_mapping i' in 
	  (Dis((i,ig),(i',ig')),true)::sol,map 
      |	Is_forbid(i,l) -> 
	    let i,ig = irule.id_mapping i in
	    (Forb((i,ig),l),true)::sol,map 
	      
      | Is_related ((i1,s1),(i2,s2)) -> 
	  let i1',ig1 = irule.id_mapping i1 in 
	  let i2',ig2 = irule.id_mapping i2 in 
	 
	   (    ((l((i1',ig1,s1),(i2',ig2,s2)),true)(*::(H(i1',ig1),true)::(H(i2',ig2),true)*)::(B(i1',ig1,s1),true)::(B(i2',ig2,s2),true)::sol),map
	   )
      | Is_marked ((i,s),m) -> 
	 let i',ig = irule.id_mapping i in 
	 ((*(H(i',ig),true)::*)(M((i',ig,s),m),true)::sol),(fadd (i,s) m map)
      


	   
    let translate_control_update pb irule map cpb_map t  (context_control,uncontext_control) = 
    match t with 
      No_Pol | No_Helix -> (context_control,uncontext_control) 
    | Bind ((i1,s1),(i2,s2)) -> 
	let i1',ig1 = irule.id_mapping i1 in 
	let i2',ig2 = irule.id_mapping i2 in 
	  (AL((i1',ig1,s1),(ig2,s2)),true)::(AL((i2',ig2,s2),(ig1,s1)),true)::(l((i1',ig1,s1),(i2',ig2,s2)),true)::(B(i1',ig1,s1),true)::(B(i2',ig2,s2),true)::context_control,uncontext_control
    | Mark ((i,s),m) -> 
	let i',ig = irule.id_mapping i in 
	  StringSet.fold  
	    (fun m' sol -> (M((i',ig,s),m'),m'=m)::sol)
	    (StringSet.add m 
	       (try (IntStringMap.find (i,s) map) with _ -> 
		  try (String2Map.find (ig,s) cpb_map) 
		  with Not_found -> StringSet.empty))
            context_control,uncontext_control
    | Release ((i1,s1),(i2,s2)) ->
	let i1',ig1 = irule.id_mapping i1 in 
	let i2',ig2 = irule.id_mapping i2 in 
	  (AL((i1',ig1,s1),(ig2,s2)),false)::(AL((i2',ig2,s2),(ig1,s1)),false)::(l((i1',ig1,s1),(i2',ig2,s2)),false)::(B(i1',ig1,s1),false)::(B(i2',ig2,s2),false)::context_control,uncontext_control
    | Check_choice _ | Check _ -> context_control,uncontext_control
    | Break_half(i1,s1) -> 
	let i1,ig1 = irule.id_mapping i1 in 
          (B(i1,ig1,s1),false)::context_control,(i1,ig1,s1)::uncontext_control																				    
	    

    let translate_control_remove pb irule i sol = (fst (irule.id_mapping i))::sol

    let translate_control_create pb irule i sol = (irule.id_mapping i)::sol 

    let order  (a,b)  = 
      if compare a b < 0 
      then (a,b)
      else (b,a)
				

    let make_commutation r ir = 
      let l = list_fold
	  (fun x q -> 
	    match x with 
	     Bind((i1,s1),(i2,s2)) -> 
		(
		 match compare (snd (ir.id_mapping i1),s1) (snd (ir.id_mapping i2),s2) 
		 with 1 -> 
		   list_fold 
		     (fun (id,l) sol -> (id,((Bind((i1,s1),(i2,s2)))::l))::sol)
		     q [] 
		   
		 |  0 -> 
		     list_fold 
			 (fun 
			   (id,l)
			     sol -> ((id,((Bind((i1,s1),(i2,s2))))::l))::sol)
			 q 
			 []

		 |  -1 ->
		     list_fold 
		       (fun (id,l) sol -> (id,((Bind((i2,s2),(i1,s1)))::l))::sol)
		       q []
		| _ -> error 179 "COMPARE" (list_fold 
		     (fun (id,l) sol -> (id,((Bind((i1,s1),(i2,s2)))::l))::sol)
		     q []))
	    |  Release((i1,s1),(i2,s2)) -> 
	      (
	       match compare (snd (ir.id_mapping i1),s1) (snd (ir.id_mapping i2),s2) 
	       with 1 -> 
		 list_fold 
		   (fun (id,l) sol -> (id,((Release((i1,s1),(i2,s2)))::l))::sol)
		   q [] 
		   
	       |  0 -> 
		    if (i1,s1)=(i2,s2) or (not (!Config_complx.duplicate_rules_when_sym))
		    then
		      list_fold 
			(fun 
			  (id,l)
			    sol -> ((id,((Release((i1,s1),(i2,s2))))::l))::sol)
			q 
			[]
		    else
		      list_fold
			(fun ((id,b),l) (sol) -> 
			  let id1 = 
			    if (not b) then (id,b) 
			    else (id^"_",true) in 
			  
			  ((id1,((Release((i1,s1),(i2,s2)))::l))::
                           ((id^"\"",true),((Release((i2,s2),(i1,s1)))::l))::
                           sol))
			q []
	       |  -1 ->
		   list_fold 
		     (fun (id,l) sol -> (id,((Release((i2,s2),(i1,s1)))::l))::sol)
		     q []
	       | _ -> error 214 "COMPARE\n"  (list_fold 
		   (fun (id,l) sol -> (id,((Release((i1,s1),(i2,s2)))::l))::sol)
		   q []))
	    | Mark _ | Break_half _ | Check _  | No_Pol | No_Helix  -> 
		list_fold 
		  (fun (id,l) sol -> (id,x::l)::sol)
		  q [] 
	    | Check_choice(list) -> 
		if (!Config_complx.duplicate_rules_when_sym)
		then 
		  list_fold 
		    (fun ((id,b),l) sol -> 
		      
		      fst (list_fold 
			 (fun i (sol,b) -> (
			   ((if (not b)  then (id,b) else (id^"."^(string_of_int i),true)),((Check i))::l)::sol,true))
			 list 
			 (sol,b))) q []
		else 
		  list_fold 
		  (fun (id,l) sol -> 
                    (id,list_fold (fun i sol -> (Check(i)::sol)) list l)::sol)
		  q [])


	  r.cpb_control.cpb_update  [("",false),[]]  
      in 
      List.map (fun ((a,b),c) -> ((a,b),{r with cpb_control ={r.cpb_control with cpb_update = c}})) l

    let gen_order o ir = 
      let rename_agent i = snd (ir.id_mapping i) in 
      let rename_site (i,s) = (rename_agent i,s) in 
      match o with 
	Bind(s1,s2)    -> 
	  let (a,b) = order (rename_site s1,rename_site s2) in 
	  GBind(a,b)
      | Mark(s,m)          -> GMark(rename_site s,m)
      | Release(s1,s2) -> 
	  let (a,b) = order (rename_site s1,rename_site s2) in 
          GRel(a,b)
      | Break_half(s) -> GRel_half(rename_site s)
      |	No_Pol -> GNo_Pol
      |	No_Helix -> GNo_Helix 
      |	Check(i) -> GCheck(rename_agent i)
     (* | Disjoint (i,i') -> GDisjoint(rename_agent i,rename_agent i')
      |	Forbid (i,s) -> GForbid(rename_agent i,s)*)
      |	_ ->  error_frozen 260 "Unknown control command in Cbng.gen_order" (fun () -> raise Exit)
      
	    
	    
	    
    let prehash r ir = 
      (List.sort compare (list_map (fun a -> gen_order a ir) r.cpb_control.cpb_update),
       List.sort compare (IntSet.fold  (fun a s -> (snd (ir.id_mapping a))::s) r.cpb_control.cpb_remove []),
       List.sort compare (IntSet.fold  (fun a s -> (snd (ir.id_mapping a))::s) r.cpb_control.cpb_create []))
    
    let print_prehash (l,l2,l3) = 
      let print_agent a = print_string a in 
      let print_site (a,s) = print_agent a;print_string ",";print_string s in 
      let print_site_pair (a,b) = print_site a;print_string ",";print_site b in
      begin
	(List.iter 
	   (fun x -> 
	     match x with 
	       GNo_Pol -> print_string "No_Pol;"
	     | GNo_Helix -> print_string "No_Helix;"
	     | GBind x -> (print_string "B";
                           print_site_pair x;
			   print_string";")
		   
		   
	     | GMark (a,m) -> (print_string "M";print_site a;print_string ("~"^m))
	     | GRel(x) -> (print_string "R";
			   print_site_pair x;print_string ";")
	     | GRel_half a -> (print_string "R1/2";print_site a)
	     |	GCheck a -> (print_string "CH";print_agent a)) l;
	 print_newline ())
      end

    let compare_id a b = 
      let get_id l = fst(fst l) in 
      compare (get_id a) (get_id b)
	  
    let sort_control (r,ir)   =
      List.sort 
	(fun a b -> 
        let rep = compare (prehash (snd (fst a)) ir) (prehash (snd (fst b)) ir) in 
	if rep = 0 
	then compare_id a b else rep)
	(List.fold_left 
	   (fun l s -> (s,ir)::l)
	   [] (make_commutation r ir))
     
    let order_control l = 
       (List.sort 
	     (fun a b -> 
	       match a,b with 
		 Check i,Check j -> compare i j 
	       | Check i,_ -> -1
	       | _,Check i -> 1
	       | _ -> 0) l)  

    let compute_renaming_rule r   = 
      let _ = trace_print "Compute_renaming_rule" in
      let rename_var i (f,n) = 
	try 
	  (IntMap.find i f;f,n) 
	with Not_found -> 
	       (IntMap.add i n f,n+1) in 
      let c1 = list_fold
	  (fun t c -> 
	    (match t with 
	      Release((i1,_),(i2,_)) | 
	      Bind((i1,_),(i2,_))  -> 
		if i1 < i2 then 
		rename_var i2 (rename_var i1 c)
		else
		  rename_var i1 (rename_var i2 c)
	    | Mark((i,_),_) ->  rename_var i c
	    | Check i -> rename_var i c
	    | Break_half (i,_) -> rename_var i c
	    | No_Pol | No_Helix -> c 
            | _ -> error_frozen  336 "Unknown control command in Cbng.compute_renaming" (fun () -> raise Exit)))
	  (order_control r.cpb_control.cpb_update) (IntMap.empty,0)
      in 
      let c2 = IntSet.fold rename_var r.cpb_control.cpb_remove c1 in 
      let c3 = IntSet.fold rename_var r.cpb_control.cpb_create c2 in 
      let _ = trace_print "END COMPUTE_RENAMING_RULE" in
      Some c3
		  
    	
    let apply_renaming_control  r f = 
      {{{r.cpb_control 
	with cpb_update = 
	  list_map (fun x -> 
	    match x with 
	      Release((i1,s1),(i2,s2)) -> Release((f i1,s1),(f i2,s2))
	    | Bind((i1,s1),(i2,s2)) -> Bind((f i1,s1),(f i2,s2))
	    | Mark((i,s),m) -> Mark((f i,s),m)
	   | Check(i) -> Check(f i)
	   | Break_half(i1,s1) -> Break_half(f i1,s1)
	   | No_Pol | No_Helix -> x 
           | _ -> error_frozen  356 "Unknown control command in Cbng.apply_renaming_control" (fun () -> raise Exit))
	    (order_control r.cpb_control.cpb_update)}
       with 
	cpb_remove = IntSet.fold (fun i s -> IntSet.add (f i) s) r.cpb_control.cpb_remove IntSet.empty}
       with cpb_create = IntSet.fold (fun i s -> IntSet.add (f i ) s) r.cpb_control.cpb_create IntSet.empty}


    let rename_test f x = 
      match x with 
	Is_here i -> Is_here (f i)
      |	Is_connected(i,j) -> Is_connected(f i,f j)
      | Is_bound (i,s) -> Is_bound(f i,s)
      | Is_free  (i,s) -> Is_free(f i,s)
      | Is_related ((i1,s1),(i2,s2)) -> Is_related((f i1,s1),(f i2,s2))
      | Is_marked ((i,s),m) -> Is_marked((f i,s),m)
      |	Is_disjoint(i,j) -> Is_disjoint(f i,f j)
      |	Is_forbid (i,l) -> Is_forbid(f i,l)

	
    let apply_renaming_full r f = 
      {
      cpb_quarks = r.cpb_quarks ;
      cpb_equal = List.map (fun (x,y) -> f x,f y) r.cpb_equal;
      cpb_r_species=List.map (fun (i,s) -> (f i,s)) r.cpb_r_species;
       cpb_passive=
       (match r.cpb_passive with None -> None
       | Some l -> Some (List.map (fun ((i1,s1),(i2,s2)) -> ((f i1,s1),(f i2,s2))) l));
       cpb_guard =
       List.map 
	 (fun (a,c,b) -> (a,IntSet.fold (fun x -> IntSet.add (f x)) c IntSet.empty,
List.map (rename_test f) b)) r.cpb_guard;
       cpb_control = apply_renaming_control r f;
  cpb_dots = r.cpb_dots}

    let test_iso r' (f',n') r (f,n) = 
      (n=n') && 
      (apply_renaming_control r (fun i -> IntMap.find i f)= 
       apply_renaming_control r' (fun i -> IntMap.find i f'))
	
	
    let build_passive lr  (pb:'a cpb) = 
      match lr with [] -> None 
      | t::q -> let _ = trace_print "BD PASSIVE" in 
	  begin
	    let ir = rule_precomputation pb t in 
	    let _ = trace_print "PRECOMPUTATION_OK" in 
	    let rootset,targetset  = 
	      list_fold (fun x (set,set')-> 
		match x with 
		  Release((x1,_),(x2,_)) | Bind((x1,_),(x2,_)) -> set,(IntSet.add x1 (IntSet.add x2 set'))
		| Mark((x1,_),_) -> set,set'
		| Check(x1) -> (IntSet.add x1 set,set')
		| Break_half (x1,_) -> set,IntSet.add x1 set'
		| No_Helix | No_Pol -> set,set'
		| _ -> error  409 "Unknown control command in Cbng.build_passive" (set,set'))
		t.cpb_control.cpb_update (IntSet.empty,IntSet.empty) in  
	    let modset = IntSet.union t.cpb_control.cpb_create t.cpb_control.cpb_remove in 
	    let rootset = 
	      IntSet.union rootset modset in 
	    let supportset = 
	      IntSet.union rootset targetset in 
	    let targetset = 
	      IntSet.diff targetset modset in 
	    let fadd x path  (solset,(sol,list))  = 
             let solset'=
               let old = 
		 try  IntMap.find x solset 
		 with Not_found -> 
		   PathSet.empty in 
                 IntMap.add x (PathSet.add path  old) solset in
             let sol',list' = 
               try (IntMap.find x sol;(sol,list)) 
	       with Not_found -> 
		 (IntMap.add x path sol,(x,path)::list)
             in solset',(sol',list') in 
           let bindinginfo = 
             list_fold (fun x bi-> 
	                  match x with Release((x1,s1),(x2,s2)) ->
                            let bi =  
                              if IntSet.mem x1 rootset then bi
                              else fadd x1 [Agent_id x1;Site s1;Site s2;Agent_id x2;Agent_id x2] bi in 
                              if IntSet.mem x2 rootset then bi
                              else fadd x2 [Agent_id x2;Site s2;Site s1;Agent_id x1;Agent_id x1] bi 
                            | _ -> bi
		       )
	       t.cpb_control.cpb_update (IntMap.empty,(IntMap.empty,[])) in 
	   let print_path path = 
	     List.iter 
	       (fun x -> 
		 match x with Agent_id i -> print_int i;print_string " "
		 |  Agent_gen s -> print_string s;print_string " "
		 | Site s -> print_string s;print_string " "
		       ) path in 
	   let annotated_guard = 
	     list_fold 
	       (fun r sol -> 
		  let ir = rule_precomputation  pb r in 
		  let _ = trace_print "PRECOMPUTATION_OK2" in 
		    
                    list_fold 
		      (fun (id,_,g) sol -> 
			 
			 (r,ir,id,g,supportset,
			  (let succ = 
			     list_fold 
			       (fun (_,_,g) map -> 
				  list_fold 
				    (fun x map -> 
				       let fadd x y z = 
					 let l = try (IntMap.find x z) with Not_found -> [] in 
					   IntMap.add x (y::l) z in 
					 match x with 
					     Is_related((x1,s1),(x2,s2)) -> 
					       fadd x1 (x2,(s1,s2))
						 (fadd x2 (x1,(s2,s1)) map)
					   | _ -> map)
				    g map)
			       r.cpb_guard IntMap.empty
			   in 
			     
			   let span = 
                             let initset = bindinginfo in 
                             let rec aux to_see  initset  = 
			       match to_see with 
				   [] -> initset 
				 | (x1,path,p)::q -> 
				     let (a,b) = list_fold 
				       (fun (x2,(s1,s2)) (to_see,initset) -> 
					  if IntSet.mem x2 p or IntSet.mem x2 rootset
					  then to_see,initset
					  else 
					    let path' = Agent_id x2 :: Site s2 :: Site s1 ::path in 
					    (x2,path',IntSet.add x2 p)::to_see,
                                          (fadd x2 path'  initset))
				       
				       (try (IntMap.find x1 succ) with Not_found -> [])
				       (q,initset) in 
				       aux a b  
			     in aux 
			       (IntSet.fold (fun x l -> (x,[Agent_id x;Agent_id x],rootset)::l) rootset [])
			       initset  in 
			   span))::sol) r.cpb_guard sol) 
	       lr [] in
	   let _ = trace_print "ANNOTATED_OK" in 
	   let translate_path p ir = 
	     let rec aux p sol = 
	       match p with
		 (Agent_id x)::t::q -> aux (t::q) ((Agent_gen (try (snd(ir.id_mapping x)) with _ -> (error 502 "id_mapping" (string_of_int x))))::sol)
	       | t::q -> aux q (t::sol)
	       | [] -> List.rev sol
	     in aux p [] in 
	   let allocate_path path (f_path_to_int,f_int_to_path,n) = 
	     try (S4.find path f_path_to_int,(f_path_to_int,f_int_to_path,n))
	     with Not_found ->
	       if trace 
		   then 
		 (print_string "ALLOCATE PATH \n";
	       print_int n;print_string "->";
	       print_path path ;
	       print_newline ());
	       n,(S4.add path n f_path_to_int,IntMap.add n path f_int_to_path,n+1) in 
	   let f_path_to_int,f_int_to_path,renaming,n = 
	     IntSet.fold 
	       (fun x (f,g,h,n) ->
		 let y,(f,g,n) = 
		   allocate_path
		     (translate_path [Agent_id x;Agent_id x] ir) (f,g,n) in 
		 (f,g,IntMap.add x y h,n))
	       rootset (S4.empty,IntMap.empty,IntMap.empty,1) in 
         
	   let f_path_to_int,f_int_to_path,alias,renaming,n = 
             IntSet.fold 
               (fun x (f_path_to_int,f_int_to_path,alias,renaming,n) -> 
		 try 
		   begin 
		     let main_path = 
		       translate_path (IntMap.find x (fst (snd bindinginfo))) ir in 
		     let all_path = 
		       List.map (fun x -> translate_path x ir)
			 (PathSet.elements
			    (IntMap.find x (fst bindinginfo)))  in 
		     
		     let f_path_to_int,f_int_to_path,n = 
		       List.fold_left
			 (fun (f_path_to_int,f_int_to_path,n) path -> 
			   let y,(f_path_to_int,f_int_to_path,n) = allocate_path path (f_path_to_int,f_int_to_path,n) in 
			   (f_path_to_int,f_int_to_path,n))
			 (f_path_to_int,f_int_to_path,n) all_path in 
		     let alias,_ = 
		       List.fold_left
			 (fun (alias,y) x ->
			   let x = S4.find x f_path_to_int in 
			   match y with None -> (alias,Some x)
			   |  Some y' -> ((x,y')::alias,y))
			 (alias,None) all_path in 
		     let renaming = 
		       let x' = try (S4.find main_path f_path_to_int)
		       with Not_found -> (error 552 "renaming473" (-1)) in 
		       let renaming = IntMap.add x x' renaming in 
		       renaming in 
		     (f_path_to_int,f_int_to_path,alias,renaming,n)
		   end
		     
		 with _ -> error 558 "renaming 461" 
		     (f_path_to_int,f_int_to_path,alias,renaming,n))
               (IntSet.diff targetset rootset) (f_path_to_int,f_int_to_path,[],renaming,n)  in 
           

	
	    let _ = trace_print "FPATH_OK" in 
	    let new_guard,f_path_to_int,f_int_to_path,alias,n  =
              list_fold 
		(fun 
		  (r,ir,id,g,sup,span) 
		    (lg,f_path_to_int,f_int_to_path,alias,n) ->
		  let f_path_to_int,f_int_to_path,alias,n = 
		    IntMap.fold
		      (fun _ path (f_path_to_int,f_int_to_path,alias,n) -> 
			snd (PathSet.fold 
			  (fun path (x,(f_path_to_int,f_int_to_path,alias,n)) -> 
			    let y,(f_path_to_int,f_int_to_path,n) = allocate_path (translate_path path ir) (f_path_to_int,f_int_to_path,n) in
			    match x with
			      None -> (Some y,(f_path_to_int,f_int_to_path,alias,n))
			    | Some x  -> (Some x,(f_path_to_int,f_int_to_path,(x,y)::alias,n))) 
			  path 
			  (None,(f_path_to_int,f_int_to_path,alias,n))))
		      (fst span) (f_path_to_int,f_int_to_path,alias,n) 
		       in 
		  let f_path_to_int,f_int_to_path,renaming,n = 
		    list_fold 
		      (fun (x1,ks) (f_path_to_int,f_int_to_path,renaming,n) ->
			let path = translate_path ks ir in 
			let y,f_path_to_int,f_int_to_path,n = 
			  try (S4.find path f_path_to_int,f_path_to_int,f_int_to_path,n) 
			  with Not_found -> 
			    begin
			      (n,
			       S4.add path  n f_path_to_int,
			       IntMap.add  n path  f_int_to_path,
			       n+1)
			    end
			in
			let renaming = IntMap.add x1 y renaming in 
			(f_path_to_int,f_int_to_path,renaming,n))

		      (List.rev (snd (snd span)))      
		      (f_path_to_int,f_int_to_path,renaming,n)
		  in 
		  (((r,ir,id,g,sup,snd span,renaming)::lg),f_path_to_int,f_int_to_path,alias,n))
		(annotated_guard) 
                ([],f_path_to_int,f_int_to_path,alias,n) in 
	    let _ = if trace 
	    then 
	      let _ = print_newline () in
	      let _ = print_string "ALIASES" in
	      let _ = List.iter 
		  (fun (x,y) -> print_int x;print_int y)
		  alias in () in  
	    let _ = trace_print "NG_OK" in 
	     let fr x = try (IntMap.find x renaming) 
	     with Not_found -> (error 615 "FR.CBNG.ml" x) in  
	    Some {
	     cpb_quarks = None ;
	     cpb_r_species=(trace_print "R_SPECIES";
	     IntMap.fold (fun i j sol ->
	       match j with 
		 (Agent_gen x)::_ -> (i,x)::sol
	       | _ -> error 622 "CBNG 518" sol) 
	       f_int_to_path []); 
	     cpb_passive= (trace_print "PASSIVE";
		       Some 
			 (IntMap.fold 
			    (fun i j sol -> 
			      match j 
			      with a::(Site b)::(Site c)::path -> ((i,b),((
									  try (S4.find path f_path_to_int) with _ -> 
									    List.iter 
									      (fun x -> 
										match x with Agent_id i -> print_int i
										|  Agent_gen s -> print_string s;
      | Site s -> print_string s
	    ) path;
  error 637 "CBNG 528" 0),c))::sol 
			      | _ -> sol)
			      f_int_to_path 
			      []));
		   cpb_dots = t.cpb_dots;
	     cpb_guard = (trace_print "GUARD";
			  List.rev_map 
			    (fun (r,ir,id,(g:test list),sup,span1,c) ->
			      (id,IntSet.fold (fun x -> IntSet.add (try (IntMap.find x c) with Not_found -> x)) sup IntSet.empty,
			       List.rev_map 
				 (fun (x:test) -> 
				   rename_test  
				     (fun x-> 
				       (try (IntMap.find x c) with Not_found -> x ))
				     (x:test)) g)) 
			    new_guard);
	     cpb_equal  = 
	     (if trace then (print_string "\nFINAL_ALIAS\n";
	      List.iter 
		(fun (x,y) -> print_int x;print_int y;print_newline ())
		alias) ; alias) ;
	     cpb_control= (trace_print "CONTROL";apply_renaming_control t fr 
)}

	  end


    let fun_of_map f i =  IntMap.find  i f 
    let divide_list bool pb l =  
      let _ = trace_print "START DIVIDE_LIST" in 
      let l = List.map (fun x -> x,rule_precomputation pb x) l in 
      let _ = trace_print "PRECOMPUTATION_OK" in 
      let enrich_l = List.map sort_control l in 
      let _ = trace_print "SORT_OK" in 
      let enrich_l = 
	list_fold 
	  (fun a  -> 
	    list_fold 
	      (fun (((suf,copy1),r),ir) l ->
                 ({r 
		   with cpb_guard = 
		      List.map 
			(fun (a,c,b) -> 
			   (List.map 
			      (fun rid  -> 
				 ({{rid 
				    with r_id = (rid.r_id^suf)} 
				   with r_clone = copy1 or rid.r_clone}))  
			      a),c,
			   b)
			r.cpb_guard},ir)::l) 
	      a) 
	  enrich_l 
	  [] in 
      let _ = trace_print "SUFFIX_OK" in 
      let feed (h,b,ll) = 
	match h,b with 
	  None,None  -> ll
	| Some h,Some b  -> 
	    List.fold_left 
	      (fun sol (f,r,l) -> ((h,l)::sol)) 
	      ll 
	      b 
	| _ -> error 700 "FEED_FUNCTION in CBNG.ml" ll  in 
      let enrich_l = 
	List.sort 
	  (fun (r,ir) (r',ir') -> 
	  compare (prehash r ir) (prehash r' ir'))
	  enrich_l in 
	  
      let rep = 
	List.fold_left 
	  (fun  (old_hash,bucket,list) (r',ir') ->  
	     let _ = trace_print "BEGIN PREHASH" in
	     let h' = prehash r' ir' in 
	     let _ = trace_print "END PREHASH" in 
	     let f' = 
	       match compute_renaming_rule r'
	       with None -> (error_frozen 715 "Compute_renaming_rule has output None" (fun () -> raise Exit))
		 | Some f -> f in 
	       (match old_hash with 
		   None -> (Some h',
			    Some ([ f',r',[r',ir',f']]),
			    [])
		  | Some h -> 
		      if bool & h=h' 
		      then 
			begin
			  let rec aux x vue = 
			    match x with 
				t::q -> 
				  begin
				    match t with 
					f,r,current_list -> 
					  begin
					    if test_iso r f r' f' 
					    then (old_hash,Some ((f,r,(r',ir',f')::current_list)::(q@vue)),list)
					    else aux q (t::vue)
					  end
				  end
			      | [] -> (old_hash,Some ((f',r',[(r',ir',f')])::vue),list)
			  in 
			    match bucket with 
				None -> (old_hash,Some[(f',r',[r',ir',f'])],list)
			      | Some bucket -> aux bucket [] 
			end
		      else
			(Some h',Some ([f',r',[r',ir',f']]),
			 
			 ((feed (Some h,bucket,list))))
			  
	       ))
          (None,None,[]) enrich_l
      in feed rep

 
	   
    let recover_pb (pb:'a cpb) l = 
      list_fold  (fun (h,bucket)  (sol:cpb_rule list) -> 
		    let f' (f,n) i = 
		      (try ((fun_of_map f) i) 
		       with Not_found -> i+n )
		    in 
		      match bucket with 
			  [] -> (sol:cpb_rule list)
			| l -> 
			    let l = List.rev_map (fun ((r,ir,f)) -> apply_renaming_full r (f' f)) l  in 
			    let _ = trace_print "RECOVER RULE" in 
			    match 
			      (let rep = build_passive l pb 
			      in (trace_print "BUILD_PASSIVE_OK";rep))
			    with None -> sol | Some t -> t::sol)
	l []
	
    let smash_pb bool pb messages = 
      let _ = trace_print "ENTERING SMASH" in 
      let _ = 
	(if trace then 
           begin 
	     print_newline (); 
	     print_string "ENTERING SMASH";
	     print_newline ();
	   end) in 
      let l = divide_list bool pb pb.cpb_rules  in 
      let _ = trace_print "END DIVIDE" in 
      let rep =  
	{pb with cpb_rules = recover_pb pb l
	}
      in 
      let _ = trace_print "RETURN SMASH" in 
      let _ = if trace then (print_string "RETURN SMASH";print_newline ()) in 
      let _ = Cbng_sig.cbng_dump "" rep  
      in 	rep,messages 
	  
  

    let sort_pb (s:'a Pb_sig.rule_class list ) = 
      let s' = 
	List.sort 
	  (fun c1 c2 -> 
	     try (compare 
		    (try (List.hd ((List.hd  c1.Pb_sig.rules).labels)).Pb_sig.r_id
		     with _ -> "")
		    (try (List.hd ((List.hd  c2.Pb_sig.rules).labels)).Pb_sig.r_id     with _ -> "")) with _ -> error 800 "Empy rule_class" 0)
	  
	  (List.rev_map 
	     (fun c -> 
		{c with Pb_sig.rules = 
		    List.sort 
		      (fun a a' -> 
			 compare 
			   (match a.labels with t::_ -> t.Pb_sig.r_id | [] -> "")
			   (match a'.labels with t::_ -> t.Pb_sig.r_id | [] -> ""))
		      (
			List.rev_map 
			  (fun r -> {r with labels = List.sort (fun id1 id2 ->compare id1.Pb_sig.r_id id2.Pb_sig.r_id) r.labels})
			  c.Pb_sig.rules)}) s)
      in s'
	   
    let translate_init init = 
      match init 
      with None -> None
	| Some l -> 
	    Some 
	      (List.rev_map 
		 (fun (a,b) -> 
		    (a,
		     list_fold 
		       (fun t l -> 
			  match t 
			  with S_mark(s,m) -> (M((a,a,s),m),true)::l
			    | S_free(s) -> l
			    | S_bound(s,(a2,s2)) -> (AL((a,a,s),(a2,s2)),true)::(B(a,a,s),true)::l
		       )
		       b [H(a,a),true])) l)
	      
    let translate_problem  pb' pb  f =
      let _ = trace_print "TRANSLATE_INIT" in 
      let init = translate_init pb.cpb_init in 
      let cpb_map = 
	match pb.cpb_mark_site 
	with None -> String2Map.empty 
	  | Some a -> 
	      String2Map.map 
		(fun  a -> 
		   List.fold_left 
		     (fun set a -> StringSet.add a set)
		     StringSet.empty 
		     a)
		a 
      in 
      let _ = trace_print "POSSIBLE_LINKS" in 
      let possible_links,possible_linksb  = 
	match  pb.cpb_contact with None  -> 
	  list_fold 
	    (fun (a,_,c) sol ->
	       list_fold 
		 (fun (a',_,c') sol -> 
		    list_fold 
		      (fun c sol -> 
			 list_fold 
			   (fun c' (sol1,sol2) -> 
			      if L((a,a,c),(a',a',c'))=l((a,a,c),(a',a',c')) 
			      then 
				(((a,c),(a',c'))::sol1),(L((a,a,c),(a',a',c'))::sol2) else sol1,sol2)
			  c' sol)
		     c sol)
		pb.cpb_interface sol)
	    pb.cpb_interface ([],[]) 
	| Some f -> 
	    String2Map.fold 
	      (fun (a1,s1) s sol ->
		list_fold
		  (fun (a2,s2) (sol1,sol2) -> 
		    if L((a1,a1,s1),(a2,a2,s2))=l((a1,a1,s1),(a2,a2,s2)) 
		    then 
		      (((a1,s1),(a2,s2))::sol1),(L((a1,a1,s1),(a2,a2,s2))::sol2) 
		    else sol1,sol2)
		  s sol)
	      f ([],[]) in 
      
  
      let liste_rules = 
	List.rev_map 
	  (fun r -> 
	    let ir = rule_precomputation pb r in 
	    let _ = trace_print "END_PRECOM" in 
	    let idlist = list_fold (fun i l -> (ir.id_mapping i)::l) ir.id_list [] in 
	    let old_list_map,dotlist =
	      let (m,dlist) = 
		list_fold 
		  (fun i  (m,dl) -> 
		    let (id,_) = ir.id_mapping i in 
		    StringMap.add id i m,
		    if IntSet.mem i r.cpb_dots then id::dl
		    else dl)
		  ir.id_list
		  (StringMap.empty,[]) in 
	      m,dlist  in 
	    let id = idlist in 
	    let dots = r.cpb_dots in 
	    let active_species = List.rev_map fst idlist in
	    let passive_species = 
	      (match r.cpb_passive 
	      with None -> []
	      | Some l -> (
		  list_map 
		    (fun ((i,s),(i',s')) -> 
		      ((fst (ir.id_mapping i),snd (ir.id_mapping i),s),
		       (fst (ir.id_mapping i'),snd (ir.id_mapping i'),s'))) l)) in 
	    let guard_prefix = 
	      IntSet.fold (fun i sol -> (Pb_sig.H (fst (ir.id_mapping i),
						   snd (ir.id_mapping i)),
                                         false)::sol) r.cpb_control.cpb_create [] in 
	    let rules,map =  
	      list_fold 
		(fun (a,c,b) (sol,map) -> 
		  let inj,map = 
		    list_fold (translate_test pb ir) b (guard_prefix,map) in 
		  {labels=a;
                    abstract_guard = None;
		    injective_guard=inj;
                    target = IntSet.fold (fun x -> StringSet.add (fst (ir.id_mapping x))) c StringSet.empty;
		    alias = 
		    
		    let l = 
			    List.map 
			      (fun (x,y) -> 
				(fst (ir.id_mapping x),
				(fst (ir.id_mapping y)))) 
			      r.cpb_equal in
	     l
	      ;
		  }::sol,map)
		r.cpb_guard ([],IntStringMap.empty) in 
	    let control = 
	      let context,uncontext = 
		list_fold 
		  (translate_control_update pb ir map cpb_map)
                  r.cpb_control.cpb_update
		  ([],[]) in
	      let context = 
		let (solt,solf,sol) = 
		  list_fold 
		    (fun x (solt,solf,sol) ->
		       match x with 
			   B(_),true -> BSet.add (fst x) solt,solf,x::sol 
			 | B(_),false -> solt,BSet.add (fst x) solf, sol
		      |  _ -> solt,solf,x::sol)
		    context (BSet.empty,BSet.empty,[]) in
		  BSet.fold
		    (fun x sol ->
		       if BSet.mem x solt then sol else (x,false)::sol)
		    solf sol in 
	      let bound_site = 
		List.fold_left
		  (fun set a -> 
		    match a with 
		      Pb_sig.B(i,ig,a),true -> String2Set.add (i,a) set
		    | _ -> set)
		  String2Set.empty context in 
	      let create =  
		IntSet.fold 
		  (translate_control_create pb ir) 
		  r.cpb_control.cpb_create [] in 
	      {Pb_sig.context_update = 
		List.fold_left 
			 (fun context_update (i,ig) -> 
			   (
			   let sites = 
			     let rec aux l = 
			       match l with 
				 (a,b,c)::q -> if a = ig then c else aux q
			       | [] -> [] 
			     in aux pb.cpb_interface  in 
			   List.fold_left
			     (fun context_update s -> 
			       if String2Set.mem (i,s) bound_site 
			       then context_update 
			       else (Pb_sig.B(i,ig,s),false)::context_update)
			     
			     ((Pb_sig.H(i,ig),true)::context_update)
			     sites ))
		  context create;
		Pb_sig.uncontext_update = uncontext ;
		Pb_sig.add = 
		IntSet.fold
		  (translate_control_remove pb ir)
		  r.cpb_control.cpb_create [];
		Pb_sig.remove = 
		IntSet.fold 
			 (translate_control_remove pb ir) 
		  r.cpb_control.cpb_remove [];
	      }
		
	    in 
	    
	    
	    let id_of_species,specie_of_id  = 
	       let fadd a b (c,c') = 
		 let list = try (StringMap.find a c) with Not_found -> [] in 
		 (StringMap.add a (b::list) c,
		  StringMap.add b a c')
	       in 
	       list_fold 
		 (fun (a,k) sol -> fadd k a sol)
		 id (StringMap.empty,StringMap.empty)
	     in 
	    let b_of_sites,b_of_binding = 
	      let fadd k l m = 
		let set = 
		   try (BMap.find k m) with
		     Not_found -> BSet.empty in 
		BMap.add k (BSet.add l set) m  in 
	      list_fold 
		(fun a (map1,map2) -> 
		  match a  with 
		    L((a,a',b),(c,c',d)) -> 
			    list_fold 
			(fun ida (map1,map2) -> 
			  let binding = AL((ida,a,b),(c,d)) in 
			  (fadd (B(ida,a,b)) binding map1,
			   list_fold 
			     (fun idb map2 ->
			       (fadd binding (l((ida,a,b),
							  (idb,c,d))) map2))
			     (try (StringMap.find c id_of_species) with Not_found -> []) map2
			     )) (try (StringMap.find a id_of_species) with Not_found -> []) 
			(if (a,b)=(c,d) 
			then (map1,map2) 
				else
			  (let (a,b),(c,d) = (c,d),(a,b) in 
			  list_fold 
			    (fun ida (map1,map2) -> 
			      let binding = AL((ida,a,b),(c,d)) in 
				      (fadd (B(ida,a,b)) binding map1,
				       list_fold 
					 (fun idb map2 ->
					   (fadd binding (l((ida,a,b),
							    (idb,c,d))) map2))
					 (try (StringMap.find c id_of_species) with Not_found -> []) map2
					 )) (try (StringMap.find a id_of_species) with Not_found -> []) (map1,map2)))
		  |	 _ -> (error 1031 "PARSE_CASE" (raise Exit))) 
		possible_linksb
		(BMap.empty,BMap.empty) in
	    let b_of_id = 
	      list_fold 
		(fun (a,b,c) sol -> 
		  try (
		    let l = StringMap.find a id_of_species in 
		    list_fold 
		      (fun k sol -> 
			StringMap.add 
			  k 
			       (list_fold 
				  (fun s sol -> 
				    let bool = B(k,a,s) in 
				    (BSet.fold
				       (fun binding sol -> 
					 BSet.fold
					       (fun b sol -> b::sol)
					   (try (BMap.find binding b_of_binding) 
					   with Not_found -> BSet.empty)
					       (binding::sol))
				       (try (BMap.find 
					       bool 
					       b_of_sites) with Not_found -> BSet.empty)
				       (bool::sol)))
				  c [H(k,a)]) sol)
		      l sol) with Not_found -> sol)
		pb.cpb_interface StringMap.empty
	    in 
	    let interface_id = 
	      list_fold 
		(fun (a,b,c) sol ->
		  list_fold             (* we copy each specie data for each instance of this specie in the rule *)
		    (fun k sol -> (k,b,c)::sol)
		    (try (StringMap.find a id_of_species)
		    with _ -> [])
		    sol)
		pb.cpb_interface [] in
	    let vars_of_rules (c,r) = 
	      let set = 
		List.fold_left 
		  (fun set r -> 
		    List.fold_left 
		      (fun set (a,_) -> BSet.add a set)
		      set r.injective_guard)
		  BSet.empty r  in
	      let set = 
		List.fold_left
		  (fun set (r,_) ->
		    BSet.add r set)
		  set 
		  c.Pb_sig.context_update
		  in 
	      let set = 
		BSet.fold 
		  (fun b set -> 
		    match b with 
		      B(a,b,c) ->
			List.fold_left
			  (fun set (d,e) -> 
			    BSet.add (AL((a,b,c),(d,e))) set)
			  set
			  (try 

			    String2Map.find (b,c)  
			      (match pb.cpb_contact with None -> raise Not_found | Some a -> a)
			  with Not_found -> [])
		    | M((a,b,c),_) ->
			List.fold_left
			  (fun set d -> 
			    BSet.add (M((a,b,c),d)) set)
			  set pb.cpb_marks
		    | _ -> set)
		  set set 
	      in set
	     in 
	    {Pb_sig.vars= Some (vars_of_rules (control,rules)) ;
	      Pb_sig.id = id;
	      Pb_sig.dotset = dots;
	      Pb_sig.active_species=active_species ;
	      Pb_sig.old_id = old_list_map ;
	      Pb_sig.passive_species=passive_species;
	      Pb_sig.abstract_lens=None;
	      Pb_sig.rules=rules;
	      Pb_sig.control=control;
	      Pb_sig.id_of_species=id_of_species;
	      Pb_sig.specie_of_id=specie_of_id;
	      Pb_sig.b_of_sites=b_of_sites ;
	      Pb_sig.b_of_binding=b_of_binding ;
	      Pb_sig.b_of_id=b_of_id;
	      Pb_sig.interface_id=interface_id;}
	      )
	  pb.cpb_rules in 
     
	  Some 
	{
      init = init;
      system = 
      let rep = List.rev(sort_pb liste_rules) in rep}
	  
 

    let fill_pretty_map pb f = 
      let cpb = 
	match pb.intermediate_encoding with
	  None -> 
	    (match pb.gathered_intermediate_encoding 
		with Some a -> a
	    | None -> error_frozen 980 "CBNG 980" (fun () -> raise Exit))
	| Some a -> a in 
      
      let pretty_map = 
	list_fold 
	  (fun (a,b,c) sol -> 
	    StringMap.add a 
	  (let ffind a map = 
	    (try (StringMap.find a map) 
	    with Not_found -> tuple_bot) in 
	  list_fold 
	    (fun a' sol -> 
	      StringMap.add a' {(ffind a' sol) with is_marked = 
				 if f (H(a,a)) then Any else Abstracted} sol)
	    b
	    (list_fold 
	       (fun b sol ->
                     if (not (f (B(a,a,b)))) then sol else  
		     StringMap.add b {(ffind b sol) with is_bound = if f (B(a,a,b))then Any else Abstracted} sol)
	       c
	       StringMap.empty))
	      sol)
	  cpb.Pb_sig.cpb_interface  StringMap.empty in 
      {pb with pretty_map=pretty_map}
	
    let refine_contact_map pb old f = pb 
  end )
     
   
     

    

     

     

    

     

     
     

    

     

     

    

     

     
