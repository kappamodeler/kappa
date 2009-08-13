(* 19/05/2007 *)
(* Static analysis of Kappa systems*)
(* Jerome Feret pour PlectiX *)
(* Encoding To compute the causality map  *)
(* quarkification.ml *)
open Data_structures 
open Pb_sig 
open Tools
open Error_handler 

let error (*i*) x (*t*) y = 
    unsafe
      (Some x) 
      (Some "Complx")
      (Some "quarkification.ml") 
      None (*(Some t)*) 
      None (*(Some i)*) 
      y

let error_frozen (*i*) x (*t*) y = 
    unsafe_frozen
      (Some x) 
      (Some "Complx")
      (Some "quarkification.ml") 
      None (*(Some t)*) 
      None (*(Some i)*) 
      y

type bound = UNKNOWN | BOUND | FREE | LINK of (int*string) | NONE
  

let upgrade b1 b2 = 
  match b1,b2 with 
    UNKNOWN,_ -> b2
  | _,UNKNOWN -> b1
  | LINK(a,b),BOUND -> b1
  | BOUND,LINK(a,b) -> b2
  | _ -> if b1=b2 then b1 else NONE


let ql (a,b,c,d) = 
  if compare a c < 0 then QL(a,b,c,d)
  else if compare c a<0 then QL(c,d,a,b)
  else if compare b d<0 then QL(a,b,c,d)
  else QL(c,d,a,b)

let quarkify cpb contact rule = 
  let mark_site = cpb.Pb_sig.cpb_mark_site in
  match rule.cpb_guard,mark_site  with 
    [guard],Some mark_site  -> 
      let control = rule.cpb_control in 
      let r_species = rule.cpb_r_species in
      let mapid = 
	List.fold_left 
	  (fun map (i,s) -> IntMap.add i s map)
	  IntMap.empty 
	  r_species in 
      let (r,_,test) = guard in 
      let fadd (a,s) b map = 
	let old = 
	  try (IntMap.find a map) with 
	    Not_found -> StringMap.empty in
	let old_site = 
	  try (StringMap.find s old) with 
	    Not_found -> UNKNOWN in
        IntMap.add a (StringMap.add s (upgrade old_site b) old) map  in
      let get (a,s) map = 
	try StringMap.find s (IntMap.find a map)
	with Not_found -> UNKNOWN in
      let fadd_mark (a,s) m map = 
	let old = 
	  try (IntMap.find a map) with 
	    Not_found -> StringMap.empty in
	IntMap.add a (StringMap.add s m old) map  in
      let specie i = 
	try (IntMap.find i mapid)
	with Not_found -> 
	  error_frozen "QUARKIFICATION" (fun () -> raise Exit) 
      in
      let get_mark (a,s) map' = 
	try (let old = IntMap.find a map' in
	       [StringMap.find s old])
	with Not_found -> 
	  try (String2Map.find (specie a,s) mark_site)
	  with Not_found -> [] in
      let map,map',here,questions = 
	List.fold_left 
	  (fun (map,map',here,questions) test -> 
	    match test with 
	      Is_connected _  -> map,map',here,questions
	    | Is_here (a) -> map,map',IntSet.add a here,questions
	    | Is_bound (a,s) -> (fadd (a,s) BOUND map),map',here,IntSet.add a questions
	    | Is_free (a,s) -> fadd (a,s) FREE map,map',here,IntSet.add a questions
	    | Is_related (a,b) -> fadd a (LINK(b)) (fadd b (LINK(a)) map),map',here,IntSet.add (fst a) questions
	    | Is_marked ((a,s),m) -> 
		(map,
		 fadd_mark (a,s) m map',
		 here,IntSet.add a questions)
	    | Is_disjoint _ | Is_forbid _  -> (map,map',here,questions))
	  (IntMap.empty,IntMap.empty,IntSet.empty,IntSet.empty)
          test in
      let here = (*IntSet.diff*) here (*questions*) in
      let test_pos = 
	IntMap.fold 
	  (fun a map test_pos -> 
	    StringMap.fold 
	      (fun s state test_pos ->
		let a = specie a in 
		match state with 
		 BOUND -> 
		    List.fold_left 
		      (fun test_pos (_,a',s') ->QSet.add (ql(a,s,a',s')) test_pos)
		      test_pos 
		      (try (String2Map.find (a,s) contact.link_of_site)
		      with Not_found -> []) 
	
		      
		| FREE  -> QSet.add (QF(a,s)) test_pos
		| LINK(a',s') -> 
		    QSet.add 
		      (ql(a,s,specie a',s')) 
		      test_pos
		| _ -> test_pos
	      )
	      map test_pos)
	  map QSet.empty in
      let test_pos = 
	IntMap.fold 
	  (fun a map test_pos -> 
	    let a = specie a in
	    StringMap.fold 
	      (fun s state test_pos -> QSet.add (QM(a,s,state)) test_pos)
	      map test_pos)
	  map' test_pos 
      in 
      let test_pos = 
	IntSet.fold 
	  (fun a test_pos -> 
	    QSet.add (QH(specie a)) test_pos)
	  here test_pos in 
      let mod_pos,mod_neg = 
	List.fold_left 
	  (fun (mp,mn) action ->
	    match action with 
	      No_Pol | No_Helix -> (mp,mn) 
	    | 
	      Bind ((a,s),(a',s')) -> 
		let aid = specie a in
		let aid' = specie a' in
		  (QSet.add (ql(aid,s,aid',s')) mp)
		   ,
		 let mn' = 
		   if 
		     (let q = get (a,s) map in
		     q = FREE or q = UNKNOWN)
		   then
		     (QSet.add (QF(aid,s)) mn)
		   else mn
		 in
		 if 
		   (let q = get (a',s') map in
		   q = FREE 
		     or q = UNKNOWN)
		 then
		   (QSet.add (QF(aid',s')) mn')
		 else mn'
	    | Mark ((a,s),m) -> 
		let aid = specie a in 
		let mark_list = get_mark (a,s) map' in
		  (QSet.add (QM(aid,s,m)) mp,
		   List.fold_left 
		     (fun sol x -> (QSet.add (QM(aid,s,x)) sol))
		     mn mark_list)
	    | Release ((a,s),(a',s')) -> 
		let aid = specie a in
		let aid' = specie a' in
		  
		let mp = 
		  if 
		    (let q = get (a,s) map in 
		       not (q = FREE))
		  then
		    QSet.add (QF(aid,s)) mp
		  else 
		    mp in 
		let mp = 
		  if 
		    (let q = get (a',s') map in 
		       (not (q=FREE)))
		  then 
		    QSet.add (QF(aid',s')) mp
		  else
		    mp
		in 
		  mp,
		QSet.add (ql(aid,s,aid',s')) mn
	    | Break_half (a,s) -> (
		let aid = specie a in
		let q = get (a,s) map in
		  match q with 
		      LINK(a',s') -> 
			let aid' = specie a' in
			   (QSet.add (QF(aid,s)) 
			      (QSet.add (QF(aid',s')) mp),
			    QSet.add (ql(aid,s,aid',s'))
			      mn)
		    | BOUND | UNKNOWN -> 
			let l = 
			  try (String2Map.find (aid,s) contact.link_of_site)
			  with Not_found -> []
			in
			  List.fold_left 
			    (fun (mp,mn) (_,aid',s') -> 
			       QSet.add (QF(aid',s')) mp,
			       QSet.add (ql(aid',s',aid,s)) 
				 mn
			    )
			    (QSet.add (QF(aid,s)) mp,mn) l 
		    | _ -> mp,mn)
		
	    | Check _ | Check_choice _  -> (mp,mn))
	  (QSet.empty,QSet.empty) control.cpb_update
          in
      let mod_pos,mod_neg = 
	IntSet.fold 
	  (fun a (mp,mn) ->
	     let aid = specie a in
	     QSet.add (QH(aid)) mp,mn)
	  control.cpb_create (mod_pos,mod_neg) in
      let mod_pos,mod_neg = 
	IntSet.fold 
	  (fun a (mp,mn) -> 
	     let aid = specie a in
	     let mn = QSet.add (QH(aid)) mn in
	     let old = 
	       try (IntMap.find a map) 
	       with Not_found -> StringMap.empty 
	     in
	     let marksite,boundsite = StringMap.find aid cpb.cpb_interface_of_agent in 
	     let mn = 
	       List.fold_left 
		 (fun mn s -> 
		   let l = get_mark (a,s) map' 
		   in
		   List.fold_left
		     (fun mn m -> QSet.add (QM(aid,s,m)) mn)
		     mn l) 
		 mn marksite in 
		     
	     let mp,mn = 
	       List.fold_left 
		 (fun (mp,mn) s -> 
		   try begin
		     let q = StringMap.find s old in
		     match q with 
			 FREE -> mp,QSet.add (QF(aid,s)) mn
		       | LINK(a',s') -> 
			  let aid'=specie a' in
			    QSet.add (QF(aid',s')) mp,
			  QSet.add (ql(aid,s,aid',s')) mn
		      | BOUND -> 
			  let l = 
			    try (String2Map.find (aid,s) contact.link_of_site)
			    with Not_found -> []
			  in
			    List.fold_left 
			      (fun (mp,mn) (_,aid',s') -> 
				 QSet.add (QF(aid',s')) mp,
				 QSet.add (ql(aid',s',aid,s)) 
				   
				      mn)
			      (mp,mn) l 
		      | _ -> raise Not_found 
		   end
		       with Not_found -> 
			 let l = 
			   try (String2Map.find (aid,s) contact.link_of_site)
			   with Not_found -> []
			 in
			 List.fold_left 
			   (fun (mp,mn) (_,aid',s') -> 
			     QSet.add (QF(aid',s')) mp,
			     QSet.add (ql(aid',s',aid,s)) 
			       
			       mn)
			      (mp,QSet.add (QF(aid,s)) mn)
			   l) (mp,mn) boundsite
	     in
	     mp,mn
	  )
		 control.cpb_remove (mod_pos,mod_neg) in
      let q = Some {test_pos = QSet.fold (fun t q -> t::q) test_pos []; 
		     mod_pos = QSet.fold (fun t q -> t::q) mod_pos []; 
		     mod_neg = QSet.fold (fun t q -> t::q) mod_neg [];}
      in 
      
	  
	  
      {rule with cpb_quarks = q}
  | _ -> rule 
