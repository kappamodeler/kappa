(* 13/04/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Compression *)
(* compressor.ml *)

open Printf
open Config_complx
open Tools
open Data_structures
open Pb_sig
open Abstract_expr_sig
open Comment_sig  
open Error_handler

let access1 = true   
let access2 = true
let access3 = false
let access31 = true
let access32 = true
let access33 = true
let access4 = true

let error i x t y = 
    unsafe
      (Some x) 
      (Some "compressor.ml") 
      (Some t) 
      (Some i) 
      y

let error_frozen i x t y = 
    unsafe_frozen
      (Some x) 
      (Some "compressor.ml") 
      (Some t) 
      (Some i) 
      y

module Compressor =
  (functor (A:AbstractExprBool) -> 
    (struct   
      let h s = A.atom_pos (A.K.E.V.var_of_b  s)
      let r_h s = A.r_atom (A.K.E.V.var_of_b s)
      let set_of_list l =    
	list_fold (fun k sol -> A.K.E.V.varset_add (A.K.E.V.var_of_b k) sol) l A.K.E.V.varset_empty	
      let run mode vars_init var_const var_var access rs dep messages = 
	if A.r_is_bot access 
	then A.K.empty_kleenean_rule_system,messages
	else 
	  
	let dep = fun x -> List.rev_map A.K.E.V.var_of_b (dep (A.K.E.V.b_of_var x)) in 
	let vars = set_of_list vars_init in 
	let vars_const = set_of_list var_const in 
	let vars_var = set_of_list var_var in 
	let rs = 
	  List.rev_map 
	    (fun (a,a',b) -> 
	      (a,
	       a',
	       List.rev_map 
		 (fun (a,b) -> A.K.E.V.var_of_b a,b) 
		 b)
		) 
	    rs in 
	let s = A.K.build_kleenean_rule_system rs vars in 
        match mode 
        with Full -> 
        let a,messages  = A.abstract_system mode s access vars_var dep messages in 
	A.abstract_system mode a access vars_const dep messages  
	| Isolated   -> A.abstract_system mode s access vars dep messages 
	| _ -> error "line 71" "Unknown compression mode" "run" (A.abstract_system mode s access vars dep messages)
	  
	  
      let parse mode pb  messages (clock,counter) is_access = 
	let print_expr expr = 
	  try 
	    A.print_reachable_states string_txt expr (fun x->x) pb (Some "()") (Some stdout)  with _ -> []  in 
	let cpb = 
	  match mode with 
	    Full -> 
	      (match pb.gathered_intermediate_encoding 
	      with None -> error_frozen "line 83" "Gathered intermediate encoding is missing" "parse" (fun () -> raise Exit)
	      | Some a -> a) 
	  | _ ->
	      (match pb.intermediate_encoding 
	      with None -> error_frozen "line 87" "Intermediar encoding is missing" "parse" (fun () -> raise Exit)
	      |	 Some a -> a)
	in 
	let boolean_encoding = 
	  match mode with 
	    Full -> (
	      match pb.gathered_boolean_encoding with 
		None -> error_frozen "line 94" "Gathered boolean encoding is missing" "parse" (fun () -> raise Exit)
	      | Some a -> a)
	  | _ -> (
	      match pb.boolean_encoding with 
		None -> error_frozen "line 98" "Boolean encoding is missing" "parse" (fun () -> raise Exit)
	      | Some a -> a) in 
	let n = match pb.n_rules with None -> 1 | Some n -> n in
	let rep = 
	  list_fold
	    begin 
	      (fun case (s,messages,(clock,counter)) -> 
		 (* id_of_species maps each specie onto the list of id of this spechie *)
		 let _ = trace_print  "PARSE CASE" in
		   
	      let vclock = (clock*(!Data.clock_precision))/n in
	      let counter = ticking vclock counter in
	      let id_of_species,specie_of_id = 
		case.id_of_species,case.specie_of_id in 
	      let id_of_species = 
		(fun x -> 
		   try (StringMap.find x id_of_species) with Not_found -> []) in 
	      let n = 
	        StringMap.fold (fun _ _ n -> n+1) specie_of_id 0 in 
	      let specie_of_id = 
		(fun x -> 
		   try (
		     StringMap.find x specie_of_id) with Not_found -> x )in 
	      	
	      (* possible_links is the list of any links between sites, we copy each generic sites according to each instance of species*)
		
	      let _ = trace_print "POSSICLE LINKS" in
              let possible_links = 
		match pb.contact_map
		with Some x -> x.relation_list 
		| None -> 
		    list_fold 
		      (fun (a,_,c) sol ->
			 list_fold 
			   (fun (a',_,c') sol -> 
			      list_fold 
				(fun c sol -> 
				   list_fold 
				  (fun c' sol -> ((a,c),(a',c'))::sol)
				     c' sol)
				c sol)
			   cpb.cpb_interface sol)
		      cpb.cpb_interface [] in 
	      

	      (* interface_id build the set of triple (k,p,b) where k is a specie id, p the list of its phoporilasable site and b the list of its boundable sites.*)
	      let interface_id = case.interface_id in 
		
	      (*special_link is the set of the link that defines passive species (the ones that may be connected to the active ones)*)
              (*anchor is the set of (id,site) that are used in the definition of passive species*)
	      let _ = trace_print "GEN" in
	      let dangerous_site = 
		  List.fold_left
		  (fun set r ->
		    List.fold_left 
		      (fun set (x,y) ->
			StringSet.add x (StringSet.add y set))
		      set r.alias)
		  StringSet.empty case.rules in
	      let dangerous = not (StringSet.is_empty dangerous_site) in
	      let passive = if dangerous then [] else case.passive_species in
	      
	      let generic_special_link,special_link,sym_special_link,generic_anchor,anchor,anchors_of_species = 
		list_fold 
		  (fun ((a,ga,sa),(b,gb,sb)) (set0,set,set1,gset,set',map) -> 
		     String2Set.add (ga,gb) (String2Set.add (gb,ga) set0),
		     String22Set.add ((a,sa),(b,sb)) set,
		     BSet.add (l((a,ga,sa),(b,gb,sb))) set1,
		     String2Set.add (ga,sa) gset,
		     SiteSet.add (a,ga,sa) set',
		   let l = try (StringMap.find a map)
		   with _ -> StringSet.empty  in 
		     StringMap.add ga (StringSet.add sa l) map)
		  passive 
		  (String2Set.empty,String22Set.empty,BSet.empty,String2Set.empty,SiteSet.empty,StringMap.empty)  in
	      let possible_links_id = 
		list_fold
		  (fun ((a,b),(c,d)) sol -> 
		     list_fold 
		       (fun k sol -> 
			list_fold 
			  (fun k2 sol -> 
			    if (String2Set.mem (a,b) generic_anchor  
				  && StringSet.cardinal (StringMap.find a anchors_of_species) < 2  
				  && (not ((String22Set.mem ((k,b),(k2,d)) special_link) or String22Set.mem ((k2,d),(k,b)) special_link)))
				or 
			      (String2Set.mem (c,d) generic_anchor  
				 && StringSet.cardinal (StringMap.find c anchors_of_species) < 2
				 && (not ((String22Set.mem ((k2,d),(k,b)) special_link) or (String22Set.mem ((k,b),(k2,d)) special_link)))) 
				or (k=k2 && b=d)
 				or (b=d && a=c && (List.mem ((k2,c,d),(k,a,b)) sol)) then sol else 
			      (((k,a,b),(k2,c,d)))::sol)
			  (id_of_species c)
			  sol)
		      (id_of_species a)
		      sol)
		  possible_links [] in
	      

	      
               (*link_of_species maps each id specie  to the set of link that share this specie*)
	      let link_of_species = 
		let fadd a (b1,b2) c = 
		  let rep = l(b1,b2) in 
		  try (StringMap.add a (BSet.add rep (StringMap.find a c)) c)
		  with 
		    Not_found -> (StringMap.add a (BSet.singleton rep) c) in 
		
		list_fold 
		  (fun ((a,a',b),(c,c',d)) map -> fadd a ((a,a',b),(c,c',d)) (fadd c ((a,a',b),(c,c',d)) map))
		  possible_links_id 
		  StringMap.empty in 
	      
               (*bool_of_specie maps each id specie to the set of its boolean attributes *)
	     
	      let bool_of_species = 
		list_fold 
		  (fun (a,b,c) sol -> 
		    let set = 
		      
			(list_fold 
			   (fun b set -> BSet.add (B(a,specie_of_id a,b)) set)
			   c 
			   (try (StringMap.find a sol) with _ -> BSet.empty)) in 
		    let set = 
		      if 
			(List.exists (fun ((s,_,_),_) -> s=a) passive)
		      then BSet.add (H(a,specie_of_id a)) set else set in 
		    

		    if BSet.is_empty set 
		    then sol
		    else StringMap.add a set sol)
		  interface_id 
		  link_of_species  in 
	      
	      
		(*var_init is the list of all boolean attributes (for each id specie)*)
	        (*dep is the list of all potential links *)
              let var_init,dep = 
		list_fold 
		  (fun ((a,b,_),_) (sol,dep) -> 
		  	(H(a,b)::sol,dep))
		  passive 
		  (list_fold 
		     (fun c (sol,dep) -> 
		       ((let (a,a',a''),(b,b',b'') = c in
		       if (*dangerous or*) StringSet.mem a dangerous_site  or 
			 StringSet.mem b dangerous_site  
		       then sol 
		       else ((l((a,a',a''),(b,b',b'')))::AL((a,a',a''),(b',b''))::AL((b,b',b''),(a',a''))::sol)),dep))
		     possible_links_id
		     ((list_fold 
			 (fun (s,p,l) sol -> 
			   list_fold 
			     (fun p' sol -> 
			       list_fold 
				 (fun m sol ->
				   (M((s,specie_of_id s,p'),m)::sol))
				 cpb.cpb_marks sol) p 
				 (list_fold (fun l' sol -> (
				   if not dangerous && not (StringSet.mem s dangerous_site) then 
				      (B(s,specie_of_id s,l'))::sol else sol))
				l sol))
			     interface_id []),[]))
		      in 
	      let dep =
		if !Config_complx.ignore_dep or n>8 then BMap.empty else 
		let fadd a b c = BMap.add a (b::(try (BMap.find a c) with _ -> [])) c in
		let fadd_sim a b c = fadd a b (fadd b a c) in 
		let dep = BMap.empty in 
	
		let dep = (* when we forget about one bit of information about the mark of a site, we forget all of them *)
		  list_fold (fun (a,b,c) dep -> 
		    list_fold (fun b dep ->
		      list_fold (fun m dep ->
			list_fold (fun m' dep -> 
			  if m=m' then dep
			  else 
			    let x = M((a,specie_of_id a,b),m) in 
		            let y = M((a,specie_of_id a,b),m') in 
			    fadd_sim  x y dep)
			  cpb.cpb_marks dep)
			cpb.cpb_marks dep)
		      b dep)
		    interface_id dep in
	
		let dep = 
		   list_fold (fun (a,b,c) dep -> 
		    list_fold (fun c dep ->
		      let contact = 
			try String2Map.find (a,c) (match cpb.cpb_contact
			    with Some a -> a | _ -> raise Not_found)
			    with Not_found -> [] in
		      list_fold (fun l1 dep ->
			list_fold (fun l2 dep -> 
			  if l1=l2 then dep
			  else 
			    let x = AL((a,specie_of_id a,c),l1) in 
		            let y = AL((a,specie_of_id a,c),l2) in 
			    fadd_sim  x y dep)
			  contact dep)
			contact dep)
		      c dep)
		    interface_id dep in

		let dep = (* optional dependences (Cf config.ml)*)
		  if !Config_complx.ignore_dep then BMap.empty else 
		  dep in 
		
		let dep = (* we forget abount missing passive species *)
		  list_fold 
		    (fun ((a,a',b),(c,c',d)) map -> 
		      fadd_sim  (l((a,a',b),(c,c',d))) ((AL((a,a',b),(c',d))))
		      (fadd (AL((a,a',b),(c',d))) (AL((c,c',d),(a',b)))
			(fadd (AL((c,c',d),(a',b))) (AL((a,a',b),(c',d)))
			   (fadd (AL((a,a',b),(c',d))) (H(a,specie_of_id a))
			(fadd (AL((c,c',d),(a',b))) (H(a,specie_of_id a))
			   map)))))
		    passive dep in
		let dep = list_fold 
		    (fun ((a,a',b),(c,c',d)) map -> 
		      fadd (B(c,c',d)) (B(a,a',b)) (fadd  (B(c,c',d)) (H(a,a'))  map)) passive dep  in 
		let dep = (* when we abstract the fact that a site is here, we abstract all booleans about it *)
		  list_fold 
		    (fun (a,b,c) map ->
		      let k = H(a,specie_of_id a) in 
		      list_fold (fun b map -> 
			list_fold (fun m map -> 
			  fadd k (M((a,specie_of_id a,b),m)) map) cpb.cpb_marks map)
			b
			(list_fold (fun c map ->
			  fadd k (B(a,specie_of_id a,c)) map) c map))
		    interface_id  dep  in
		let dep = (* when we abstract the fact that a site is bound, we forget about its linkage *)
		  list_fold 
		    (fun ((a1,a2,a3),(b1,b2,b3)) map -> 
		      let k1 = (B(a1,a2,a3)) in 
		      let k2 = (B(b1,b2,b3)) in 
		      let k3 = (AL((a1,a2,a3),(b2,b3))) in
		      let k4 = (AL((b1,b2,b3),(a2,a3))) in
		      (fadd k1 k3 (fadd k2 k4 map)))
		    possible_links_id dep in 
		dep in  
	      let _ = if !trace then 
		let _ = print_string "DEP\n" in
		let _ = 
		  BMap.iter
		    (fun x y -> 
		      print_b x;
		      print_string ":";
		      List.iter print_b y;
		      print_newline ())
		    dep in () 
	      in 
	      let dep x = try (BMap.find x dep) with _ -> [] in
	      let dump i = 
		(*print_int i ;print_newline ()*) ()  in 
	      let _ = dump 296 in
	      let access = 
	             
		if !Config_complx.enforce_structural_invariants && n<(!Config_complx.max_lens_size +1) (*&& not dangerous*)  && access1
		then 
		  begin 
		    let _ = dump 302 in 
		    let access =
		      if access2 then 
			list_fold 
			  (fun ((a,a',b),(c,c',d)) expr -> 
			    if dangerous 
				or StringSet.mem a dangerous_site 
				or StringSet.mem c dangerous_site 
			    then expr
			    else 
			      let expr = 
				A.r_conj expr 
				  (*whenever a passive species is bounded, it is bounded by its specified sites to specified sites *)
				  (A.r_imply 
				     (r_h (AL((c,c',d),(a',b))))
				     (r_h (AL((a,a',b),(c',d))))
				     ) in
			      let expr = 
				A.r_conj expr 
			    (*whenever a passive species is not bound, all attibutes take the value false *)
				    (if !Config_complx.efficient 
				    then 
				      (A.r_imply 
					 (A.r_neg (r_h (AL((c,c',d),(a',b)))))
					 (A.r_all_false_s 
					    ((BSet.fold (fun a l -> a::l)) 
					       (try ( (BSet.add (H(a,a')) (StringMap.find a bool_of_species)))
					       with _ -> BSet.singleton (H(a,a'))) [])))
				    else 
				      (A.r_imply 
					 (A.r_neg (r_h (AL((c,c',d),(a',b)))))
					 (A.r_neg 
					    (BSet.fold 
					       (fun a expr -> A.r_union (r_h a) expr)
					       (try (BSet.add (H(a,a')) (StringMap.find a  bool_of_species))
					       with _ -> BSet.singleton (H(a,a')))
					       (A.r_neg (A.r_ae_true)))
					    )
					 )
					) in
			      let expr = 
				A.r_conj expr
				  (A.r_conj 
				     (A.r_imply 
					(r_h (AL((c,c',d),(a',b))))
					(
				      r_h (l((c,c',d),(a,a',b)))
					)
					)
			      (A.r_imply 
			      (r_h (l((c,c',d),(a,a',b))))
			      	(r_h (AL((c,c',d),(a',b)))))) in 
			      
			      let _ = if !trace then 
				let _ = print_string "PASSIVE \n" in
				let _ = print_expr expr in () in 
				  expr)
			  
			  passive 
			  A.r_ae_true 
		      else A.r_ae_true in
		    let _ = dump 316 in
		    access 
		  end



		else A.r_ae_true in 
		(*we compute the list var_const of the attributes that can take only one value,*)
		(*and list var_val of the attributes that can take several values *)
	      let _ = if !trace then 
		
		let _ = print_string "ACCESS\n" in
		let _ = print_expr access in
		let _ = print_newline () in () in 
		let _ = dump 418 in 
	      let access = (* we take into account explicit implication *)
		match case.abstract_lens with None -> (access) 
		| Some implication -> (
		    if n>(!Config_complx.max_lens_size) then access 
		    else 
		      let inv = A.compute_abstract_lens implication in
		      let _ = if !trace then 
			let _ = print_string "INV\n" in
			let _ = print_expr inv in
			let _ = dump 440 in () in 
		      A.r_conj access inv)
		      in
	      let _ = dump 444 in 
	      let _ = if !trace  then 
		let _ = print_string "ACCESS2\n" in
		let _ = print_expr access in  () in 
	      let var_const,var_val = 
		if n>(!Config_complx.max_lens_size) then [],[] 
                else 
                  let full_map = 
		    list_fold 
		      (fun x map -> BMap.add x [false;true] map)
		      var_init BMap.empty in 
		  
		  let a = 
		    List.rev_map 
		    (fun rule  -> 
		      let x=rule.injective_guard in 
		      let x = 
		    List.fold_left 
		      (fun list (ax,bx) -> 
			match ax with 
			  L((a,a',a''),(b,b',b'')) -> 
			    (AL((a,a',a''),(b',b'')),bx)::(AL((b,b',b''),(a',a'')),bx)::(ax,bx)::list
			| _ -> (ax,bx)::list)
			  [] x in 
		      list_fold 
			(fun (a,b) map -> BMap.add a [b] map)
			x full_map)
		    case.rules  in 
		list_fold 
		  (fun v (v1,v2) -> 
		    let rec aux l (rep:bool list) = 
		      match l,rep with 
			t::q,[a] -> 
			  if BMap.find v t  = rep
			  then aux q rep
			  else (v1,v::v2) (*v can take several values*)
		      | t::q,[] -> 
			  let l = (BMap.find v t) in 
			  if List.length l = 1 
			  then aux q l
			  else (v1,v::v2) (*v can take several values*)
		      |	[],_ -> (v::v1,v2) (*v takes at most one value*)
		      |	_ -> v1,v::v2 (*v can take several values*)
		    in aux a [])
		  var_init  ([],[]) in 
	        let _ = if !Config_complx.trace then
		  (print_newline ();
		   print_string "VARCONST";
		   print_newline ();
		   List.iter print_b var_const;
		   print_newline ();
		   print_string "VARVAL";
		   print_newline ();
		   List.iter print_b var_val;
		   print_newline () 
		     )
		    in 
		let rs = case.rules in 
		let rs = List.rev_map (fun rule  -> (rule.labels,case.control,rule.injective_guard)) rs in
		let rs,dead = 
		  List.fold_left 
		    (fun (rs,dead) (lab,control,inj) -> 
		      if List.for_all 
			  (fun lab -> not (is_access lab)) 
			  lab
		      then
			rs,(lab,control,inj)::dead
		      else
			(lab,control,inj)::rs,dead)
		    ([],[]) (List.rev rs) in 
		    
		let _ = if !Config_complx.trace then 
		  (print_newline ();
		   print_string "LABEL";
		   print_newline ();
		   List.iter (fun (a,_,_) -> 
		     List.iter (fun a -> print_string (name_of_rule a)) a)  rs) in 
		let int_of_id = 
		  (fun x -> 
		    StringMap.find
		      x 
		      case.old_id )
		in 
		let deal_with rs (s,messages) = 
		  (*if rs = [] then s
		  else*)
		    let rep ,messages = run mode  
			var_init 
			var_const 
			var_val 
			access 
			rs 
			dep
			messages in 
		    (var_init,[rs],access,specie_of_id,int_of_id,case.dotset,
		  [rep])::s,messages
		in
		let s,messages = 
		  deal_with dead (deal_with rs (s,messages)) in 
		
		s,messages,(clock+1,counter))
	  end
	  (boolean_encoding.system) ([],messages,(clock,counter))
	in ((*end_ticking ();*)rep)

		    
     let print_kleenean_system ars =   
       List.iter (fun list -> 
	 List.iter (fun ((l,b),a) -> 
	   List.iter (fun r -> A.K.Id.print_id r;print_string ",") l;
	   print_string " ";
	   List.iter print_string a;

	   print_newline ()) list;
	 print_string "------------\n")
	 ars 

     let do_it fic fic2 mode auto pb  messages  =     
       let solstring = ref [] in 
       let is_access = 
	 match pb.unreachable_rules with 
	   None -> (fun x -> true)
	 | Some a -> (fun x -> not (RuleIdSet.mem x a)) in 
       let _ = trace_print "DOIT" in 
       let ars,messages,(clock,counter)  = 
	 parse mode pb messages (0,init_counters ()) is_access in
       let _ = ticking (!Data.clock_precision) counter in
       let _ = trace_print "PARSE OK" in 
       let print_string,print_newline,channel = 
	 match fic2 with "" -> (fun _ -> ()),(fun _ -> ()),None
	 |  _  ->
	     let channel = open_out fic2 in 
	     (
	     (fun x -> (Printf.fprintf channel "%s" x)),
	     (fun x -> (Printf.fprintf channel "\n")),Some channel) in 
             let sol =      
	 begin
	   List.fold_left 
	     (fun (liste) (vars,rs,access,(sp_of_id:string->string),int_of_id,dotset,ars) -> 
	       let vars = 
		 list_fold 
		   (fun a b -> A.K.E.V.varset_add (A.K.E.V.var_of_b a) b) vars A.K.E.V.varset_empty in
		 let ars_input = 
		   (list_map  
		      (fun rs  -> 
			let rs = 
			  list_map
			    (fun (a,b,c) -> 
			      (a,
			       b,
			       list_map (fun (a,b) -> A.K.E.V.var_of_b a,b) c))
			    rs in 
			A.K.extract_kleenean_rule_system 
			  (A.K.build_kleenean_rule_system  
			     rs 
			     vars)) 
		      rs) in
(*		 let ars_input = 
		   list_map 
		     (fun rs 
			 -> 
			   List.filter 
			     (fun (a,b,c) ->
			       List.exists (fun id -> 
				   (not (id.r_clone))) a) rs)
		     ars_input in *)
		 if List.for_all  (fun x -> x=[]) ars_input then  liste
		 else 
		   begin
		     let ars_output = 
                       list_map A.K.extract_kleenean_rule_system ars in 
		
		     let _ = 
		       (if !Config_complx.trace_concrete_rules or !Config_complx.trace_abstract_rules or !Config_complx.trace_reachable_states 
		       then (print_string "**************************************";print_newline ();print_newline ()));
		       (if !Config_complx.trace_reachable_states then 
			 begin
			   print_string "REACHABLE STATES:";
			   print_newline ();
			   let _ = A.print_reachable_states  
			       string_txt 
			       access 
			       sp_of_id 
			       pb 
			       (Some "()") (Some stdout) in 
			   print_newline () 
			 end);
		       let int_of_id = 
			 (fun x -> try (int_of_id x) with _ -> (print_string x;print_newline ();(-1))) in 
		       let chan = channel in 
		       (if !Config_complx.trace_concrete_rules then 
			 begin
			   print_string "CONCRETE RULES:";
			   print_newline ();
			   print_newline ();
			   try ( List.iter (fun a -> 
			     let _ = 
			       A.K.print_kleenean_system  
				 string_txt
				 (fun x -> true)  
				 name_of_rule 
				 int_of_id 
				 dotset 
				 a 
				 (Some "()") 
				 (fun x-> x) 
				 (fun x -> x) true 
				 (Count_isomorphism.compute_kyn_factor2 a auto)
				 chan
			     in ();print_string "\n") ars_input) with _ -> ();
			     print_newline ()
			 end);
		       (if !Config_complx.trace_abstract_rules then 
			 begin
			   print_string "ABSTRACT RULES:";
			   print_newline ();
			   print_newline ();
			   (
			   try (
			     List.iter 
			       (fun a -> (
				 let s = 
				   A.K.print_kleenean_system 
				     string_txt
				     is_access 
				     name_of_rule 
				     int_of_id 
				     dotset 
				     a 
				     (Some "()") 
				     (fun x -> x) 
				     sp_of_id 
				     true 
				     (Count_isomorphism.compute_kyn_factor2 a auto) 
				     chan 
				 in 
				 solstring:=s::(!solstring)))				
			       ars_output) with _ -> ();
				 
				 print_newline ())
			 end )  in 
		     (ars,ars_input,ars_output,vars,rs,access,
		      (sp_of_id:string->string),
		      (int_of_id:string->int),
		      dotset)::liste end)
			   [] ars
	 end  in 
       let _ = 
	 match channel with None -> () | Some chan -> close_out chan in 
    
	       
       
	       
     (end_ticking ();
     let nameint_of_rule x = idint_of_rule x in 
     let name_of_rule x = 
          try name_of_rule x with _ -> error "700" "Name_of_rule" "" "" in 
     let length l = 
       let rec aux l k = 
	 match l with [] -> k
	 |	t::q -> aux q ((String.length t)+k) in
       aux l 0 in 
(*     let fadd i s map = 
       let j = List.map nameint_of_rule i in 
       try 
	 let (_,old) = IntListMap.find j map in
	 begin
	   if length s < length old 
	    then 
	     IntListMap.add j (i,s) map 
	   else
	     map
	 end
       with 
	 Not_found -> 
	   begin
	     IntListMap.add j (i,s) map
	    end
     in*)
     let rep = (!solstring) in 
     let count = 
	       List.fold_left 
		 (fun map l1 ->
		   List.fold_left
		     (fun map (l,b) -> 
		       let size1 = List.length l in 
		       let size2 = List.length b in 
		       let node = (size1,size2,l,b) in 
		       List.fold_left 
			 (fun map x -> 
			   try 
			     let (size1',size2',l1',l2'),_ = IntMap.find x.r_simplx.Rule.id map
			     in 
			     if size1<size1' or 
			       (size1=size1' && size2<size2') 
			     then 
			       IntMap.add x.r_simplx.Rule.id (node,x) map 
			     else 
			       map 
			   with 
			     Not_found -> 
			       IntMap.add x.r_simplx.Rule.id (node,x) map
				 )
			 map l)
		     map l1
		     )
		 IntMap.empty rep in 
	     let good_ids = 
	       IntMap.fold 
	       (fun _ (_,x) -> RuleIdSet.add x) 
		 count 
		 RuleIdSet.empty 
	     in 
	     let rep = 
	       List.map 
		 (fun x -> 
		   List.map 
		     (fun (l,b) -> 
		       (List.filter (fun x -> RuleIdSet.mem x good_ids) l,b))
		     x)
		 rep in 
	     let rep = 
	       List.map 
		 (fun x -> 
		   List.filter (fun (l,b) -> l<>[]) x)
		 rep in 
     let _ = 
       try 
	 begin
	   if fic = "" 
	   then ()
	   else 
	     let output = open_out fic in 
	     let print s = Printf.fprintf output s in 
	     let print_opt x  = if !Config_complx.keep_comments then  print "%s" x else () in 
             
	     let dep,map,l = 
	       List.fold_left 
                 (fun (m1,m2,idl) l1 -> 
                   (List.fold_left 
                      (fun (m1,m2,idl) (l,b) -> 
			match l 
			with 
			  [] -> (m1,m2,idl)
		        | rid1::a2 -> 
		            (List.fold_left  
                	       (fun (m1,m2,idl) rid2 -> 
		                 (RuleIdMap.add rid2  rid1 m1,
				  m2,
				  rid2::idl))
                               (m1,RuleIdMap.add rid1 (l,b) m2,rid1::idl)
			       a2)) 
		      (m1,m2,idl) l1))
		 (RuleIdMap.empty,RuleIdMap.empty,[]) rep in 
	     let rec aux cl lid = 
               match cl with 
		 [] -> ()
	       | (Decl a)::q -> 
		   (
		   print "%s" a;
		   print "\n";aux q lid )
	       | (Mutt a)::q -> 
		   (print_opt a;print_opt "\n";aux q  lid)
	       | (Rgl a)::q -> (
		   let name = 
		     try ( 
		       let id = List.hd lid in 
		       let list = RuleIdMap.find id map in 
		       List.fold_left 
                         (fun s rid -> 
			       if s = "" 
			       then 
				 "'"^(name_of_rule rid)^"'"
			       else 
				 "'"^(name_of_rule rid)^"',"^s)
			     "" (fst list))
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
		   in 
		   let new_flaglength = String.length name + 1 in 
		   let _ = 
		     try (print_opt 
		       (String.make 
			  (new_flaglength - oldflaglength)
			  (String.get !Config_complx.comment 0)))
		     with _ -> () in 
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
		     let f lid  = 
		       try (
			 let id,lid = 
			   match lid with t::q -> t,q 
			   | [] -> raise Exit in
			 let dep = RuleIdMap.find id dep in      
			 let fl = name_of_rule  dep in 
			 print_opt !Config_complx.comment;
			 print_opt "Gathered with ";
			 print_opt fl;
			 print_opt "\n";lid)
		       with Not_found -> 
			 try ( 
			   
			   let id,lid = match lid with t::q -> t,q | [] -> raise Exit in 
			   let (a,b) = 
			     try RuleIdMap.find id map 
                             with Not_found -> error "865"  "" "" ([],[]) in 
			   print_opt !Config_complx.comment;
			   print_opt "simplified  rule:";
			   print_opt "\n";
			   let step  = 
			     if b = ["Cannot be applied"]
			     then 
			       (print_opt !Config_complx.comment;
				1)
			     else 0 in 
			   let _ = 
			     try (print_opt 
				    (String.make 
				       (max 0 (oldflaglength - new_flaglength - step))
				       ' '))
			     with _ -> () in 	
			   let _ = 
			   (match rule.flag with 
			     None -> ()
			   | Some s -> (print "'";
					print "%s" s;
					print "' ")) in 
			   let _ = 
			     List.iter (fun x -> print "%s" x) (List.rev b) in
			   print_opt " ";
			   print_opt !Config_complx.comment;
			   print_opt  id.r_id;
			   print "\n";lid) 
			 with Not_found -> (print_opt "Cannot be applied \n";let id,lid = List.hd lid,List.tl lid in   lid) in 
		     ((let lid = 
				    if a.dir = 1 then f lid else 
				    (let lid = f lid in f lid) in aux q lid)))
				with _ -> 
				  error_frozen "line 871" "" "do_it" (fun () -> raise Exit)
				    )
		     
	     in 
	     let cl =  
	       match pb.txt_lines with 
	         Some l -> l 
	       | None -> [] in
	     
	     let _ = aux cl l  in 
	     let _ = print_opt "\n" in 
	     let _ = close_out output in 
	     () end 
	  with _ -> () in 
rep,
	messages) 
 	 
    end))
  
