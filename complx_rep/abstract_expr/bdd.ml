(* 10/06/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(** Abstract boolean expressions: BDD Implementation *)
(* bdd.ml *)

open Config_complx
open Tools
open Expr
open Kleenean_expr
open Var
open Pb_sig
open Data_structures
open Abstract_expr_sig

(** Choice of implementation for hashtables *)
module H = Hash.HInit


(** This function push an error on the error stack, and launch an exception. In error i x t y, i is an option string that denotes the line number, x is an optional string that denotes a message, t is an optional string that denotes the function that has launched the exception, y is a default value for ongoing the compuation in unsafe mode *)
let error i x t y = 
    unsafe
      (Some x) 
      (Some "bdd.ml") 
      (Some t) 
      (Some i) 
      y  


(** This functor creates bdd modules *)
module BddBool = 
  (functor (KExpr:Kleenean_Expr) 
   (** Implementation of three-values logic expressions *) -> 
    (struct 

      (** module for the implementation of three-values logic expressions *) 
      module K = KExpr

      (** module for the implementation of hashtable of triple boolean attributes,integer,interger; the first component is a boolean attribute, the second the hashkey of the left sibling, the third one is the hashkey of the third sibbling  *)
      module HBIntInt = H(Map.Make (struct type t = b * int * int let compare = compare end))

      (** module for the implementation of hashtables for integers *)
      module HInt = H(IntMap)

      (** module for the implementation of hashtables for pairs of integers *)
      module HIntInt = H(Map.Make (struct type t = int*int let compare = compare end))

      (** abstract type for bdds, in Node(i,v,e1,e2), i is a hashkey, v a variable, e1 the implementation of the set of valuation when v=T, e2 the implementation of the set of valuation when v=F; variables from roots to leaves are increasing; a node has always distinct sibbling; If !Config_complx.hash_cons = T, hashkeys and sharing are optimal: two bdds that are equal when we abstract away hashkeys have the same hashkeys and are stored in the same memory location *)
      type abstract_expr = Leaf of bool | Node of (int*(K.E.V.var*abstract_expr*abstract_expr))
	  
	  
      (** implentation of the whole set *) 
      let bdd_true =  Leaf true

      (** implementation of the empty set *)
      let bdd_false = Leaf false

      (** implementation of the empty set *)
      let ae_false = bdd_false

      (** implementation of the empty set *)
      let empty = ae_false
	  
      (** equality test, optimized when using hash consing *) 
      let bdd_equal a b = if (!Config_complx.hash_cons) then a==b else a=b

      (** equality test, explore bdds recursively *) 
      let rec sanity_equal a b = 
	match a,b with Leaf a,Leaf b -> a=b
	| Node(_,(a,b,c)),Node(_,(a',b',c')) -> a=a' && (sanity_equal b b') && (sanity_equal c c')
	| _ -> false

      (** check wether a bdd is well-formed: variables are increasing from root to leaves, two sibbling are distinct *)
      let rec sanity_check a = 
	match a with Leaf _ -> true 
	| Node(_,(a,b,c)) -> (if (sanity_equal b c) then (print_string "SHARING!";print_newline ();false) else true) 
	      &&
	    (if (not (match b with Node(_,(a',_,_)) -> K.E.V.K.compare a a'<0
	    | _ -> true)) then (print_string "ORDERING";print_newline ();false)
		else true)
	      &&
	    (if (not (match c with Node(_,(a',_,_)) -> K.E.V.K.compare a a'<0
	    | _ -> true)) then (print_string "ORDERING";print_newline ();false) else true)
	      && 
	    (sanity_check b)
	      &&
	    (sanity_check c)


      (** Comment the following line to activate sanity checks*)
      let sanity_check x = true 

      (** Give the set of variables that occurs in a bdd *) 
      let var_of_expr a = 
	let rec aux a sol = 
	  match a with Leaf _ -> sol 
	  | Node(a,(b,c,d)) -> aux c (aux d (
				      if c=bdd_false then sol else 
				      K.E.V.varset_add b sol))
	in aux a K.E.V.varset_empty

      (** give the hash_key of a bdd *)	  
      let hash1_of_bdd a = 
	match a with 
	  Leaf x -> if x then -1 else -2
	| Node(a,_) -> a

      (** give the hash_key for a node (a boolean attibute, the hash_key of the left sibbling, the hash_key of the right sibbling) *)
      let hash3_of_bdd (a,b,c) = K.E.V.b_of_var a,hash1_of_bdd b,hash1_of_bdd c

      (** use this function to create a node, it ensures that the node is smashed when the two sibbling are the same, when hash_consing is used, it ensures maximal sharing and hashkey uniqueness *)
      let node = 
	let node () = 
	  if !Config_complx.hash_cons then 
	    let hash = HBIntInt.create !Config_complx.hashinit in 
	    let n = ref 0 in 
	    let f (a,b,c)  = 
	      (let h = hash3_of_bdd (a,b,c) in 
	    try (HBIntInt.find hash h)
	    with Not_found  -> 
     	      (let k = !n in 
	      (n:=(!n)+1;
	       let rep = Node (k,(a,b,c)) in 
	       HBIntInt.add hash h rep;rep)))
	    in f
	  else 
	    (fun (a,b,c) -> if bdd_equal b c then c else Node(0,(a,b,c)))
	in node () 
    	      
    	  
     (** use this function to create a leaf, it ensures maxiaml sharing *) 
      let leaf x = 
	if !Config_complx.hash_cons then 
	  match x with true -> bdd_true
	  |  false -> bdd_false
	else Leaf x

     (** we do not used partitioning yet, there is no difference between abstract expression and set of reachable states *)	    
      type reachable_states = abstract_expr
	    
     (** is it the whole set *)	    
      let is_ae_true s = (s=bdd_true)

     (** convert a partitioned bdd into a set of reachable states *)	  
      let reachable_states_of_abstract_expr a = a

     (** to iter a unary function to a valuation set *)
      let generic_unary f = 
	if !Config_complx.memoisation then 
	  begin
	    let hash = HInt.create !Config_complx.hashinit in
	    let rec generic_unary f a1 = 
	      let h1 = hash1_of_bdd a1 in 
	      try (HInt.find hash h1) 
	      with Not_found -> 
		let rep = 
		  match a1 with 
		    Node(a,(v1,e1,f1)) -> 
		      let e2 = generic_unary f e1 in 
		      let f2 = generic_unary f f1 in 
		      if bdd_equal e2 f2 then e2 else node(v1,e2,f2)
		  | Leaf(a) -> leaf(f a) 
		in 
		(HInt.add hash h1 rep;rep)
	    in generic_unary f
	  end
	else 
	  begin
	    let rec generic_unary f a1 = 
	      match a1 with 
		Node(_,(v1,e1,f1)) -> 
		  let e2 = generic_unary f e1 in 
		  let f2 = generic_unary f f1 in 
		  if bdd_equal e2 f2 then e2 else node(v1,e2,f2)
	      | Leaf(a) -> leaf(f a)
	    in generic_unary f 
	  end
	    
     (** to compute the complement of a valuation set *)
      let bdd_not = generic_unary (fun x -> not x)

     (** to compute a binary operator over two valuation sets
         it exploits sharing: if two bdds are the same, it applies g instead of f  *)	  
      let generic_binary_smart f g = (* require that f a1 a1 = g a1 *)
	if !Config_complx.memoisation then
	  begin
	    let hash = HIntInt.create !Config_complx.hashinit in
	    let rec aux a1 a2 = 
	      if a1==a2  then g a1 else
	      begin
		let h1,h2 = hash1_of_bdd a1,hash1_of_bdd a2 in
		try (HIntInt.find hash (h1,h2)) 
		with Not_found -> 
		let rep = 
		  match a1,a2 with 
		    Node(_,(v1,e1,f1)),Node(_,(v2,e2,f2)) when K.E.V.K.compare v1 v2 = 0 -> 
		      let e3 = aux e1 e2 in 
		      let f3 = aux f1 f2 in 
		      if bdd_equal e3 f3 then e3 else node(v1,e3,f3)
		  | Node(_,(v1,e1,f1)),Node(_,(v2,_,_)) when  K.E.V.K.compare v1 v2 < 0 -> 
		      let e3 = aux e1 a2 in 
		      let f3 = aux f1 a2 in 
		      if bdd_equal e3 f3 then e3 else node(v1,e3,f3)
		  | Node(_,(v1,_,_)),Node(_,(v2,e2,f2)) when  K.E.V.K.compare v1 v2 > 0 -> 
		      let e3 = aux a1 e2 in 
		      let f3 = aux a1 f2 in 
		      if bdd_equal e3 f3 then e3 else node(v2,e3,f3)
		  |	Leaf a,Leaf b -> if f a b then bdd_true else bdd_false
		  | _,Leaf a -> 
		      (match f true a,f false a with 
			true,true -> bdd_true
		      | true,false -> a1 
		      | false,true -> bdd_not a1
		      | false,false -> bdd_false)
		  | Leaf a,_ -> 
		      (match f a true,f a false  with 
			true,true -> bdd_true
		      | true,false -> a2
		      | false,true -> bdd_not a2
		      | false,false -> bdd_false)
		  |	_ -> (print_string "BDD_BINARY";raise Exit) in 
		(HIntInt.add hash (h1,h2) rep;rep)
	      end
	    in aux 
	  end 
	else 
	  let rec aux a1 a2 =
	    if a1 == a2 then g a1 else
	    begin
	      match a1,a2 with 
		Node(_,(v1,e1,f1)),Node(_,(v2,e2,f2)) when compare v1 v2 = 0 -> 
		  let e3 = aux e1 e2 in 
		  let f3 = aux f1 f2 in 
		  if bdd_equal e3 f3 then e3 else node(v1,e3,f3)
	    | Node(_,(v1,e1,f1)),Node(_,(v2,_,_)) when  K.E.V.K.compare v1 v2 < 0 -> 
		let e3 = aux e1 a2 in 
		let f3 = aux f1 a2 in 
		if bdd_equal e3 f3 then e3 else node(v1,e3,f3)
	    | Node(_,(v1,_,_)),Node(_,(v2,e2,f2)) when  K.E.V.K.compare v1 v2 > 0 -> 
		let e3 = aux a1 e2 in 
		let f3 = aux a1 f2 in 
		if bdd_equal e3 f3 then e3 else node(v2,e3,f3)
		  
	    |	Leaf a,Leaf b -> if f a b then bdd_true else bdd_false
	    | _,Leaf a -> 
		(match f true a,f false a with 
		  true,true -> bdd_true
		| true,false -> a1
		| false,true -> bdd_not a1
		| false,false -> bdd_false)
	    | Leaf a,_ -> 
		(match f a true,f a false  with 
		  true,true -> bdd_true
		| true,false -> a2
		| false,true -> bdd_not a2
		| false,false -> bdd_false)  
	    |	_ -> (print_string "BDD_BINARY";raise Exit)
	    end
	  in aux 
	    
      (** implementation of the whole set *)
      let ae_true = bdd_true
	  
    
      (** pretty_print *) 
     let rec print_bdd a = 
	match a with Node(_,(a,b,c)) -> (K.E.V.print_var a;
					 print_string ":";
					 print_bdd b;
					 print_string ";";
					 print_bdd c)
	|  Leaf(true) -> (print_string "true";print_newline ())
	|  Leaf(false) -> (print_string "false";print_newline ()) 

     (** upgrade2 f s transforms a binary operator f so that sanity checks are performed, in case of error, the string s is prompted before a description of the arguments *)	
     let upgrade2 f s a b = 
	let sa = sanity_check a in
	let sb = sanity_check b in 
	let rep = f a b in
	let srep = sanity_check rep in 
	if (not srep) 
	then 
	  (print_string "SANITY WARNING";
	   (if not sa then print_string "PBARGA");
	   (if not sb then print_string "PBARGB");
	   print_string s;print_newline ();
	   print_string "ARGA";
	   print_newline ();
	   print_bdd a;
	   print_newline ();
	   print_string "ARGB";
	   print_newline ();
	   print_bdd b;
	   print_string "REP";
	   print_newline ();
	   print_bdd rep;
	   print_newline ();

	   rep)
	else 
	  rep

	           
    
      	  
      
     
      (** meet of two valuation sets *)
      let bdd_and = 
	let bdd_and = generic_binary_smart (fun a b -> a && b) (fun x -> x) in
	upgrade2 bdd_and "BDD_AND"
      
      (** union of two valuation sets *) 	  
      let bdd_or = 
	let bdd_or = generic_binary_smart (fun a b -> a || b) (fun x -> x) in 
	upgrade2 bdd_or  "BDD_OR"
      
      (** set of valuations that takes the same values *)	  
      let bdd_equiv = 
	let bdd_equiv = generic_binary_smart (fun a b -> a=b) (fun x -> bdd_true) in 
	upgrade2 bdd_equiv "BDD_EQU"

      (** set of valuations for which if it takes the values T in the first, it takes the value T in the second *)
      let bdd_imply = 
	let bdd_imply = generic_binary_smart (fun a b -> if a then b else true) (fun x -> bdd_true) in
	upgrade2 bdd_imply "BDD_IMP"

      (** translate a boolean expression into a bdd *)
      let rec bdd_of_expr a = 
	match a with 
	  K.E.Var x -> node(x,leaf(true),leaf(false))
	| K.E.Not x -> bdd_not (bdd_of_expr x)
	| K.E.And (e1,e2) -> 
	    bdd_and
	      (bdd_of_expr e1) 
	      (bdd_of_expr e2) 
	| K.E.Or (e1,e2) -> 
	    bdd_or 
	      (bdd_of_expr e1)
	      (bdd_of_expr e2)
	| K.E.Equiv(e1,e2) -> 
	    bdd_equiv
	      (bdd_of_expr e1)
	      (bdd_of_expr e2)
	| K.E.Imply (e1,e2) -> 
	    bdd_imply
	      (bdd_of_expr e1)
	      (bdd_of_expr e2)
	| K.E.True -> bdd_true
	| K.E.False  -> bdd_false
	      
      (** translate a boolean expression into a (not-partitioned) set of valuations *)
      let reachable_states_of_expr = bdd_of_expr

      (** translate a boolean expression into a (partitioned) set of valuations *)  
      let abstract_expr_of_expr = bdd_of_expr

      (** translate a (partitioned) set of valuations into an expression *)	      let expr_of_abstract_expr  = 
	if !Config_complx.local_memoisation then 
	  let hash = HInt.create !Config_complx.hashinit in 
	  let rec f x = 
	    match x with 
	      Leaf(true) -> K.E.expr_true
	    | Leaf(false) -> K.E.expr_not (K.E.expr_true)
	    | Node(k,y) -> 
		try (HInt.find hash k) 
		with Not_found  -> 
		  let (a,b,c)=y in 
		  let rep = 
		    K.E.expr_and 
		      (K.E.expr_imply (K.E.expr_atom a) (f b))
		      (K.E.expr_imply (K.E.expr_not (K.E.expr_atom a)) (f c))
		  in (HInt.add hash k rep;rep)
	  in f
	else 
	  let rec f x = 
	    match x with 
	      Leaf(true) -> K.E.expr_true
	    | Leaf(false) -> K.E.expr_not (K.E.expr_true)
	    | Node(a,y) -> 
		let (a,b,c)=y in 
		let rep = 
		  K.E.expr_and 
		    (K.E.expr_imply (K.E.expr_atom a) (f b))
		    (K.E.expr_imply (K.E.expr_not (K.E.expr_atom a)) (f c))
		in rep
	  in f
	    
      let reachable_states_of_valuation_list a s= 
	bdd_of_expr 
	  (K.E.expr_of_valuation_list a s)
	  
	  
      let conj =bdd_and	     
      let tautology expr = bdd_equal expr (leaf(true))

      let test expr v b = bdd_and expr (node(v,leaf (not b),leaf b)) 
      let filtre p   = 
	if !Config_complx.local_memoisation 
	then 
	  let hash = HInt.create !Config_complx.hashinit in 
	  let rec aux expr = 
	    match expr with 
	      Node(k,(a,b,c)) -> 
		begin
		  try (HInt.find hash k) 
		  with Not_found -> 
		    let rep = 
		      if p a then 
			let repb = aux b  in
			let repc = aux c in 
			if bdd_equal repb repc then repb else 
			bdd_or repb repc
		      else 
			let repb = aux b in
			let repc = aux c in 
			if bdd_equal repb repc then repb else 
			node(a,repb,repc) in 
		    (HInt.add hash k rep;rep) 
		end
	    | Leaf(_) -> expr
	  in aux 
	else
	  let rec aux expr = 
	    match expr with 
	      Node(k,(a,b,c)) -> 
		begin
		  let rep = 
		    if p a then 
		      let repb = aux b  in
		      let repc = aux c in 
		      if bdd_equal repb repc then repb else 
		      bdd_or repb repc
		    else 
		      let repb = aux b in
		      let repc = aux c in 
		      if bdd_equal repb repc then repb else 
		      node(a,repb,repc) in 
		  rep
		end
	    | Leaf(_) -> expr
	  in aux 
	    
      let is_bot x = x=bdd_false
      let forget expr f = filtre f expr 
      let forgetone e v = filtre (fun x -> x=v) e
      let project expr f = filtre (fun x-> not (f x)) expr

      let set expr f = 
	if (not (sanity_check expr)) then (print_string "SET";raise Exit)
	else 
	  let l = List.rev (K.E.V.varmap_fold (fun a b c -> (a,b)::c) f []) in 	   
	  if l = [] then expr
	  else 
	    let rec aux e l = 
	      match l with [] -> e 
	      | (t,bool)::q -> begin
		  match e with 
		    Node(a,(b,c,d)) when K.E.V.K.compare b t < 0 -> 
		      let c' = aux c l in 
		      let d' = aux d l in 
		      if bdd_equal c' d' then c'
		      else node(b,c',d')
		  | Node(a,(b,c,d)) when K.E.V.K.compare b t = 0 -> 
		      let c' = bdd_or (aux c q) (aux d q) in 
		      let d'=bdd_false in 
		      if bdd_equal c' d' then c' else
		      if bool then 
			node(b,c',d') else node (b,d',c')
		  | _ -> (
		      let c' = aux e q in 
		      let d' = bdd_false in 
		      if bdd_equal c' d' then c' else
		      if bool then node(t,c',d') else node(t,d',c'))
	      end
	    in aux expr l 
	   
      let weak_set expr f = 
	if (not (sanity_check expr)) then (print_string "WEAKSET";raise Exit)
	else 
	  let l = List.rev (K.E.V.varmap_fold (fun a b c -> (a,b)::c) f []) in 	   
	  if l = [] then expr
	  else 
	    let rec aux e l = 
	      match l with [] -> e 
	      | (t,bool)::q -> 
		  begin
		    match e with 
		      Node(a,(b,c,d)) when K.E.V.K.compare b t < 0 -> 
			let c' = aux c l in 
			let d' = aux d l in 
			if bdd_equal c' d' then c'
		      else node(b,c',d')
		    | Node(a,(b,c,d)) when K.E.V.K.compare b t = 0 -> 
			let f = bdd_or (aux c q) (aux d q) in 
			if bool then 
			  if bdd_equal f d then d
			  else 
			    node(b,f,d) 
			else 
			if bdd_equal f c then c
			else  node (b,c,f)
		    | _ -> aux e q
		  end
	    in aux expr l    

      let switch expr s = 
	if K.E.V.varset_is_empty s then expr 
	else
	  if !Config_complx.local_memoisation 
	  then 
	    let hash = HInt.create (if !Config_complx.local_memoisation then !Config_complx.hashinit else 1) in 
	    let rec aux e = 
	      match e with 
		Node(a,(b,c,d))  -> 
		  begin
		    try (HInt.find hash a) 
		    with Not_found -> 
		      let rep = 
			if  K.E.V.varset_mem b s 
			then node(b,aux d,aux c) 
			else node(b,aux c,aux d)
		      in 
		      (HInt.add hash a rep;rep)
		  end
	      | Leaf _ -> e
	    in aux expr 
	  else 
	    let rec aux e = 
	      match e with 
		Node(a,(b,c,d))  -> 
		  begin
		    let rep = 
		      if  K.E.V.varset_mem b s 
		      then node(b,aux d,aux c) 
		      else node(b,aux c,aux d)
		    in 
		    rep
		  end
	      | Leaf _ -> e
	    in aux expr 
	      
      let union = bdd_or
      let is_included a b = tautology (bdd_imply b a)

      let may_be_true expr s = 
	let rec aux expr s = 
	  match expr
	      with Leaf false -> K.E.V.varset_empty
	  | Leaf true -> s
	  (*| Node(_,(a,b,c)) when bdd_equal b bdd_false ->
	      let s'= K.E.V.varset_remove a s in 
	      aux c s'*)
	  | Node(_,(a,b,c)) -> K.E.V.varset_union (aux b s) (aux c (K.E.V.varset_remove a s))
	in aux expr s 
	  

      let rec rename expr f = 
	match expr with Leaf(_) -> expr
	|  Node(_,(a,b,c)) -> 
	    let repb = set (rename (forgetone b a ) f) (K.E.V.varmap_add (f a) false K.E.V.varmap_empty) in 
            let repc = set (rename (forgetone c a ) f) (K.E.V.varmap_add (f a) true K.E.V.varmap_empty) in 
	    union repb repc 
      let upgrade f x = K.E.V.var_of_b (upgrade_renaming f (K.E.V.b_of_var x))
      let rec increasing_renaming expr f = 
	match expr with Leaf(_) -> expr
	|  Node(_,(a,b,c)) -> node(upgrade f a,increasing_renaming b f,increasing_renaming c f)
	      
      let guard s = 
	K.ksfold 
	  (fun (_,_,x) ->
	    bdd_of_expr 
	      (K.expr_of_kleenean_valuation x))
	  bdd_or
	  s (leaf(false))

 
	      
      let fnd_of_bdd a = 
	let rec aux to_visit sol = 
	  match to_visit with 
	    (path,Node(_,(a,b,c)))::q -> 
	      aux (((a,true)::path,b)::(((a,false)::path),c)::q) sol 
	  |   (path,Leaf true)::q -> aux q (path::sol)
	  | (path,_)::q -> aux q sol 
	  | [] -> sol in 
	let liste_true = aux [[],a] [] in liste_true

	  
      let export_ae a k pb h  = 
	let map = pb.pretty_map  in
	let fnd = fnd_of_bdd a in 
	(list_fold 
	   (fun fnd sol -> 
	     let new_sol = 
	       let map = 
		 list_fold (fun (var,bool) (set,map) -> 
		   let b = K.E.V.b_of_var var in 
		   match b with 
		     H (_,_)-> (bool,map)
		   | B(a,_,b) -> set,StringMap.add b {(StringMap.find b map) with is_bound = Init bool} map 
		   | M((a,_,a'),c) -> 
		       set,StringMap.add a'
			 (
			 if bool 
			 then {(StringMap.find a' map) with mark = Init c}
			 else
			   let tuple = StringMap.find a' map in 
			   match tuple.impossible_marks with 
			     Init t -> {tuple with impossible_marks = (Init(c::t))}
			   | _    -> {tuple with impossible_marks = (Init([c])) }
			 ) map

		   | AL((a,_,a'),(b,b')) ->
		       set,StringMap.add a' 
			 (
			 if bool 
			 then {(StringMap.find a' map) with link=Init (b,b')}
			 else 
			   let tuple = StringMap.find a' map in
			   match tuple.impossible_links with
			     Init t -> {tuple with impossible_links = (Init((b,b')::t))}
			   | _    -> {tuple with impossible_links = Init [b,b']}
			   )
			 map
		   | L _  | Connected _ | Forb _ | Dis _ -> set,map)
		   fnd (true,StringMap.find k map) in 
	       (k,map,h) in 
	     new_sol::sol) fnd [])

      let print_export_ae exp print_any log = 
	[[],
	List.fold_left 
	  (fun sol (k,(bool,map),h) -> 
	    if (not bool) then sol
	    else 
	      let a,b,c = print_pretty k (fun _ -> true) 
			    ((StringMap.add k map StringMap.empty),1) 
			    h 
		  print_any "" (fun x -> x) (fun x -> x) None log 
	      in 
	      if  a
	      then (print_option empty_prefix log "\n")
	      else ();
	      ((List.rev b)::sol)) []
	  exp]
	  
      let print a (k:string) pb h  print_any log = print_export_ae (export_ae a k pb h) print_any  log
	  
      let export_reachable_states access specie_of_id  pb = 
	(if (not (sanity_check access)) then (print_string "EXPORT";raise Exit));
	let map = pb.pretty_map in 

	
        let fnd = fnd_of_bdd access  in
	  list_fold 
            (fun fnd l ->
	       let newsol = 
		 (list_fold (fun (var,bool) (hb,(sol,n)) ->
			       let b = K.E.V.b_of_var var in
				 try (match b with
					  Connected _ | Dis _ | Forb _ -> hb,(sol,n)
             				| H (a,_) -> 
					    if bool then hb,(sol,n)
					    else StringSet.add a hb,(sol,n)
					| B(a,_,b) -> hb,
				      let spec = try (StringMap.find a sol) 
				      with Not_found -> (StringMap.find (specie_of_id a) map) in
					StringMap.add 
					  a
					  (StringMap.add b {(StringMap.find b spec) with is_bound = Init bool} spec) sol,n 
					| L((a,_,a'),(b,_,b')) -> hb,
				      let f a a' b b' sol = 
					let spec = 
					  try (StringMap.find a sol) 
					  with Not_found -> (StringMap.find (specie_of_id a) map) in
					  StringMap.add 
			a
					    (StringMap.add a' 
					       (let tuple = StringMap.find a' spec in
						  if bool then 
						    {tuple with link = Init (bound_of_number n)} 
						  else
						    match tuple.impossible_links with 
							Init t -> {tuple with impossible_links = Init ((b,b')::t)}
						      | _ -> {tuple with impossible_links = Init [b,b']}
					       ) spec)
					    sol in 
				      let sol = f a a' b b' (f b b' a a' sol) in 
					sol,(if bool then (n+1) else n)
					|  M((a,_,a'),c) -> hb,
				      let spec = try (StringMap.find a sol)
				      with Not_found -> (StringMap.find (specie_of_id a) map) in 
					StringMap.add 
					  a 
					  (StringMap.add a'
					     (let tuple = StringMap.find a' spec in 
						if bool
						then 
						  {tuple with mark = Init(c)}
						    
						else 
						  {tuple with impossible_marks = 
						      match tuple.impossible_marks with 
							  Init(t) -> Init (c::t)
							| _ -> Init [c]})
					     spec) sol,n
					    
					|  AL((a,_,a'),(b,b')) -> hb,
				      let spec = try (StringMap.find a sol) 
				      with Not_found -> (StringMap.find (specie_of_id a) map) in
					StringMap.add 
					  a
					  (StringMap.add a' 
					     (let tuple = StringMap.find a' spec in
						if bool 
						then {tuple with link = Init(b,b')}
						else 
						  match tuple.impossible_links  with 
						      Init (t) -> {tuple with impossible_links = Init ((b,b')::t)}
						    | Any -> {tuple with impossible_links = Init [b,b']}
						    | _ -> tuple)
					     spec) 
					  sol,n) 
				 with Not_found -> error "line 735" ("NotFOUND"^(string_of_b b)) "export_reachable_states"  (hb,(sol,n)))
		    
		    fnd (StringSet.empty,(StringMap.empty,1))) in 
	    (newsol::l))
	  fnd []

  let export_reachable_states2 access specie_of_id  pb = 
	(if (not (sanity_check access)) then (print_string "EXPORT";raise Exit));
	let map = pb.pretty_map in 

	
        let fnd = fnd_of_bdd access  in
	  list_fold 
            (fun fnd l ->
	       let newsol = 
		 (list_fold (fun (var,bool) (hb,(sol,n)) ->
			       let b = K.E.V.b_of_var var in
				 try (match b with
					  Connected _ | Forb _ | Dis _ -> hb,(sol,n)
             				| H (a,_) -> 
					    if bool then hb,(sol,n)
					    else StringSet.add a hb,(sol,n)
					| B(a,_,b) -> hb,
				      let spec = try (StringMap.find a sol) 
				      with Not_found -> (StringMap.find (specie_of_id a) map) in
					StringMap.add 
					  a
					  (StringMap.add b {(StringMap.find b spec) with is_bound = Init bool} spec) sol,n 
					| L((a,_,a'),(b,_,b')) -> hb,
				      let f a a' b b' sol = 
					let spec = 
					  try (StringMap.find a sol) 
					  with Not_found -> (StringMap.find (specie_of_id a) map) in
					  StringMap.add 
			a
					    (StringMap.add a' 
					       (let tuple = StringMap.find a' spec in
						  if bool then 
						    {tuple with link = Init (bound_of_number n)} 
						  else
						    match tuple.impossible_links with 
							Init t -> {tuple with impossible_links = Init ((b,b')::t)}
						      | _ -> {tuple with impossible_links = Init [b,b']}
					       ) spec)
					    sol in 
				      let sol = f a a' b b' (f b b' a a' sol) in 
					sol,(if bool then (n+1) else n)
					|  M((a,_,a'),c) -> hb,
				      let spec = try (StringMap.find a sol)
				      with Not_found -> (StringMap.find (specie_of_id a) map) in 
					StringMap.add 
					  a 
					  (StringMap.add a'
					     (let tuple = StringMap.find a' spec in 
						if bool
						then 
						  {tuple with mark = Init(c)}
						    
						else 
						  {tuple with impossible_marks = 
						      match tuple.impossible_marks with 
							  Init(t) -> Init (c::t)
							| _ -> Init [c]})
					     spec) sol,n
					    
					|  AL((a,_,a'),(b,b')) -> hb,
				      let spec = try (StringMap.find a sol) 
				      with Not_found -> (StringMap.find (specie_of_id a) map) in
					StringMap.add 
					  a
					  (StringMap.add a' 
					     (let tuple = StringMap.find a' spec in
						if bool 
						then {tuple with link = Init(b,b')}
						else 
						  match tuple.impossible_links  with 
						      Init (t) -> {tuple with impossible_links = Init ((b,b')::t)}
						    | Any -> {tuple with impossible_links = Init [b,b']}
						    | _ -> tuple)
					     spec) 
					  sol,n) 
				 with Not_found -> error "line 817"  ("NotFOUND:"^(string_of_b b)) "Export_reachable_states2" (hb,(sol,n)))
		    
		    fnd (StringSet.empty,(StringMap.empty,1))) in 
	    (newsol::l))
	  fnd []
	  
      let print_export_reachable_states exp print_any log  = 
	List.fold_left 
	  (fun sol (s,map) -> 
            let _,string,_ = 
	      StringMap.fold (fun a b (bool,l,n) -> 
		if StringSet.mem a s then (bool,l,n)  
		else 
		  let a,b,c = 
		    print_pretty 
		      a (fun _ -> true)
		      map 
		    tuple_known
		      print_any 
		      (if bool then !Config_complx.solution_separator else "")
		      (fun x -> x) (fun x -> x) None log 
		  in (a,b::l,c))
		(fst map) (false,[],snd map) in		
	    print_option empty_prefix log "\n";
	    let rep = 
	    (List.rev (List.flatten 
		  (string:string list list))) in 
	    if rep = [] then sol else rep::sol)
	  [] exp

   let print_export_reachable_states2 exp print_any hash log  = 
     snd (List.fold_left 
	       (fun (bool1,sol) (s,map) -> 
	 	let _ = 
                    if bool1 then 
                       print_option empty_prefix log ","  in
	
                let _,string,_  = 
		   StringMap.fold (fun a b (bool,l,n) -> 
		     if StringSet.mem a s then (bool,l,n)  
		     else 
		       
		       let a,b,c = 
			 print_pretty 
			   a (fun _ -> true)
			   map 
			   tuple_data
			   print_any 
			   (if bool then !Config_complx.solution_separator else "")
			   (fun x -> x) (fun x -> x) 
			   (Some hash) log 
		       in (a,b::l
			   
			     ,c))
                   (fst map) (false,[],snd map) in
	
	   (* print_option empty_prefix log "\n";*)
		 let rep = 
		   (List.rev (List.flatten 
		  (string:string list list))) in 
	    if rep = [] then true,sol 
	    else true,(rep::sol))
	       (false,[]) exp)

      let print_reachable_states (access:reachable_states) specie_of_id  pb b  = 
	print_export_reachable_states (export_reachable_states access specie_of_id  pb) b 
	  
      let print_reachable_states2 (access:reachable_states) specie_of_id  pb b  = 
	print_export_reachable_states2 (export_reachable_states2 access specie_of_id  pb) b
      let abstract_system mode  s access vars dep messages = 
	let g = match mode with Isolated -> bdd_false  | _ -> guard s in 
	K.E.V.fold_vars 
	  (fun v s -> 
	    let composante = K.E.V.close dep v in 
	    K.ksmap
	      (fun r -> 
		let abs = K.abstract r v in 
		if abs = r 
		then r 
		else 
		  let abs = K.E.V.fold_vars 
		      (fun v r -> K.abstract r v) 
		      composante r in 
		  let e = 
		    bdd_imply 
		      (bdd_of_expr (K.expr_of_kleenean_valuation abs))
		      (bdd_imply access 
                         (match mode 
                         with Isolated -> bdd_of_expr (K.expr_of_kleenean_valuation r)
                         | _ -> 	 g)) in 
		  if tautology e 
		  then abs
		  else r)
	      s)
	  vars s,messages 
	  
      let project a b = 
	let rep = project a b in 
	if (not (sanity_check rep)) then (print_string "BDDPROJECT";raise Exit) else rep
	  
      let r_neg = bdd_not
      let imply = bdd_imply
      let atom_pos x =  node(x,leaf(true),leaf(false))
      let atom_neg x = node(x,leaf(false),leaf(true))
      let r_ae_true = ae_true
      let r_conj = conj
      let r_imply = imply
      let r_union = union
      let r_atom = atom_pos
	  
      let f x = true 
	  
      let all_false_sl l = list_fold (fun a l -> node(K.E.V.var_of_b a,bdd_false,l)) l (bdd_true)



      let list_conj_sl l = 
	list_fold 
	  (fun (a,bool) l ->
	    if bool then 
	      if l=bdd_false 
	      then bdd_false 
	      else node(K.E.V.var_of_b a,l,bdd_false)
	    else node(K.E.V.var_of_b a,bdd_false,l)
	  ) l bdd_true

      

      let list_conj_sl l =
	let rep = list_conj_sl l in 
	if sanity_check rep then rep
	else (print_string "LIST_CONJ_SL";print_newline ();raise Exit)


      let upgrade s f  l = 
	try (let rep = f l in 
	if sanity_check rep then rep else (print_string s;print_newline ();
					    List.iter print_b l;print_newline ();raise Exit))
	    with Exit -> (print_string s;print_newline ();
					    List.iter print_b l;print_newline ();raise Exit)
	  
      let all_false_sl = upgrade "all_falsesl" all_false_sl 

      let atmost_once_in_sl l = 
	snd (list_fold 
	       (fun a (q,l) ->  (a::q,
				 let l1=all_false_sl (List.rev q) in 
	                         let l2=l in 
				 if l1=l2 then l1 else 
				 node(K.E.V.var_of_b a,l1,l2)))
	       l 
	       ([],bdd_true))

	      
      let rec exactly_once_in_sl l = 
	snd (list_fold
	       (fun a (q,l) -> (a::q,node(K.E.V.var_of_b a,all_false_sl  (List.rev q),l)))
	       l 
	       ([],bdd_false))

 
      let sort_blist l  = 
	let l0 = List.sort (fun a b -> (K.E.V.K.compare (K.E.V.var_of_b a) (K.E.V.var_of_b b))) l in 
	let rec aux l old sol = 
	  match l with t::q when t=old -> aux q old sol
	  | t::q -> aux q t (t::sol)
	  | []   -> sol
	in match l0 with [] -> [] | t::q -> aux q t [t]

      let sort_blistbool l  = 
	let l0 = List.sort (fun a b -> (K.E.V.K.compare (K.E.V.var_of_b (fst a)) (K.E.V.var_of_b (fst b)))) l in 
	let rec aux l old sol = 
	  match l with t::q when t=old -> aux q old sol
	  | t::q when fst t = fst old -> None
	  | t::q -> aux q t (t::sol)
	  | []   -> Some sol
	in match l0 with [] -> Some [] | t::q -> aux q t [t]  
     
      let all_false l = all_false_sl (sort_blist l)
      let exactly_once l = exactly_once_in_sl (sort_blist l)
      let atmost_once l = atmost_once_in_sl  (sort_blist l)
      let list_conj l = 
	match (sort_blistbool l) 
	with 
	  None -> bdd_false
	| Some l -> list_conj_sl l 
    
      let r_all_false  = all_false
      let r_exactly_once  = exactly_once
      let r_atmost_once  = atmost_once
      let r_list_conj  = list_conj 

   
       
      let r_all_false_s  = all_false_sl
      let r_exactly_once_s  = exactly_once_in_sl 
      let r_atmost_once_s  = atmost_once_in_sl 
	
      let all_false = upgrade "allfalse" all_false
      let exactly_once = upgrade "exactly_once" exactly_once
      let atmost_once = upgrade "atmost_ince" atmost_once

      let all_false_sl = upgrade "allfalsesl" all_false_sl
      let exactly_once_in_sl = upgrade "exactly_oncesl" exactly_once_in_sl
      let atmost_once_in_sl = upgrade "atmost_incesl" atmost_once_in_sl
      
      let r_all_false_s = upgrade "rallfalses" r_all_false
      let r_exactly_once_s = upgrade "rexaconces" r_exactly_once_s
      let r_atmost_once_s = upgrade "ratmostonces" r_atmost_once_s
      
      let r_all_false = upgrade "rallfalse" r_all_false
      let r_exactly_once = upgrade "rexac" exactly_once
      let r_atmost_once  = upgrade "ratmost" atmost_once 
  
      let list_conj l = 
	let rep = list_conj l in 
	if sanity_check rep then rep 
	else (print_string "LIST_CONJ";raise Exit)

      let r_is_bot x = bdd_equal x bdd_false 

      let upgrade_bin f a b = 
	if a==b then a 
	else f a b

      let union = upgrade_bin union
      let conj = upgrade_bin conj
      let r_union = upgrade_bin r_union
      let r_conj = upgrade_bin r_conj 
      let summarize_abstract_expr a = ["",a]

      let compute_abstract_lens a = 
	if StringMap.is_empty a 
	then 
	  ae_false
	else
	  StringMap.fold 
	    (fun _ a b   ->
	      let rep  = r_conj a b in
	      rep)
	    a r_ae_true 
	    
      let compute_subviews a = KeyMap.add Void a KeyMap.empty
      let restore_subviews a = 
	try 
	  KeyMap.find Void a 
	with Not_found -> 
	  unsafe_frozen 
	    (None) 
	    (Some "bdd.ml") 
	    (Some "resotre_views") 
	    (Some "line 1069")   
	    (fun () -> raise Exit)	
	    
	 end:AbstractExprBool))

    
