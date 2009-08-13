(* 11/05/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Partitionement des expressions abstraites *)
(* partition.ml *)

open Config_complx
open Share
open Tools
open Expr
open Var
open Pb_sig
open Data_structures
open Hash 
open Abstract_expr_sig
open Error_handler 

let error i x t y = 
    unsafe
      (Some x) 
      (Some "Complx")
      (Some "partition.ml") 
      (Some t) 
      (Some i) 
      y

let error_frozen i x t y = 
    unsafe_frozen
      (Some x) 
      (Some "Complx")
      (Some "partition.ml") 
      (Some t) 
      (Some i) 
      y

module type Part =
    sig
      val print: key -> string
      val part: b  -> key  list
      val rename: (string->string) -> key -> key
	  (*for any sigma:string->string *)
	  (* k \in part b *)
	  (* should be equivalent to *)
	  (*   (rename sigma k) \in part (sigma* b) *)

	  (*where sigma* is the extension of sigma to boulean attributes *)
  	  (* sigma*: B(a,b)->B(sigma a,b) *)
	  (*         M((a,b),c)->M((sigma a,b),c) *)
          (*         AL((a,b),c) -> AL((sigma a,b),c) *)
	  (*         L((a,b),(c,d)) -> L((sigma a,b),(sigma c d))*)
	  (*         H(a)           -> H(sigma a) *)
 end


module Part = 
  (functor (AE:AbstractExprBool)
   -> 
     (functor (P:Part) ->
     (struct 
	module AE=AE
	module K = AE.K

      
       type abstract_expr = AE.abstract_expr KeyMap.t 
       type reachable_states = AE.reachable_states

       let print_bdd x = 
	 KeyMap.iter 
	   (fun k a -> 
	     let _ = P.print k in () ;
	     print_newline ();
	     AE.print_bdd a;
	     print_newline ())
	   x

       let ae_true = KeyMap.empty

       let find s map = try (KeyMap.find s map) with Not_found -> AE.ae_true
       let store k s map = 
	 if AE.is_ae_true s
	 then 
	   KeyMap.remove k map
	 else 
	   KeyMap.add k s map 
       
       let is_bot s = 
	 KeyMap.fold 
	   (fun _ b s -> (AE.is_bot b) or s) 
	   s false

	 
       let apply_1ary f  a = KeyMap.fold (fun a b sol -> 
	 let rep=  f b in if rep==b then sol else KeyMap.add a rep sol) a a 
       let apply_2ary f a b  = 
	 let sol=ref a in 
	 let _ = KeyMap.iter2
	   (fun a b -> let rep = f b AE.ae_true in 
                           if rep==b then () else sol:=(KeyMap.add a rep (!sol)))
	   (fun a c -> let rep = f AE.ae_true c in 
	               sol:=KeyMap.add a rep (!sol))
	   (fun a b c -> let rep = f b c in 
	   if rep==b then () else (sol:=KeyMap.add a rep (!sol))) a b in 
	 (!sol)
       let apply_2ary_t f a b = 
	  let sol=ref a in 
	 let _ = KeyMap.iter2
	   (fun a b -> ())
	   (fun a c -> let rep = f AE.ae_true c in 
	               sol:=KeyMap.add a rep (!sol))
	   (fun a b c -> let rep = f b c in 
	   if rep==b then () else (sol:=KeyMap.add a rep (!sol))) a b  in 
	 (!sol)

	 
       let atom f v  = 
	 let b = AE.K.E.V.b_of_var v in 
	 let l = P.part b in 
	 list_fold 
	   (fun x map -> store x (f v) map)
	   l 
	   KeyMap.empty
       let atom_pos = atom AE.atom_pos
       let atom_neg = atom AE.atom_neg
       let r_atom = AE.r_atom 


       let imply = apply_2ary AE.imply 
       let conj  = apply_2ary_t AE.conj  
       let union = apply_2ary_t AE.union 
       let forget ae f = apply_1ary (fun ae -> AE.forget ae f) ae 
       let project ae f = apply_1ary (fun ae -> AE.project ae f) ae 
       let switch ae s = 
	   let m=AE.K.E.V.fold_vars
			(fun x m ->
				list_fold
					(fun k m ->
						let old = try (KeyMap.find k m) with Not_found -> AE.K.E.V.varset_empty in 
						KeyMap.add  k (AE.K.E.V.varset_add x old) m)
					(P.part (AE.K.E.V.b_of_var x))
					m)
			s KeyMap.empty in
	   KeyMap.fold (fun k ae  sol -> let s = try (KeyMap.find k m) with Not_found -> AE.K.E.V.varset_empty in 
	                             let rep = (AE.switch ae s) in 
                                     if ae == rep then sol else store k rep sol ) ae ae 
       let set ae v  = 
	 let m=AE.K.E.V.varmap_fold
	     (fun x b m ->
	       list_fold
		 (fun k m ->
		   let old = try (KeyMap.find k m) with Not_found -> AE.K.E.V.varmap_empty in 
		   KeyMap.add  k (AE.K.E.V.varmap_add x b old) m)
		 (P.part (AE.K.E.V.b_of_var x))
		 m)
	     v KeyMap.empty in
	 KeyMap.fold (fun k ae  sol -> let s = try (KeyMap.find k m) with Not_found -> AE.K.E.V.varmap_empty in 
	 let rep = (AE.set ae s) in 
         if ae == rep then sol else store k rep sol ) ae ae 

	   
       let weak_set ae v  = 
	 let m=AE.K.E.V.varmap_fold
	     (fun x b m ->
	       list_fold
		 (fun k m ->
		   let old = try (KeyMap.find k m) with Not_found -> AE.K.E.V.varmap_empty in 
		   KeyMap.add  k (AE.K.E.V.varmap_add x b old) m)
		 (P.part (AE.K.E.V.b_of_var x))
		 m)
	     v KeyMap.empty in
	 KeyMap.fold (fun k ae  sol -> let s = try (KeyMap.find k m) with Not_found -> AE.K.E.V.varmap_empty in 
	 let rep = (AE.weak_set ae s) in 
         if ae == rep then sol else store k rep sol ) ae ae 

       let may_be_true ae s = 
	 KeyMap.fold  (fun _ ae s ->
	   AE.K.E.V.varset_cap s (AE.may_be_true ae s))
	   ae s

       let is_included a1 a2 = 
	 try (KeyMap.iter2
		(fun x y -> ())
		(fun x y -> if not (AE.is_included AE.ae_true y) then raise Exit else ())
		(fun x a1 a2 -> if not (AE.is_included a1 a2) then raise Exit else ()) a1 a2;true)
	     with Exit -> false

     
       let gather ae = KeyMap.fold (fun _ a sol -> AE.conj a sol) ae AE.ae_true 
       let var_of_expr ae = 
	 KeyMap.fold 
	   (fun _ a sol -> AE.K.E.V.varset_union sol (AE.var_of_expr a))
	   ae AE.K.E.V.varset_empty
       let split ae = 
	 KeySet.fold  
	   (fun k map -> 
	     store k 
	       (AE.project ae (fun x -> List.mem k (P.part (AE.K.E.V.b_of_var x)))) (*to do improve *)
	       map)
	   (AE.K.E.V.fold_vars 
	      (fun x sol -> list_fold KeySet.add (P.part (AE.K.E.V.b_of_var x)) sol)
	      (AE.var_of_expr ae) KeySet.empty)
	   KeyMap.empty
       let rename ae f = split (AE.rename (gather ae) f)
       let increasing_renaming ae f = 
	 KeyMap.fold 
	   (fun a ae map -> KeyMap.add (P.rename f a) (AE.increasing_renaming ae f) map)
	   ae (KeyMap.empty)
       let test ae v b =  
		let klist = P.part (AE.K.E.V.b_of_var v) in
                        list_fold
                                (fun k ae -> store  k (AE.test (find k ae) v b) ae) klist ae

	   
       let reachable_states_of_expr = AE.reachable_states_of_expr
      

       let reachable_states_of_abstract_expr ae = AE.reachable_states_of_abstract_expr (gather ae) 
       let reachable_states_of_valuation_list = AE.reachable_states_of_valuation_list 

       let abstract f k marks pb = 
	 let b' = pb.pretty_map in 
	 {pb with pretty_map = 
	   StringMap.mapi
	      (fun a b -> 
		StringMap.mapi
		  (fun s tuple  -> 
		    {{tuple with is_marked = 
		      if List.exists (fun m -> List.mem k (f (M((a,a,s),m)))) marks 
		      then tuple.is_marked else Abstracted}
		    with is_bound = if List.mem k (f (B(a,a,s))) then tuple.is_bound else Abstracted}
		  ) b) b'}
	     
	   
       let print handler ae f g h pretty_bool log = 
	 let cpb = 
	   match g.intermediate_encoding with 
	     None -> error "line 232" "Intermediar encoding is missing" "print" (raise Exit)
	   | Some a -> a in 
	 let marks = cpb.cpb_marks in 
         KeyMap.fold
	   (fun  a b sol -> 
	     (let b'=AE.forget b (fun x -> match AE.K.E.V.b_of_var x with H _ -> true | _ -> false)  in 
	     if AE.is_ae_true b' then sol
		 
	     else 
	       (print_option empty_prefix log (P.print a) ;
		let rep = 
		  try (let rep = 
		    AE.print handler b f 
		      (abstract (P.part) a marks g) 
		      h  pretty_bool log  in 
		       let _ = print_option empty_prefix log "\n\n" in rep) with _ -> [] 
		in 
		List.flatten [List.map (fun (a1,b1) ->((P.print a)::a1,b1))
				      rep ;sol])))
	   ae []

       let fnd_of_bdd ae = AE.fnd_of_bdd (gather ae)   
       let expr_of_abstract_expr ae = AE.expr_of_abstract_expr (gather ae)    

       let aexpr_of_kleenean_valuation v = 
	 (K.kfold 
	    (fun x b expr -> 
	      match b with 
		TRUE -> conj expr (atom_pos x)
	      | FALSE -> conj expr (atom_neg x)
	      | ANY -> expr)
	    v ae_true)
		 
       let guard s = 
	 AE.K.ksfold (fun (_,_,x) -> aexpr_of_kleenean_valuation x) union s (ae_true)


       let abstract_system = AE.abstract_system
      
       let r_neg = AE.r_neg
       let r_ae_true = AE.r_ae_true
       let r_imply = AE.r_imply
       let r_union = AE.r_union
       let r_conj = AE.r_conj
       let r_all_false = AE.r_all_false
       let r_exactly_once = AE.r_exactly_once
       let r_list_conj = AE.r_list_conj
       let r_atmost_once = AE.r_atmost_once 

       let r_exactly_once_s = AE.r_exactly_once_s
       let r_atmost_once_s = AE.r_atmost_once_s
       let r_all_false_s = AE.r_all_false_s

 	     
       let print_reachable_states  = AE.print_reachable_states 
       let print_reachable_states2 = AE.print_reachable_states2 
       let is_ae_true s = s=KeyMap.empty

       let f x = P.part x <> [] 

       let all_false l =
	let m =  list_fold (fun a sol -> 
	   let lkey = P.part a in 
	   list_fold 
	     (fun k sol ->
	       let set = try (KeyMap.find k sol) with Not_found -> []  in 
	       KeyMap.add k (a::set) sol)
	     lkey sol)
	   l KeyMap.empty in 
	KeyMap.map AE.all_false m
	  
       let list_conj l =
	 let m =  list_fold (fun a sol -> 
	   let lkey = P.part (fst a) in 
	   list_fold 
	     (fun k sol ->
	       let set = try (KeyMap.find k sol) with Not_found -> []  in 
	       KeyMap.add k (a::set) sol)
	     lkey sol)
	     l KeyMap.empty in 
	 KeyMap.fold (fun k m map -> if m = [] then map else KeyMap.add k ( AE.list_conj m) map) m KeyMap.empty

	   
       let atmost_once l = 
	 let m = 
	   list_fold 
	     (fun a sol -> 
	       let lkey = P.part a in
	       list_fold 
		 (fun k sol -> 
		   let set = try (KeyMap.find k sol) with Not_found -> [] in 
		   KeyMap.add k (a::set) sol)
		 lkey sol)
	     l KeyMap.empty in 
	 KeyMap.map AE.atmost_once m

       let exactly_once l = 
         let map,m = 
	   list_fold 
	     (fun a (sol,m) -> 
	       let lkey = P.part a in
	       (list_fold 
		 (fun k sol -> 
		   let set,n = try (KeyMap.find k sol) with Not_found -> [],0 in 
		   KeyMap.add k (a::set,n+1) sol)
		 lkey sol),m+1)
	     l (KeyMap.empty,0) in 
	 KeyMap.map 
	   (fun (b,n) -> 
	     if m=n 
	     then AE.exactly_once b 
	     else AE.atmost_once b)
	   map

      

       let export_ae ae  = AE.export_ae (gather ae)
       let r_is_bot x = AE.r_is_bot x 
 	 
       let summarize_abstract_expr a = 
	 KeyMap.fold 
	   (fun a b sol -> 
	      List.fold_left 
		(fun sol (k,l) -> ((P.print a)^k,l)::sol)
		[] sol)
	   a []


       let compute_abstract_lens = AE.compute_abstract_lens

       let compute_subviews a = 
	 KeyMap.fold 
	   (fun a b ->
	     let x = AE.compute_subviews b in
	     KeyMap.fold 
	       (fun a' e -> 
		 KeyMap.add 
		   (Conj(a,a'))
		   e)
	       x) a KeyMap.empty

       let restore_subviews a = 
	 let sol = 
	   KeyMap.fold 
	     (fun a b sol ->
	       match a with Conj(a,a') -> 
		 let old = 
		 try KeyMap.find a sol
		 with Not_found -> KeyMap.empty 
		 in
		 KeyMap.add a (KeyMap.add a' b old) sol
	     |  _ -> error_frozen "line 384" "the argument should onby be made of conjuncts" "restore_subviews" (fun () -> raise Exit))
	     a KeyMap.empty
	 in 
	 KeyMap.map AE.restore_subviews sol 


	   
end:AbstractExprBool with type reachable_states = AE.reachable_states)))


module Carthesien = 
  (functor (P1:Part) -> 
    (functor (P2:Part) -> 
      (struct
	
	let print a = 
	  match a with 
	    Conj(a,b) -> (P1.print a)^(P2.print b)
	  | _ -> error_frozen "line 402" "the argument should only be made of conjuncts" "Carthesian.print" (fun () -> raise Exit)
	let rename f a = 
	  match a with
	    Conj(a,b) -> Conj (P1.rename f a,P2.rename f b)
	  | _ -> error_frozen "line 406" "the argument should only be made of conjuncts" "Carthesian.print" (fun () -> raise Exit)

	let part x = 
	  list_fold (fun a sol -> 
	    list_fold (fun b sol -> (Conj (a,b))::sol)
	      (P2.part x) sol) 
	    (P1.part x) []
	    end:Part)))


module Somme =
  (functor (P1:Part) ->
    (functor (P2:Part) ->
      (struct
	let upgrade f g a = 
	  match a with 
	    Disj(false,a) -> Disj(false,f a)
	  | Disj(true,a) -> Disj(true,g a)
	  |  _ ->  
	       error_frozen "line 425" "the argument should only be made of disjunct" "Somme.upgrade" (fun () -> raise Exit)
       	let print a =  
	  match a with 
	    Disj(false,a) -> P1.print a
	  | Disj(true,a) -> P2.print a 
	  |  _ ->  error_frozen "line 430" "the argument should only be made of disjunct" "Somme.print" (fun () -> raise Exit)
	let rename f  = upgrade (P1.rename f) (P2.rename f)
	let part x = 
	  list_fold 
	    (fun a sol -> (Disj(false,a))::sol)
	    (P1.part x) 
	    (list_fold 
	       (fun a sol -> (Disj(true,a))::sol)
	       (P2.part x) [])
	       end:Part)))
		
module PSite = 
  struct 
    let print x = 
      match x with String2 (_,b) -> ("SITE: "^b^"\n\n")
      |	 _ -> error_frozen "line 445" "Incompatible type construct" "PSite.print" (fun () -> raise Exit)
    let part b = 
      let f (a,_,b) = String2 (a,b) in 
      match b with 
	M (x,_) | AL(x,_) |B x -> [f x]
      | L(a,b) -> [f a;f b]
      | H _ | 
	Connected (_,_) | Forb _ | Dis _ -> []
    let rename f x = 
      match x with 
	String2(a,b) -> String2(f a,b)
      |	 _ -> error_frozen "line 456" "Incompatible type construct"  "PSite.rename" (fun () -> raise Exit)
	    
  end


module ZERO = 
  struct 
    let print x = ""
    let part x = [Void]
    let rename f x = x
  end

module PPhos = 
  struct
    let print x = 
      match x with 
	Boo true ->  "PHOSPHORILATION: \n"
      |	Boo false -> "LINKAGE: \n"
      |	_ -> error_frozen "line 474" "Incompatible type construct" "PPhos.print" (fun () -> raise Exit) 
    let part x = match x with Connected (_,_) -> [] | M _ -> [Boo true] | _ -> [Boo false]
    let rename f x = x
  end

module ALink = 
  struct 
    let print x = ""
    let part x = match x with Connected (_,_) | L _ | AL _ -> [] | _ -> [Void]
    let rename f x = x
  end

module APhos = 
  struct
    let print x = ""
    let part x = match x with Connected (_,_) | M _ -> [] | _ -> [Void]
    let rename f x = x 
  end


module KLink = 
  struct 
    let print x = ""
    let part x = match x with L _ | AL _ -> [Void] | _ -> []
    let rename f x = x
  end
module PUnrel = 
  struct 
    let print x = 
      match x with Q y -> string_of_b y
      |	_ -> error_frozen "line 504" "Incompatible type construct" "PUnrel.print" (fun () -> raise Exit)
	    
    let part x =  
      match x with 
	B _ | H _ | L _-> [Q x]
      | AL (a,_) -> [Q(AL(a,("","")))]
      | M  (a,_) -> [Q(M(a,""))]
      |	Connected (_,_) | Forb _ | Dis _ -> []
	    
    let rename f x = 
      match x with 
	Q(M((a,b,b'),c)) -> Q(M((f a,b,b'),c))
      | Q(H (a,a'))  -> Q(H((f a),a'))
      |	Q(B(a,b,c)) -> Q(B(f a,b,c))
      |	Q(AL((a,b,b'),c)) -> Q(AL((f a,b,b'),c))
      |	Q(L((a,b,b'),(c,d,d'))) -> Q(L((f a,b,b'),(f c,d,d')))
      |	Q(Connected((a,b),(c,d))) -> Q(Connected((f a,b),(f c,d)))
      |	_ -> error_frozen "line 521" "Incompatible type construct" "PUnrel.rename" (fun () -> raise Exit)
  end

module PAuto = 
  struct 
    module SM = Map.Make (struct type t = string let compare=compare end)
    module H = Hash.HInit(SM)
   
    let print x = 
      match x with 
	String_Stringlist (a,b) -> 
      "Agent: "^a^" ; Sites: "^
	(fst (List.fold_left  (fun (s,bool) x -> (if bool then s^"," else s)^x,true)
	   ("",false) b)^"\n")
      |	_ -> error_frozen  "line 535" "Incompatible type construct" "PAuto.print"  (fun () -> raise Exit)
	
    let hash = H.create !Config_complx.hashinit 
    let part x = 
      let find x = 
	try (StringMap.find x !Share.packs) 
	with Not_found -> [] in 
      try (H.find hash x)
      with Not_found -> 
	begin
	  let rep = 
	    match x with
	      H(a,a') -> List.rev_map (fun k -> String_Stringlist (a,k)) (find a')
	    | B(a,a',b) | M((a,a',b),_) | AL((a,a',b),_) -> 
		List.fold_left 
		  (fun l k -> if List.mem b k then (String_Stringlist (a,k))::l else l)
		  []
		  (find a')
	    | L _ -> []
	    | Connected (_,_) | Forb _ | Dis _ -> []
	  in (H.add hash x rep ;rep)
	end
    let rename f x = 
      match x with 
	String_Stringlist (a,b) -> String_Stringlist (f a,b)
      |	_ ->  error_frozen "line 560" "Incompatible type construct" "PAuto.rename"  (fun () -> raise Exit)
	
end

module PLink = ALink
module PSitePhos = Carthesien(PSite)(PPhos)
module PSiteLink = Carthesien(PSite)(ALink)
module PSitePhosLink = Carthesien(PSitePhos)(ALink)
module PPhosLink = Carthesien(PPhos)(ALink)
module NLink = Somme(Carthesien(PUnrel)(KLink))

module QLink      =  NLink(ALink)
module QSitePhos = PSitePhos
module QSiteLink =  NLink(PSiteLink)
module QSitePhosLink = NLink(PSitePhosLink)
module QPhosLink  = NLink(PPhosLink)
 
module RPhos      = Carthesien(APhos)(PPhos)
module RLink      =  Carthesien(APhos)(QLink)
module RSitePhos  =  Carthesien(APhos)(QSitePhos)
module RSiteLink =  Carthesien(APhos)(QSiteLink)
module RSitePhosLink = Carthesien(APhos)(QSitePhosLink)
module RPhosLink  = Carthesien(APhos)(QPhosLink)
module RPSite = Carthesien(APhos)(PSite)

module Auto = Carthesien(PAuto)
