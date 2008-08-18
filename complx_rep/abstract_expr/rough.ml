(* 11/05/2006 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Abstract boolean expressions: Rough Implementation *)
(* rough.ml *)

open Tools
open Config_complx
open Expr
open Kleenean_expr
open Var
open Pb_sig
open Data_structures
open Abstract_expr_sig
  
  
module RoughBool = 
  (functor (KExpr:Kleenean_Expr)
   -> 
     
     (struct 
       module K = KExpr
       type abstract_expr = K.E.expr 
		   
	     
       type reachable_states = K.E.expr

       let reachable_states_of_abstract_expr a = a 
       let var_of_expr  = K.E.vars_of_expr
       let reachable_states_of_expr a = a
       let reachable_states_of_valuation_list = K.E.expr_of_valuation_list
	   
       let is_bot x = false
       let print_bdd x = ()
       let empty = K.E.expr_not K.E.expr_true
  
       let union a b = K.E.expr_or a b
       let rename a f = K.E.expr_rename a f 
       let increasing_renaming a f = rename a (fun x -> K.E.V.var_of_b (upgrade_renaming f (K.E.V.b_of_var x)))
       let conj = K.E.expr_and
       let r_neg = K.E.expr_not
       let r_union = union
       let r_conj = conj
       let atom_pos = K.E.expr_atom 
       let atom_neg x = K.E.expr_not (atom_pos x)
       let r_atom = atom_pos
       let imply = K.E.expr_imply
       let r_imply = imply
       let ae_true = K.E.expr_true
       let ae_false = K.E.expr_not ae_true
       let is_ae_true s = s=K.E.expr_true
       let is_ae_false s = s=K.E.expr_not K.E.expr_true
       let r_ae_true = ae_true 
       let r_is_bot x = false 
       let abstract_expr_of_expr i = i
       let expr_of_abstract_expr i = i

       let switch expr set = 
	 K.E.V.fold_vars 
	   (fun s expr -> K.E.dual  expr s)
	   set expr 
	 
       let forget expr f = 
	 K.E.V.fold_vars 
	   (fun v expr -> 
	     if f v then K.E.expr_or expr (K.E.dual expr v)
		 else expr)
	   (K.E.vars_of_expr expr) 
	   expr

       let project expr f = forget expr (fun x -> not (f x))
       let test expr v b = 
	 K.E.expr_and expr ((if b then fun x -> x else K.E.expr_not) (K.E.expr_atom  v))

       let set expr f = 
	 let expr' = forget expr (fun x -> try (K.E.V.varmap_find x f;true) with Not_found -> false) in
	 K.E.V.varmap_fold (fun v b sol -> test sol  v b) f expr'
      
       let weak_set expr f  = 
	 K.E.V.varmap_fold
	   (fun v b sol -> 
	     union sol (set expr (K.E.V.varmap_add v b (K.E.V.varmap_empty))))
	   f expr 

     
       let rho_next vset valuation = 
	 let rep = ref valuation in 
	 try (K.E.V.varset_iter (fun x -> 
	   if K.E.V.varset_mem x (!rep) 
	       		 then (rep:=K.E.V.varset_remove x (!rep))
	   else ((rep:=K.E.V.varset_add x (!rep));(raise Exit)))
		vset;None) with Exit -> (Some (!rep))
		    
       let rho_init x = K.E.V.varset_empty
	   
       let rec power n  = 
	 if n=0 then 1 else
	 let rep = power (n/2) in 
	 if n=(n/2)*2 then rep*rep
	 else 2*rep*rep

       let tautology expr = 
	 let vars = K.E.vars_of_expr expr in 
	 let card = K.E.V.fold_vars (fun a n -> n+1) vars 0 in 
	 print_string "N Var:";
	 print_int card;
	 print_newline ();
	 print_string "VALUATION:";
	 print_int (power card);
	 print_newline ();
	 let rhoinit = rho_init vars in 
	 let n = ref 0 in 
	 let rec aux rho = 
	   (if (!n)/1000*1000=(!n) then 
	   (print_int (!n);
	   print_newline ()));
	   n:=(!n)+1;
	   let b = K.E.eval expr (fun x->K.E.V.varset_mem x rho)  in 
	   if not 
	       b 
	   then false 
	   else (match rho_next vars rho with None -> true | Some(rho) -> aux rho)
	 in aux rhoinit
	   
       let fnd_of_bdd expr = 
	  let vars = K.E.vars_of_expr expr in 
	  let rhoinit = rho_init vars in 
	  let sol = ref [] in 
	  let rec aux rho = 
	    let b = K.E.eval expr (fun x->K.E.V.varset_mem x rho)  in 
	    (if not b then () else sol:=(K.E.V.fold_vars
					   (fun v l -> 
					     if K.E.V.varset_mem v rho
					     then (v,true)::l
					     else (v,false)::l)
					   vars
					   [])::(!sol));
	    (match rho_next vars rho with 
	      None -> (!sol) 
	    | Some(rho) -> aux rho)
	  in aux rhoinit

       let is_included a b = tautology (K.E.expr_imply a b)

       let guard = K.expr_of_kleenean_rule_system
	   
       let may_be_true expr l = 
	 K.E.V.fold_vars
	   (fun v l -> 
	     let expr' = set expr (K.E.V.varmap_add v false (K.E.V.varmap_empty))
		 in 
		 if is_included expr expr' && is_included expr' expr
		 then 
		   l
		 else 
		   K.E.V.varset_add v l)
	   l K.E.V.varset_empty
    

       let abstract_system mode  s access vars dep messages = 
	   let g = guard s in 
	   K.E.V.fold_vars 
	     (fun v s -> 
	       let composante = K.E.V.close dep v in 
	       K.ksmap
		     (fun r -> 
		       let abs = K.abstract r v in 
		       if abs = r then r 
		       else 
			 let abs = K.E.V.fold_vars 
			     (fun v r -> K.abstract r v) 
			     composante r in 
			 let e = 
			   imply 
			     (K.expr_of_kleenean_valuation abs)
			    (imply access (match mode with Isolated -> 
                                   K.expr_of_kleenean_valuation r | _ -> g)) in 
			 if tautology e 
			 then abs
			 else r)
		 s)
	     vars s,messages 
		
       let print s f g h p log = K.E.print_expr (expr_of_abstract_expr s);[] 
       let print_reachable_states access  specie_of_id pb print_any   log = 
	 let a = print access specie_of_id  pb (fun _ -> true)  print_any log in
	 List.map snd a 
       let print_reachable_states2 a b c d e = print_reachable_states a b c d  
       let f x = true 
       let all_false l = 
	 list_fold (fun b sol ->
	   K.E.expr_and (K.E.expr_not (K.E.expr_atom (K.E.V.var_of_b b))) sol) 
	   l K.E.expr_true

       let list_conj l = 
	 list_fold (fun (b,bool) sol ->
	   K.E.expr_and ((if bool then (fun x -> x) else K.E.expr_not) (K.E.expr_atom (K.E.V.var_of_b b))) sol)
	   l K.E.expr_true

       let exactly_once l = K.E.expr_exactly_once_in (List.rev_map K.E.V.var_of_b l)
       let atmost_once l = K.E.expr_mutual_exclusion (List.rev_map K.E.V.var_of_b l)

       let r_all_false = all_false
       let r_list_conj = list_conj
       let r_exactly_once = exactly_once 
       let r_atmost_once = atmost_once 

       let r_all_false_s = all_false
       let r_list_conj_s = list_conj
       let r_exactly_once_s = exactly_once 
       let r_atmost_once_s = atmost_once 

       let summarize_abstract_expr  expr = ["",expr]

       let dump_concretization rep pb parsed_pb parsed_case = None
	 
 
       let export_ae a k parsed_pb h = []

       let compute_abstract_lens a = 
	 if StringMap.is_empty a then 
	   ae_true
	 else
	   StringMap.fold 
	       (fun _ a  -> 
			 let _ = trace_print "CONJ" in 
                   r_conj (reachable_states_of_abstract_expr 
				 (project a 
				    (fun x -> 
				       match K.E.V.b_of_var x with  H _ | B _  | M _ -> true | _ -> false))) (*expr*))
		      a r_ae_true 
	 
       let compute_subviews a = KeyMap.add Void a KeyMap.empty
       let restore_subviews a = 
	 try 
	   KeyMap.find Void a 
	 with Not_found -> unsafe_frozen (Some "ROUGH") (Some "Rough.ml") (Some "restore_subviews") (Some "line:237")  (fun () -> raise Exit)
	   end:AbstractExprBool))
