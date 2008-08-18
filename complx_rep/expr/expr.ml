(* 2/10/2006 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Boolean expressions *)
(* expr.ml *)
  
open Tools
open Var 
  
module type ExprBool = 
  sig 
    module V:Var
    type expr  = Var of V.var
      | Not of expr 
      | And of expr*expr
      | Or of expr*expr
      | Equiv of expr*expr
      | Imply of expr*expr 
      |	True | False
	  
    type boolean_valuation
	  
    val print_expr: expr -> unit
    val vars_of_expr: expr -> V.varset	
    val eval: expr -> (V.var->bool) -> bool
    val expr_true:expr
    val expr_atom: V.var -> expr
    val expr_and: expr -> expr -> expr
    val expr_not: expr -> expr
    val expr_or: expr -> expr -> expr
    val expr_imply: expr -> expr -> expr
    val expr_equiv: expr -> expr -> expr
    val expr_rename: expr -> (V.var -> V.var) -> expr
    val dual: expr -> V.var -> expr
    val expr_equ_class: V.var list ->  expr 
    val expr_mutual_exclusion: V.var list -> expr
    val expr_exactly_once_in: V.var list -> expr 
    val expr_of_valuation_list: boolean_valuation list -> V.varset -> expr
	
    val boolean_valuation_of_list: V.var list -> boolean_valuation
    val boolean_valuation_of_varset: V.varset -> boolean_valuation
    val varset_of_boolean_valuation: boolean_valuation -> V.varset
	
	
	
  end
      

      
module Expr  = 
  (functor (Var:Var) -> 
    (
    struct 
      module V = Var
      type expr = Var of V.var
	| Not of expr 
	| And of expr*expr
	| Or of expr*expr
	| Equiv of expr*expr
	| Imply of expr*expr 
	|	True | False
	    
      type boolean_valuation = V.varset
	    
      let expr_not e1 = Not(e1)
      let expr_and e1 e2 = And(e1,e2)
      let expr_or e1 e2 = Or(e1,e2)
      let expr_equiv e1 e2 = Equiv(e1,e2)
      let expr_imply e1 e2 = Imply(e1,e2)
	  
      let expr_true = True
      let expr_false = False
	  
      let expr_atom x = Var x
	  
      let expr_equ_class l = 
	match l with [] | _::[] -> expr_true
      |	t::q -> 
	  let expr,a = 
	    list_fold 
	      (fun v (expr,old) -> 
		expr_and expr (expr_imply (expr_atom old) (expr_atom v)),v)
	      q (expr_true,t)
	  in expr_and expr (expr_imply (expr_atom a) (expr_atom t)) 
	    
	    
      let expr_mutual_exclusion l = 
	list_fold 
	  (fun a expr ->
	    list_fold 
	      (fun b expr -> 
		if a=b then expr
		else expr_and expr 
		    (expr_imply (expr_atom a) 
		       (expr_not (expr_atom b))))
	      l expr)
	  l expr_true
	  
	  
      let expr_exactly_once_in l  = 
	expr_and (expr_mutual_exclusion l)
	  (match l with [] -> expr_false
	  | t::q -> 
	      list_fold 
		(fun a expr -> expr_or (expr_atom a) expr)
		q (expr_atom t))
	  
	  
	  
	  
      let expr_of_valuation rho var_set = 
	V.fold_vars 
	  (fun x sol -> 
	    if V.varset_mem x rho 
	    then expr_and (expr_atom x) sol 
	    else expr_and (expr_not (expr_atom x)) sol )
	  var_set expr_true
	  
	  
      let expr_of_valuation_list l var_set = 
	list_fold 
	  (fun v sol -> expr_or (expr_of_valuation v var_set) sol)
	  l (expr_not expr_true)
	  
	  

	  
	  
      let rec eval expr rho = 
	match expr with True -> true | False -> false
      | And(e1,e2) ->  eval e1 rho && eval e2 rho 
      | Or(e1,e2)  ->  eval e1 rho || eval e2 rho
      | Not(e1)    -> not(eval e1 rho)
      | Imply(e1,e2) -> if (eval e1 rho) then (eval e2 rho) else true
      | Equiv(e1,e2) -> (eval e1 rho)=(eval e2 rho)
      |  Var x -> rho x
	    
      let rec vars_of_expr expr  = 
	let rec aux expr sol = 
	  match expr with True | False -> sol 
	| And(e1,e2) | Or(e1,e2) | Imply(e1,e2) | Equiv(e1,e2) -> aux e1 (aux e2 sol)
	| Not(e1)    -> aux e1 sol 
	| Var x -> Var.varset_add x sol 
	in aux expr Var.varset_empty

      let rec dual expr v = 
	match expr with True -> expr | False -> expr
      | And(e1,e2) ->  And(dual e1 v,dual e2 v)
      | Or(e1,e2)  ->  Or(dual e1 v,dual e2 v)
      | Not(e1)    -> Not(dual e1 v)
      | Imply(e1,e2) -> Imply(dual e1 v,dual e2 v)
      | Equiv(e1,e2) -> Equiv(dual e1 v,dual e2 v)
      | Var x when x=v -> Not(expr)
      |	Var x -> expr
	
      let print_exprb x = 
	let rec aux expr = 
	  match expr with 
	    True -> print_string "T" 
	  | False -> print_string "F"
	  | And(e1,e2) -> 
	      (print_string "(";
	       aux e1;
	       print_string " & ";
	       aux e2;
	       print_string ")")
	  | Or(e1,e2) -> 
	      (print_string "(";
	       aux e1;
	       print_string " || ";
	       aux e2;
	       print_string ")")
	  | Equiv(e1,e2) -> 
	      (print_string "(";
	       aux  e1;
	       print_string " <==> ";
	       aux e2;
	       print_string ")")
	  | Imply(e1,e2) -> 
	      (print_string "(";
	       aux e1;
	       print_string " ==> ";
	       aux e2;
	       print_string ")")
	  | Var x -> (Var.print_var x)
	  | Not e1 -> (
	      print_string "(Not ";
	      aux e1;
	      print_string ")")
	in aux x
	  
      let rec expr_rename x f = 
	match x with 
	  Var x -> Var (f x)
	| True  
	| False -> x
	| And(e1,e2) -> And(expr_rename e1 f,expr_rename e2 f)
	| Or(e1,e2) -> Or(expr_rename e1 f,expr_rename e2 f)
	| Equiv(e1,e2) -> Equiv(expr_rename e1 f,expr_rename e2 f)
	| Imply(e1,e2) -> Imply(expr_rename e1 f,expr_rename e2 f)
	| Not e1 -> Not(expr_rename e1 f)
	      
      let rec print_expr expr = 
	match expr with 
	  And(e1,e2) -> print_expr e1;print_string "\n& ";print_expr e2
	| _ -> print_exprb expr 
	      
      let print_expr expr = (print_expr expr;print_newline ())
	  
      let boolean_valuation_of_list l = list_fold Var.varset_add l Var.varset_empty
	  
      let boolean_valuation_of_varset s = s
      let varset_of_boolean_valuation s = s
	  
	  end:ExprBool))
