open Data_structures 
open Ode_print_sig 

let debug = false
let count_embedding = false
    (* TRUE -> count embedding 
       FALSE -> count embedding / automaorphism *)

type expr = 
    Letter of string 
	
  | Vark of string
  | Vari of (int*string)
  | Vardi of (int*string*int)
  | Var of int 
  | Mult of (expr*expr)
  | Div of (expr*expr)
  | Eps 
  | Plus of (expr*expr)
  | Const of int 
  | Constf of float 
  | Shortcut of (string*string)

let equal_un expr = 
  match expr with 
    Const 1 | Constf 1. -> true 
  | _ -> false

let equal_zero expr = 
  match expr with 
    Const 0 | Constf 0. -> true 
  | _ -> false 

let rec simplify_expr (expr:expr) = 
  match expr with 
      Plus (a,x) when equal_zero a -> simplify_expr x
  | Plus(x,a) when equal_zero a -> simplify_expr x
  | Mult(a,x) when equal_un a -> simplify_expr x
  | Mult(x,a) when equal_un a -> simplify_expr x 
  | Div(x,a) when equal_un a -> simplify_expr x 
  | Plus (x,y) -> Plus(simplify_expr x,simplify_expr y)
  | Mult (x,y) -> 
      let x = simplify_expr x in
      let y = simplify_expr y in
	if equal_zero x or equal_zero y then Const 0 
	else if x=Const 1 then y 
	else if y=Const 1 then x 
	else Mult(x,y)
  | Div  (x,y) -> 
      let x = simplify_expr x in
      let y = simplify_expr y in
      if x=y 
      then Const 1
      else  Div(x,y)
  | Constf a when float_of_int(int_of_float a)=a -> Const (int_of_float a)
  | _ -> expr

module KeyMap = Map2.Make (struct type t = expr*expr*expr*expr let compare = compare end)

module HExprMap = Map2.Make (struct type t = expr let compare = compare end)

      
let rec simplify2 expr = 
  let rec simplify_term_list expr (map,const) = 
    match expr with 
      Plus(a,b) -> 
	simplify_term_list a (simplify_term_list b (map,const)) 
    | Constf f -> (map,const+.f)
    | Const i -> (map,const+.(float_of_int i))
    | x -> 
	let map2,const2 = 
	  simplify_factor_list x (HExprMap.empty,1.)
	in
	let exprx' = 
	  recombine_factor_list map2 in
	let old = 
	  try 
	    HExprMap.find exprx' map 
	  with 
	    Not_found -> 0.
	in
	HExprMap.add exprx' (old+.const2) map,const 
  and
      recombine_term_list map = 
    HExprMap.fold
      (fun a b expr -> 
	 if b = 1. then (Plus(a,expr))
	else if b = 0. or equal_zero a then expr
	else Plus(Mult(Constf b,a),expr))
      map
      (Constf 0.)
  and
      simplify_factor_list expr ((map:int HExprMap.t),const) = 
    match expr with 
      Mult(a,b) -> 
	simplify_factor_list a (simplify_factor_list b (map,const)) 
    | Constf f -> (map,const*.f)
    | Const i -> (map,const*.(float_of_int i))
    | Plus _  ->
	let x' = simplify2 expr in
       	let old = 
	  try 
	    HExprMap.find x' map 
	  with 
	    Not_found -> 0
	in
	HExprMap.add x' (old+1) map,const  
    | Div(a,b) 
      -> let x' = Div(simplify2 a,simplify2 b) in 
         let old = 
	  try 
	    HExprMap.find x' map 
	  with 
	    Not_found -> 0
	in
	HExprMap.add x' (old+1) map,const  
    | _ -> 
	let x' = expr in 
         let old = 
	  try 
	    HExprMap.find x' map 
	  with 
	    Not_found -> 0
	in
	HExprMap.add x' (old+1) map,const  
	
  and
      recombine_factor_list map = 
    HExprMap.fold
      (fun a b expr -> 
	 if expr = Const 0 then Const 0 
	 else if b = 0 then expr 
	 else if a = Const 0 then Const 0
	 else if b = 1 then Mult(a,expr)
	 else 
	   let rec aux k sol = 
	     if k=0 then sol
	     else aux (k-1) (Mult(a,sol))
	   in aux b (Const 1)
      )
      map
      (Const 1)
  in 
  simplify_expr (
  let map,cst = simplify_term_list expr (HExprMap.empty,0.) in
  Plus(Constf cst,recombine_term_list map)) 

let simplify_expr expr = simplify2 (simplify_expr  expr) 
let is_atomic expr = 
  match expr with 
   Letter _ | Eps | Var _ | Vark _ -> true
   | Constf f -> f>= 0.
   | Const a -> a>=0 
 | _ -> false 



type ('subclass,'subspecies) expr_handler = 
  {hash_subspecies:'subspecies -> (int*int);
   get_denum_handling_compatibility:(string*string*string*string)-> 'subspecies list;
   get_bond:'subclass  -> (string*string*string*string) option;
   get_fragment_extension: 'subclass -> 'subspecies list }

let expr_of_var expr_handler d = 
  match expr_handler.hash_subspecies d 
  with 
    (i,1) -> Var i
  | (i,n) -> 
      if count_embedding 
      then Var i 
      else
	Mult(Const n,Var i)
     
let expr_of_denum expr_handler d = 
  let _ = 
    if debug
    then
      print_string "Expr_of_denum\n"
  in
  List.fold_left 
    (fun expr d -> 
      Plus(expr,expr_of_var expr_handler d))
    (Eps) 
    d
    
let expr_of_atom expr_handler (a:(string*string*string*string) option) b = 
  let _ = 
    if debug
    then
      print_string "Expr_of_atom\n"
  in
  (match a 
  with 
    None -> expr_of_var expr_handler b
  | Some a -> 
      let d = expr_handler.get_denum_handling_compatibility  a in
      Div (Plus(Eps,expr_of_var expr_handler b),
	   expr_of_denum expr_handler d))
    
let expr_of_subcomponent expr_handler subcla  =
  let _ = 
    if debug
    then
      print_string "Expr_of_subcomponent\n"
  in
  let a = expr_handler.get_bond subcla  in
  let b = expr_handler.get_fragment_extension subcla   in 
  List.fold_left 
    (fun expr b -> 
      Plus(expr,
	   expr_of_atom expr_handler a b))
    (Const 0) b 
    
let expr_of_case expr_handler z = 
  let _ = 
    if debug
    then
      print_string "Expr_of_case\n"
  in
  List.fold_left 
    (fun expr subcla -> 
      Mult(expr,expr_of_subcomponent expr_handler subcla))
      (Const 1) z 
    
let expr_of_classe expr_handler rep = 
  List.fold_left 
    (fun cost z ->
      Plus(cost,
	   expr_of_case expr_handler z))
    (Const 0) rep 
    

let var_of_expr expr = 
  let rec vide expr sol = 
    match expr with 
	Letter _ | Vark _ | Vari _ | Eps | Const _ | Constf _ | Shortcut _ -> sol 
      | Var i -> IntMap.add i (Const 0) sol  
      | Plus (a,b) | Mult (a,b) -> vide a (vide b sol)
  in 
    vide expr IntMap.empty 
	
let diff expr v = 
  let rec aux expr = 
    match expr with 
	Var i when v = i -> Const 1 
      | Letter _ | Vark _ | Vari _ | Eps | Const _ | Constf _ | Shortcut _ | Var _ -> Const 0 
      | Mult(a,b) -> Plus(Mult(a,aux b),Mult(aux a,b))
      | Plus(a,b) -> Plus(aux a,aux b)
  in
    simplify_expr (aux expr) 

let grad expr = 
  let vars = var_of_expr expr in 
    IntMap.mapi 
      (fun v i -> diff expr v)
      vars 
