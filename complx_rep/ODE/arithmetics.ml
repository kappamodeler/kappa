
type expr = 
    Letter of string 
	
  | Vark of string
  | Vari of (int*string)
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
  | Plus (x,y) -> 
      Plus(simplify_expr x,simplify_expr y)
  | Mult (x,y) -> 
      let x = simplify_expr x in
      let y = simplify_expr y in
      if x=Const 1 then y 
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

let is_atomic expr = 
  match expr with 
   Letter _ | Eps | Var _ | Vark _ -> true
   | Constf f -> f>= 0.
   | Const a -> a>=0 
 | _ -> false 


type ('subclass,'subspecies) expr_handler = 
  {hash_subspecies:'subspecies -> int;
   get_denum_handling_compatibility:(string*string*string*string)-> 'subspecies list;
   get_bond:'subclass  -> (string*string*string*string) option;
   get_fragment_extension: 'subclass -> 'subspecies list }

let expr_of_denum expr_handler d = 
  List.fold_left 
    (fun expr d -> 
      Plus(expr,(Var (expr_handler.hash_subspecies d))))
    (Eps) 
    d
    
let expr_of_atom expr_handler (a:(string*string*string*string) option) b = 
  (match a 
  with 
    None -> Var (expr_handler.hash_subspecies b)
  | Some a -> 
      let d = expr_handler.get_denum_handling_compatibility  a in
      Div (Plus(Eps,Var (expr_handler.hash_subspecies b)), 
	   expr_of_denum expr_handler d)) 
    
let expr_of_subcomponent expr_handler subcla  =
  let a = expr_handler.get_bond subcla  in
  let b = expr_handler.get_fragment_extension subcla   in 
  List.fold_left 
    (fun expr b -> 
      Plus(expr,
	   expr_of_atom expr_handler a b))
    (Const 0) b 
    
let expr_of_case expr_handler z = 
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
    
