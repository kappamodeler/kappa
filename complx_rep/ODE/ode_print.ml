open Data_structures
open Arithmetics 
open Ode_print_sig 




let stdprint = 
  {
  print_string = print_string ;
  print_int = print_int ;
  print_newline = print_newline ;
  print_float = print_float ;
  chan = [stdout] 
  } 
    


let all_fields x  = [x.dump;x.matlab;x.mathematica;x.latex;x.data]
      
module CSet = Set.Make (struct type t = out_channel let compare = compare end)


(************************)
(* PRINTING PRIMITIVES  *)
(************************)
    
let string_of_intermediar_var var rule = 
("r"^var^"v"^rule)




let print_intermediar_var print var rule  =
  let _ = 
    match print.matlab with 
      None -> () 
    | Some matlab -> (matlab.print_string "r";
		       matlab.print_string var;
		       matlab.print_string "v";
		       matlab.print_string rule;
		       matlab.print_string "")
  in
  let _ = 
    match print.mathematica with 
      None -> ()
    | Some mathematica -> 
	(mathematica.print_string "r";
	 mathematica.print_string var;
         mathematica.print_string "v";
         mathematica.print_string rule) in 
  ( )
	 
	 
let pprint_float  print f = 
  let g x = 
    match x with 
      None -> () 
    | Some a -> a.print_float f in
  List.iter g (all_fields print)

let pprint_string  print f = 
  let g x = 
    match x with 
      None -> () 
    | Some a -> a.print_string f in
  List.iter g (all_fields print)

let pprint_newline  print  = 
  let g x = 
    match x with 
      None -> () 
    | Some a -> (a.print_newline ();List.iter flush a.chan)
  in
  List.iter g (all_fields print)

let pprint_int  print f = 
  let g x = 
    match x with 
      None -> () 
    | Some a -> a.print_int f in
  List.iter g (all_fields print)


let pprint_bool print f = 
  pprint_string print (if f then "True" else "False")


let pprint_var print k i =
  let _ = 
    match print.matlab 
    with 
      None -> ()
    | Some a -> a.print_string k;a.print_string "(";a.print_string i;a.print_string ")"
  in
  let _ = 
    match print.mathematica 
    with 
      None -> () 
    | Some a -> a.print_string k;a.print_string i
  in
   let _ = 
    match print.dump
    with 
      None -> ()
    | Some a -> a.print_string k;a.print_string "(";a.print_string i;a.print_string ")"
  in
  ()

let pprint_lvar print k i =
  let _ = 
    match print.mathematica 
    with 
      None -> () 
    | Some a -> a.print_string k;a.print_string i
  in
  let _ = 
    match print.dump
    with 
      None -> () 
    | Some a -> a.print_string k;a.print_string i
  in
  
  ()


let pprint_derivate print i = 
  let _ = 
    match print.matlab  with 
      None -> ()
    | Some a -> a.print_string "d" in
  let _ = pprint_lvar print "y" (string_of_int i) in
  let _ = 
    match print.mathematica with 
      None -> ()
    | Some a -> a.print_string "'[t]" in 
  () 

let pprint_ty print = 
 let _ = 
   match print.mathematica with 
     None -> ()
  | Some a -> a.print_string "[t]"
 in 
 let _ = 
   match print.matlab with 
     None -> ()
   | Some a -> a.print_string "(y)"
 in 
 let _ = 
   match print.dump with 
     None -> ()
   | Some a -> a.print_string "(y)"
 in 
 ()

   

let pprint_t print = 
 let _ = 
   match print.mathematica with 
     None -> ()
  | Some a -> a.print_string "[t]"
 in () 


let pprint_zero print = 
  match print.mathematica with 
    None -> ()
  | Some a -> a.print_string "[0]" 

let pprint_initvar print k = 
  let _ = pprint_var print "y" (string_of_int k) in
  let _ = pprint_zero print in ()
    
let pprint_equal print = 
  let _ = 
    match print.mathematica with 
      Some print -> print.print_string "=="
    | None -> ()
  in ()
 
let pprint_assign print = 
  let _ = 
    match print.mathematica with 
      Some print -> print.print_string "="
    | None -> ()
  in pprint_string print "="

let pprint_eq_separator print = 
  pprint_string print ","

let pprint_as_separator print = 
  pprint_string print ";"

let pprint_vart print = 
  let _ =
    match print.mathematica 
    with 
      Some a -> a.print_string "[t_]"
    | None -> ()
  in () 

let pprint_abstract print x = 
  let _ = 
    match print.matlab with 
    | None -> ()
    | Some a -> a.print_string ("@("^x^")")
  in () 

let pprint_assign print =
  let _ = 
    match print.mathematica with 
      None -> ()
    | Some a -> a.print_string ":="
  in let _ = 
    match print.matlab with 
      None -> ()
    | Some a -> a.print_string "="
  in
  () 

let pprint_commandsep print = 
  let _ = 
    match print.mathematica with 
      None -> ()
    | Some a -> a.print_string  ";"
  in 
  let _ = 
    match print.matlab with 
      None -> ()
    | Some a -> a.print_string ";"
  in () 




let pprint_ODE_head print = 
  let _ = pprint_string print "global e; \n" in
  let _ = pprint_string print "e " in 
  let _ = pprint_assign print in 
  let _ = pprint_float print (!Config_complx.ode_ulp) in
  let _ = pprint_commandsep print in
  let _ = pprint_newline print in 
  let _ = pprint_string print "tinit" in 
  let _ = pprint_assign print in 
  let _ = pprint_float print (!Config_complx.ode_init_time) in
  let _ = pprint_commandsep print in
  let _ = pprint_newline print in 
  let _ = pprint_string print "tend" in 
  let _ = pprint_assign print in 
  let _ = pprint_float print (!Config_complx.ode_final_time) in
  let _ = pprint_commandsep print in
  let _ = pprint_newline print in 
  ()

let pprint_ODE_head' print = 
  let _ = 
     match print.mathematica 
     with 
       None -> ()
     | Some a -> 
	 let _ = a.print_string "s=NDSolve[{" in
	 () in 
  let _ = 
    match print.matlab 
    with 
      None -> ()
    | Some a -> 
	let _ = a.print_string "options = odeset('OutputSel',[1]);" in
	let _ = a.print_string "dydt = @(t,y) ["
	in () 
  in () 



let pprint_ODE_middle0 print = 
  let _ = 
    match print.matlab with 
      None -> ()
    | Some a -> a.print_string "\n init = [\n"
  in () 


let pprint_ODE_middle1 print = 
  let _ = 
    match print.mathematica with 
      None -> ()
    | Some a -> a.print_string "},\n{" 
  in ()


let pprint_ODE_middle2 print = 
  let _ = 
    match print.mathematica with 
      None -> () 
    | Some a -> a.print_string "},{t,tinit,tend}]\nPlot[Evaluate[{" in
  let _ = 
    match print.matlab with 
      None -> ()
    | Some a -> a.print_string "];\node45(dydt,[tinit tend],init,options)\n"
  in ()

let pprint_ODE_foot print = 
  match print.mathematica with 
    None -> ()
  | Some a -> a.print_string "}/.s],{t,tinit,tend},PlotRange->All]\n" 

  
let print_comment print s = 
  let _ = 
    match print.mathematica with 
      None -> ()
    | Some a -> a.print_string ("\n(*"^s^"*)") 
  in let _ = 
    match print.matlab with 
      None -> ()
    | Some a -> a.print_string ("\n %"^s^"\n")
  in () 

let pprint_y print =
  let _ = 
    match print.matlab with 
      None -> ()
    | Some a -> a.print_string "(y)"
  in
  let _ = 
    match print.dump with 
      None -> ()
    | Some a -> a.print_string "(y)"
  in ()


let channel_set print set = 
  let f x set = 
    match x with None -> set
    | Some x ->
	List.fold_left
	  (fun set a -> CSet.add a set)
	  set x.chan
  in
  List.fold_left
    (fun set a -> f a set)
    set 
    (all_fields print)


let rec print_expr print bool bool2  x = 
   match x with
     Constf f -> pprint_float print f
   | Letter s -> pprint_string print  s 
   | Const i ->  pprint_int print i 
   | Vari (v,r) -> (print_intermediar_var print r (string_of_int v);(if bool then pprint_ty print else if bool2 then pprint_zero print else ())) 
   | Vark i ->   pprint_var print "k" i 
   | Var i ->  (pprint_var print "y" (string_of_int i);(if bool then pprint_t print else if bool2 then pprint_zero print else ()))
   | Shortcut (s,a) -> (print_intermediar_var print s a;(if bool then pprint_t print else if bool2 then pprint_zero print else ())) 
   | Div (a,b) -> 
      begin
	print_atom print bool  bool2 a;
	pprint_string print  "/";
	print_atom print bool bool2  b
      end
  | Mult (a,b) -> 
      begin
	print_atom print bool bool2  a;
        pprint_string print "*";
	print_atom print bool bool2 b
      end 
  | Plus (a,b) -> 
      begin
	(match a with 
	   Plus _ -> print_expr 
	|  _ -> print_atom ) print bool bool2 a;
	pprint_string print "+";
	(match b with 
	  Plus _ -> print_expr 
	|  _ -> print_atom)  print  bool bool2  b
      end
  | Eps -> pprint_string print "e"
and print_atom print  bool bool2 x = 
  if is_atomic x 
  then print_expr print  bool  bool2 x 
  else 
    begin
      pprint_string print  "(";
      print_expr print bool bool2 x;
      pprint_string print  ")"
    end



 let dump_prod (prod,bool) init obs (init_t,final,step) print_ODE_mathematica print_ODE_matlab output_data   = 
    let print_ODE = print_ODE_mathematica in 
    let _ = pprint_ODE_head' print_ODE in
    
   
    let bool  = 
      Arraymap.fold 
	(fun k b bool ->
	  let _ = if bool then pprint_eq_separator print_ODE in 
	  let _ = pprint_newline print_ODE in 
	  let _ = pprint_derivate print_ODE k in
	  let _ = pprint_equal print_ODE in 
	  let _ = List.fold_left 
	      (fun bool (a,b) ->
		let _ = 
		  if bool then pprint_string print_ODE "+"
		  else 
		    () in
		print_expr print_ODE true false (simplify_expr (Mult(Const a,b)));
			  true) 
	      false b in 
	  true)
	prod  false in

    let _  = 
      Arraymap.fold 
	(fun k b bool -> 
	  let _ = if bool then pprint_eq_separator print_ODE in 
	  let _ = pprint_newline print_ODE in 
	  let _ = pprint_initvar print_ODE k in
	  let _ = if k=0 then (print_string "BUG";Printf.fprintf stdout "BUG\n") in 
	   
	  let _ = pprint_equal print_ODE in
	  let _ = print_expr print_ODE true false  (simplify_expr b) in
	  let _ = 
	    try let _ = Arraymap.find  k prod in ()  
	    with Not_found -> 
	      let _ = if bool then pprint_eq_separator print_ODE in 
	      let _ = pprint_newline print_ODE in 
	      let _ = pprint_derivate print_ODE k in
	      let _ = pprint_equal print_ODE in 
	      let _ = print_expr print_ODE true false  (Const 0) in () in  
	  true)
	init bool  in 
    let _ = pprint_ODE_middle1 print_ODE in
    let _ = 
      List.fold_left
	(fun bool c -> 
	  let _ = if bool then pprint_eq_separator print_ODE in
	  let _ = pprint_newline print_ODE in 
	  let _ = print_expr print_ODE false false   (simplify_expr c) in
	  true)
	false 
	obs in
    let _ = pprint_ODE_middle2 print_ODE in 
    let _ = 
      List.fold_left
	(fun bool c -> 
	  let _ = if bool then pprint_eq_separator print_ODE in 
	  let _ = pprint_newline print_ODE  in 
	  let _ = print_expr print_ODE true false  (simplify_expr c) in
	  true)
	false 
	obs in  
    let _ = pprint_ODE_foot print_ODE in 
    let _ = 
     match step 
     with None -> ()
     | Some a -> 
	 let _ = pprint_string print_ODE "A:={" in 
	 let rec aux (k:float) = 
	   if k>final then ()
           else 
	     let _ = if k>init_t then pprint_string print_ODE "," in 
	     let t = Float_pretty_printing.string_of_float k in 
	     let _ = pprint_string print_ODE "{" in
	     let _ = pprint_string print_ODE t in 
	     let _ = 
	       List.iter 
		 (fun c -> 
		   let _ = pprint_string print_ODE "," in 
		   let _ = print_expr print_ODE false false   (simplify_expr c) in
		   let _ = pprint_string print_ODE "[" in 
		   let _ = pprint_string print_ODE t in 
		   let _ = pprint_string print_ODE "]" in 
		   ())
		 obs in
	     let _ = pprint_string print_ODE "}" in
	     aux (k+.a) in 
	 let _ = aux (min final init_t) in 
	 let _ = pprint_string print_ODE "}/.s;\n" in 
	 let _ = pprint_string print_ODE "Export[\"" in 
	 let _ = pprint_string print_ODE output_data in 
	 let _ = pprint_string print_ODE "\",A,\"Table\"]\n" in 
	 ( )
    in 
    let print_ODE = print_ODE_matlab in 
    let _ = pprint_ODE_middle0 print_ODE in 
    
    let _  = 
      Arraymap.fold 
	(fun k b bool -> 
	  let _ = if bool then pprint_eq_separator print_ODE in 
	  let _ = pprint_newline print_ODE in 
	  let _ = if k=0 then (print_string "BUG";Printf.fprintf stdout "BUG\n") in 
	   
	  let _ = pprint_equal print_ODE in
	  let _ = print_expr print_ODE true true  (simplify_expr b) in
	  let _ = 
	    try let _ = Arraymap.find k prod in ()  
	    with Not_found -> 
	      let _ = if bool then pprint_eq_separator print_ODE in 
	      let _ = pprint_newline print_ODE in 
	      let _ = pprint_derivate print_ODE k in
	      let _ = pprint_equal print_ODE in 
	      let _ = print_expr print_ODE true true  (Const 0) in () in  
	  true)
	init false  in
    let _ = pprint_string print_ODE "]\n" in 
    let _ = pprint_ODE_head' print_ODE in 
    let _   = 
      Arraymap.fold
	(fun k b bool ->
	  let _ = if bool then pprint_eq_separator print_ODE in 
	  let _ = pprint_newline print_ODE in 
	  let _ = pprint_equal print_ODE in 
	  let _ = List.fold_left 
	      (fun bool (a,b) ->
		let _ = 
		  if bool then pprint_string print_ODE "+"
		  else 
		    () in
		print_expr print_ODE true true  (simplify_expr (Mult(Const a,b)));
			  true) 
	      false b in 
	  true)
	prod  false in

    let _ = pprint_ODE_middle1 print_ODE in
    let _ = pprint_ODE_middle2 print_ODE in 
    let _ = pprint_ODE_foot print_ODE in 
    
    ()

 let pprint_obs print print_sb n expr pb = 
    let _ = 
      match print.dump with 
	None -> ()
      |	Some print -> 
	  print.print_string  ("Y("^(string_of_int n)^"):=");
	  print_sb expr pb  print.chan;
	  print.print_newline ()
    in 
     let _ = 
      match print.txt with 
	None -> ()
      |	Some print -> 
	  print.print_string  ("Y("^(string_of_int n)^"):=");
	  print_sb expr pb print.chan;
	  print.print_newline () 
    in 
     let _ = 
       match print.kappa  with 
	 None -> ()
       |	Some print -> 
	   print.print_string "%obs:";
	   print_sb expr pb print.chan;
	   print.print_newline () 
     in 
     let _ = 
       match print.latex  with 
	 None -> ()
       | Some print -> 
	   print_sb expr pb print.chan;
     in 
     let _ = 
       match print.data with 
	 None -> ()
       | Some print -> 
	   begin 
	     print.print_string "[";
	     print_sb expr pb print.chan;
	     print.print_string "]";
	     print.print_string " " 
	   end
     in 
     ( ) 

