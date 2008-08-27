(*SOL 1 *)
(*decimal with 6 digits *)

(*let string_of_float = Printf.sprintf "%f" *)

(*SOL 2 *)
(*decimal with 6 digit or ingeneer notation if better*)

let string_of_float = Printf.sprintf "%.6g" 

(*SOL 3 *)
(*decimal notation, length enough so that not equl to 0 *)
(*
let l = 
 [
 ("%.7f":(float->string,unit,string) format);
 "%.8f";
 "%.9f"; 
 "%.10f";
 "%.11f";
 "%.12f";
 "%.13f"; 
 "%.14f";
 "%.15f";
 "%.16f";
 "%.17f"; 
 "%.18f";
 "%.19f";
 "%.20f";
 "%.21f"; 
 "%.22f"
]  

let string_of_float x = 
  let sol = Printf.sprintf "%.6f" x in
  if sol <> "0.000000" && x<>0.
  then sol
  else
    let rec aux l  = 
      match l with 
	t::q -> 
	  let sol = Printf.sprintf t x in
	  if sol = Printf.sprintf t 0.
	  then aux q
	  else sol
      |	[] -> Printf.sprintf "%g" x 
    in aux l 
*)
    

  
