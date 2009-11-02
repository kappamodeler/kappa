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
    

(*SOL4 as many bits (<57) so that the conversion loses no information, otherwise 57 bits *)
let l = 
  [
    ("%.0g":(float->string,unit,string) format);
    "%.1g";
    "%.2g";
    "%.3g";
    "%.4g";
    "%.5g";
    "%.6g";
    "%.7g";
    "%.8g";
    "%.9g"; 
    "%.10g";
    "%.11g";
    "%.12g";
    "%.13g"; 
    "%.14g";
    "%.15g";
    "%.16g";
    "%.17g"; 
    "%.18g";
    "%.19g";
    "%.20g";
    "%.21g"; 
    "%.22g";
    "%.23g";
    "%.24g"; 
    "%.25g";
    "%.26g";
    "%.27g";
    "%.28g"; 
    "%.29g";
    "%.30g";
    "%.31g";
    "%.32g"; 
    "%.33g";
    "%.34g";
    "%.35g";
    "%.36g"; 
    "%.37g";
    "%.38g";
    "%.39g";
    "%.40g";
    "%.41g"; 
    "%.42g";
    "%.43g";
    "%.44g"; 
    "%.45g";
    "%.46g";
    "%.47g";
    "%.48g"; 
    "%.49g";
    "%.50g";
    "%.51g";
    "%.52g"; 
    "%.53g";
    "%.54g";
    "%.55g";
    "%.56g"; 
    "%.57g";
]

let exact_string_of_float (x:float) = 
  let s = string_of_float x in 
    if float_of_string s -. x = 0. then s
    else 
      let rec scan l = 
        match l 
        with 
            [] -> Printf.sprintf "%.57g" x
          |t::q -> 
             let s = Printf.sprintf t x in 
               if float_of_string s -. x = 0.  
               then s
               else scan q 
      in  
        scan l 
          
