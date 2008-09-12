open Data_structures 
open Pb_sig
open Error_handler 


let error i j = 
  unsafe_frozen 
    (Some ("line "^(string_of_int i)))
    None
    None
    None 
    (fun () -> raise Exit)

let init_sep = "\\firstsep\n"
let final_sep = "\\finalsep\n"
let sep = "\\sep\n"

let site_sep = "\\sitesep"
let agent_sep = "\\agentsep"

let mark_unknown = "?"

let translate_string x = 
  let sol = ref [] in 
  let _ = 
    String.iter 
      (fun x -> 
	if (compare 'a' x < 1&& compare x 'z' < 1)
	    or (compare 'A' x < 1 && compare x 'Z' < 1) 
	then 
	  sol:=(String.make 1  x)::(!sol) 
	else
	  match x with 
	    '0' -> sol:="zero"::(!sol)
	  | '1' -> sol:="un"::(!sol)
	  | '2' -> sol:="deux"::(!sol)
	  | '3' -> sol:="trois"::(!sol)
	  | '4' -> sol:="quatre"::(!sol)
	  | '5' -> sol:="cinq"::(!sol) 
	  | '6' -> sol:="six"::(!sol)
	  | '7' -> sol:="sept"::(!sol) 
	  | '8' -> sol:="huit"::(!sol)
	  | '9' -> sol:="neuf"::(!sol)
	  | _ -> ())
      x in
  List.fold_left 
    (fun s t ->t^s) 
    "" (!sol)
        


let store_string,dump_dictionary  = 
  let used = Hashtbl.create 1 in 
  let dictionnary = Hashtbl.create 1 in 
  let f (a,b,c,d) =
    begin 
      try
	Hashtbl.find dictionnary (a,b,c,d)
      with 
	Not_found -> 
	  let y = (translate_string c) in 
	let rec aux y = 
	  if 
	    try 
	      let _ = Hashtbl.find used y in true 
	    with 
	      Not_found -> false
	  then 
	    aux (y^"a") 
	  else
	    y
	in
	let y = aux y in 
	let _ = 
	  Hashtbl.add
	    dictionnary 
	    (a,b,c,d) y
	in 
	let _ = 
	  Hashtbl.add 
	    used y ( )
	in 
	y
    end
  in
  let dump (file:string) =
    if file = ""
    then () 
    else 
      let chan = open_out file in 
      let print_string = Printf.fprintf chan "%s" in 
      let print_newline () = Printf.fprintf chan "\n" in 
      begin
	let liste =  
	  Hashtbl.fold 
	    (fun a b l -> (a,b)::l)
	    dictionnary 
	    [] in
	let sections = 
	  List.fold_left 
	    (fun sects ((a,b,c,d),e) -> 
	      let old = 
		try 
		  StringMap.find a sects 
		with 
		  Not_found -> 
		    [] in 
	    StringMap.add a ((b,c,d,e)::old) sects)
	    StringMap.empty 
	    liste in
	let sections = 
	  StringMap.map 
	    (List.sort 
	       (fun (a,_,_,_) (b,_,_,_) -> compare a b)
	       )
	    sections in 
	let _ = 
	  StringMap.iter 
	    (fun x image -> 
	      print_newline ();
	      print_string "%";
	      print_string x;
	      print_newline ();
	      print_newline ();
	      List.iter 
		(fun (a,b,c,d) -> 
		  print_string "\\newcommand{\\";
		    print_string d;
		  print_string "}{";
		  print_string (fst c);
		  print_string a;
		  print_string (snd c);
		  print_string "}";
		  let n = String.length d+String.length (fst c)+String.length (snd c) + String.length a in
		  let _ = 
		    if n>40
		  then ()
		  else 
		    print_string (String.make  (60-n) ' ') in 
		print_string "%";
		print_string b;
		print_newline ())
	      image)
	  sections in 
      let _ = close_out chan in ()
    end
      in 
    (f,dump)

let string_of_agent_name x = "\\"^(store_string ("Agents",x,"agent "^x,("\\agent{","}")))
let string_of_site_name x = "\\"^(store_string ("Sites",x,"site  "^x,("\\site{","}")))


let dump_rs_latex chan rs bool  (var_of_b,b_of_var,varset_add,varset_empty,fold_vars,build_kleenean_rule_system,print_kleenean_system)  =
  let print_string = 
    match chan with None -> print_string 
    | Some a -> (fun x -> Printf.fprintf a "%s" x) in
  let print_newline  = 
    match chan with None -> print_newline 
    | Some a -> (fun x -> Printf.fprintf a "\n") in 
  let rs = 
    List.rev_map 
      (fun (a,a',b) -> 
	(a,
	 a',
	 List.rev_map 
	   (fun (a,b) -> var_of_b a,b) 
	   b)
	  ) 
      rs in
  let vars = 
    List.fold_left 
      (fun sol (_,_,b) -> 
	List.fold_left 
	  (fun sol (a,_) -> varset_add a sol)
	  sol b)
      varset_empty rs in
  let s = build_kleenean_rule_system rs vars in 
  let sigma =
    let map = 
      (fold_vars
	 (fun v map ->
	   match b_of_var v with H(a',a) -> StringMap.add a' a map
	   | _ -> map)
	 vars
	 StringMap.empty) in
    (fun x -> 
      try 
	StringMap.find x map
      with
      Not_found -> x) in 
 let s = 
    print_kleenean_system 
      (fun x->true) 
      (fun x -> 
	(match x.r_simplx.Rule.flag 
	with None -> x.r_id 
	| Some a -> a))
      (fun x -> 3) 
      (IntSet.empty) 
      s (Some "()") sigma sigma  true  None in 

  let bool = 
    List.fold_left
      (fun bool (a,b)  -> 
	  match a with 
	    [r] -> 
	      let rid = r.Pb_sig.r_id in 
	      let old = name_of_rule r in 
	      let flag = rid in 
	      let kynetic = kynetic_of_rule r in
	      if r.Pb_sig.r_clone  then bool 
	      else 
		  let _ = 
		    if bool then print_string sep 
		    else print_string init_sep in 
		  let _ = List.iter print_string (List.rev b) in
		  let _ = 
		    if kynetic = 1. then ()
		    else 
		      let _ = print_string " @" in
		      let _ = print_string (Printf.sprintf  "%s" (string_of_float kynetic)) in () in 
		  
		  true
	  | _ -> 
	      let _ = error 947 None 
	      in true 
	      ) bool s
  in 
  bool 


let dump file pb  handler = 
  let chan = 
    if file = "" then None
    else Some (open_out file) 
  in
  let _ = 
    try 
      begin 
	match pb.Pb_sig.boolean_encoding with 
	  Some rs -> 
            let system = rs.system in
	    List.fold_left  
	      (fun bool rs -> 
		let inj = rs.rules in 
		List.fold_left
		  (fun bool inj -> 
		    dump_rs_latex  chan  
		      [(inj.labels,rs.control,inj.injective_guard)] bool handler )
		  bool inj)
	      false (List.rev system)
	| _ -> raise Not_found 
      end
    with 
      Not_found -> true 
  in
  let _ = match chan with None -> () | Some a -> close_out a in 
	()
