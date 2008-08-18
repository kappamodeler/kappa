let key_version = ""
let debug = false

let trace_string = if debug then print_string else (fun x -> ())
let trace_int = if debug then print_int else (fun x -> ())
let trace_newline = if debug then print_newline  else (fun x -> ())

let int_of_char x = 
  match x with 
      '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7 
    | '8' -> 8
    | '9' -> 9
    | 'A' -> 10
    | 'B' -> 11
    | 'C' -> 12
    | 'D' -> 13
    | 'E' -> 14
    | 'F' -> 15
    | _ -> failwith "wrong hex char" 

let convert_key s =
  if not (String.sub s 0 2 = "0x" && String.length s = 18)
  then failwith "Bad format for the key"
  else
    (let sol = ref [] in 
       String.iter  (fun a -> sol:=(int_of_char a)::(!sol)) 
	 (String.sub s 2 16);
       List.rev !sol)


let keyref = "0x07C8190D12FE3873"

let rec complete l = 
  match l with 
      [a;b;c;d] -> l
    | a::b::c::d::e::_ -> failwith "bad format in hex -> bin conversion" 
    | _ -> complete (0::l)

    
let convert_hex_in_bin a = 
  let rec aux c rep = 
    if c=0 then rep 
    else aux (c/2) ((if c mod 2 = 0 then 0 else 1)::rep)
  in 
    complete(aux a [])

let xor a b = if a=b then 0 else 1
let xorlist a b = List.map2 xor a b 

let int_of_binlist l = 
  let rec aux k l sol = 
    match l with [] -> sol
      | 0::q -> aux (k*2) q sol
      | 1::q -> aux (k*2) q (k+sol)
      | _ -> failwith "bad format in int_of_binlist" 
  in aux 1 (List.rev l) 0

let rec binlist_in_hex_list l =
  match l with a::b::c::d::l -> (int_of_binlist [a;b;c;d])::(binlist_in_hex_list l)
    |[] -> []
    | _ -> failwith "wrong format in bin to hex conversion"

  

let good_key  key =
  let key = "0x"^key in 
(*  let _ = trace_string "Command line Key : " in*)
  let _ = trace_string key in 
  let _ = trace_newline () in 
(*  let _ = trace_string "Hidden Key       : " in*)
  let _ = trace_string keyref  in 
  let _ = trace_newline () in
  let convert x = List.flatten (List.map convert_hex_in_bin (convert_key x)) in 
(*  let _ = trace_string "Hidden Key (bin)       : " in*)
  let _ = List.iter trace_int (convert keyref) in
  let _ = trace_newline () in
(*  let _ = trace_string "Command line key (bin) : " in *)
  let _ = List.iter trace_int (convert key) in
  let _ = trace_newline () in 
  let a = Unix.localtime (Unix.time ()) in 
  let year =  (a.Unix.tm_year + 1900) in 
  let minute = (a.Unix.tm_min) in
  let sec = (a.Unix.tm_sec) in
  let month = (a.Unix.tm_mon)+1 in 
  let day = (a.Unix.tm_mday) in
  let hour = (a.Unix.tm_hour) in 
  let _ = trace_int year in
  let _ = trace_string "/" in 
  let _ = trace_int month in 
  let _ = trace_string "/" in
  let _ = trace_int day in
  let _ = trace_string "/" in
  let _ = trace_int hour in 
  let _ = trace_string "/" in
  let _ = trace_int minute in
  let _ = trace_string "/" in
  let _ = trace_int sec in 
  let _ = trace_newline () in 
  let computed = xorlist (convert keyref) (convert key) in 
  let _ = List.iter trace_int computed in
  let _ = trace_newline () in 
  let launchingtime  = binlist_in_hex_list 
    (xorlist 
       (convert keyref)
       (convert key))   in
  let _ = trace_string "Computed time :" in 
  let _ = List.iter trace_int launchingtime in
  let _ = trace_newline () in
    match launchingtime with 
	[z1;z2;y1;y2;y3;y4;m1;m2;d1;d2;h1;h2;mn1;mn2;s1;s2] -> 
	  let _ = trace_string "Computed Date : " in
	  let lyear = y4+16*y3+16*16*y2+16*16*16*y1 in
	  let lmonth = m2+16*m1+1 in
	  let lday = d2 + 16*d1 in
	  let lhour = h2 + 16*h1 in
	  let lmin = mn2 + 16*mn1 in
	  let lsec = s2 + 16*s1 in
	  let _ = trace_int lyear in
	  let _ = trace_string "/" in
	  let _ = trace_int lmonth in
	  let _ = trace_string "/" in
	  let _ = trace_int lday in
	  let _ = trace_string "|" in
	  let _ = trace_int lhour in
	  let _ = trace_string ":" in
	  let _ = trace_int lmin in 
	  let _ = trace_string ";" in
	  let _ = trace_int lsec in
	  let _ = trace_newline () in 
	    if z1=0 && z2 = 0 &&  
	      year = lyear &&
	      month = lmonth &&
	      (let d = 
		 (lsec + 60*lmin + 3600*lhour + 86400*lday) 
		 - (sec + 60*minute + 3600*hour + 86400 * day) in 
		 d >=(-10) && d<=10)
	    then true 
	    else false

      | _ -> failwith "bad key format"
