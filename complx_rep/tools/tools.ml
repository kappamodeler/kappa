(* 13/04/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* ToolKit *)
(* tools.ml *)

open Config_complx
open Pb_sig 
open Data_structures
open Memory_usage
open Error_handler 

let print_newline () = if true (*!Config_complx.trace*) then print_newline () else print_string "\n" 
let trace_print s = if !Config_complx.trace then (print_string s;print_newline ()) 
let trace_newline () = if !Config_complx.trace then print_newline () else ()

let set a b = 
  match a with None -> ()
  | Some a -> b a 

let warn_message warn module_n  function_n k  = 
  (*let _ = set "Complx" set_application *)
  let _ = set warn set_message in 
  let _ = set function_n set_function_name in
  let _ = set module_n set_file_name in
  let _ = set k set_key in
  ()

let unsafe warn module_name function_name key x  = 
   warn_message warn module_name function_name key ;
   if !Config_complx.unsafe_mode then x else raise Exit 

let frozen_unsafe warn module_name function_name key x = 
  warn_message warn module_name function_name key ;
  if !Config_complx.unsafe_mode then x () else raise Exit 

let frozen_exit () = raise Exit 
    
let unsafe_frozen = frozen_unsafe  (*WTF!!*)


let list_fold f a b = List.fold_left (fun a b -> f b a) b a
let list_map f l = List.rev_map f (List.rev l)

let print_pretty_token print_string a print n any abstracted finit fany fabs  = 
  match a with Any -> (print_string any;fany n) 
  | Abstracted -> (print_string abstracted;fabs n) 
  | Init s -> (print s;finit (n,s))
  | Buggy -> (print_string "BUGGY";n)
  | _ -> (print_string "";n)

let i x = x
let i2 (x,_) = x
let s x = x+1 
let s2 (x,_) = x+1
let sif (x,b) = if b then s x else x


let print_pretty a which_ag (pretty_map,n) tuple_f print_any pref sigma sigma2 hash log =
  let sol = ref [] in 
  let print_string1 s = 
    match log with None -> ()
    | Some log -> Printf.fprintf log "%s" s in
   let print_string2 s =     sol:=s::(!sol) in
   let print_string s = 
    (print_string2 s;
     print_string1 s) in 
  let print_name a = 
    print_string1 (sigma a);
    print_string2 (sigma2 a) in
  if which_ag a
  then 
    let l = StringMap.fold 
	(fun a b sol -> (a,b)::sol) 
	(StringMap.find a pretty_map) 
	[]
    in 
    let bool = list_fold 
	(fun (x,tuple) (bool,(n:int)) -> 
	  let port = (a,x) in 
	  if (not (interesting tuple tuple_f))
	  then (bool,n)
	  else 
	    begin
	      
	      (if (not bool) then (print_string  (!Config_complx.site_separator))
	      else (print_string pref;print_name a;print_string "("));
	      print_string x;
	      let _ = 
		if h tuple_f.f_mark tuple.mark 
		then
		  print_pretty_token print_string tuple.mark (fun x -> print_string (mark_of x)) n  
		  (!Config_complx.mark_unknown) 
		    (!Config_complx.mark_abstracted) i2 i i 
		else if h tuple_f.f_impossible_marks tuple.impossible_marks && tuple.is_marked = Init true  
		then 
		  print_pretty_token print_string tuple.impossible_marks (List.iter (fun a -> print_string (not_mark a))) n "BAD" "BAD" i2 i i 
		else if h tuple_f.f_is_marked tuple.is_marked
		then
		print_pretty_token print_string tuple.is_marked (fun x -> print_string (if x then (!Config_complx.mark_unknown)  else (!Config_complx.unmark))) n (!Config_complx.marked_or_not) (!Config_complx.mark_abstracted)  i2 i i 
		else n in 
	      (if h tuple_f.f_link tuple.link 
              then 
	        false,match tuple.link with 
		    Init port2 -> 
		      			begin

			  match hash with None -> 
			    print_pretty_token 
			      print_string 
			      tuple.link 
			       (fun x -> print_string (bound_of_known x)) 
			      n  
			      (bound_to_unknown n)  
			      (!Config_complx.bound_abstracted) 
			      i2
			      i 
			      s
			  | Some(hash,nlink) -> 
			      print_pretty_token 
			      print_string 
			      (Init begin
				try 
				  let k  = 
				    Hashtbl.find hash (port2,port) in
				  let _ = 
				    Hashtbl.remove hash (port2,port) in
				  k 
				with
				  Not_found -> 
				    (nlink:=(!nlink)+1;
				     Hashtbl.add hash (port,port2) (!nlink) ;
				     (!nlink))
			      end)
				(fun x -> print_string ("!"^string_of_int x))
			      (n)   
			      (bound_to_unknown (n))
			      (!Config_complx.bound_abstracted) 
			      i2 
			      i 
			      s
			      
			end
		| _ -> 
		     print_pretty_token 
			      print_string 
			      tuple.link 
			      (fun x -> print_string (bound_of_known x)) 
			      n  
			      (bound_to_unknown n)  
			      (!Config_complx.bound_abstracted) 
			      i2 
			      i 
			      s


	      else if h tuple_f.f_impossible_links tuple.impossible_links && tuple.is_bound = Init true 
	      then 
		(false,print_pretty_token print_string tuple.impossible_links  (List.iter (fun a -> print_string (bound_not_site a))) n "BAD" "BAD" i2 i i)
	      else  if h tuple_f.f_is_bound tuple.is_bound 
	      then (false,print_pretty_token print_string tuple.is_bound (fun x -> print_string (if x then bound_to_unknown  n else (!Config_complx.free))) n  (!Config_complx.bound_or_not) (!Config_complx.bound_abstracted)  sif i i)  
	      else (*(false,n)*)
		(false,print_pretty_token print_string tuple.is_bound 
		  (fun x -> print_string 
		     (if x then "?"  else (""))) 
		  n 
		  (!Config_complx.bound_or_not) 
		  (!Config_complx.bound_abstracted)  
		  i2 i i) 
)
	    end)
	l (true,n) in 
    if fst bool then 
      (
      match print_any with 
	Some x  -> 
	  (print_string pref;print_name a;print_string x;true,!sol,snd bool)
      |	None -> 
	  (if (!Config_complx.skip_a_specie) = "" then (();false,!sol,snd bool)
	  else (print_string (!Config_complx.skip_a_specie);true,!sol,snd bool)))
    else (print_string ")";true,!sol,snd bool)
  else false,!sol,n

let remove l =
  match (List.rev l) with [] -> []
  |  t::q -> 
      let rec aux l' t sol =
	match l' with 
	  t'::q when t=t'-> aux q t sol
	|	t'::q -> aux q t' (t'::sol)
	|	[]    -> sol
      in aux q t [t] 



let full_time pt = pt.Unix.tms_utime +. pt.Unix.tms_stime +. pt.Unix.tms_cutime +. pt.Unix.tms_cstime 


let dump_chrono prefix l = 
  match List.rev l with [] -> ()
  | (s,t)::q  ->
      let _ = list_fold   
	(fun (s,t) t' -> 
	  print_string (fst prefix);
	  print_string s;
          print_float (t-.t');
          print_string " s.";print_newline ();t)
	  q t in ()



let justify s = 
  let n = String.length s in 
    s^(if n>40 
      then ""
      else 
	(String.make (41-n) ' ' ))
     

let chrono prefix s l = 
  let print_string x = Printf.fprintf stderr "%s" x in
  let rep = justify s  in 
  let _ = 
    if (!Config_complx.dump_chrono) && s<>""
    then (print_string (fst prefix); print_string s;print_string " is completed...\n";
	  flush stderr)
  in
  (rep,(full_time (Unix.times ())))::l



let compute_prefix s = 
  let n = String.length s in 
  let rec aux k current rep = 
    if k=n then rep,current
    else 
      let c = String.sub s k 1 in 
	if c="\\" or c="/" then aux (k+1) "" ((current^(c))::rep)
	else aux (k+1) (current^c) rep 
  in let a,b = (aux 0 "" []) in List.rev a, b 

let diff_prefix fileref file = 
  let ca,_ = compute_prefix fileref in 
  let cb,file = compute_prefix file in 
  let rec aux la lb =
    match la,lb with 
	t1::q1,t2::q2 when t1=t2 -> aux q1 q2
      | _,_ -> la,lb in 
  let ca,cb = aux ca cb in
  let rec aux la lb = 
    match la with 
	t::q -> aux q ("../"::lb)
      | []   -> lb in 
  let cb = aux ca cb in
  let sb = List.fold_left (fun s a -> s^a) "" cb in 
    (sb^file) 

let name_of_rule r = 
  match r.r_simplx.Rule.flag with 
    Some a -> a
  | None -> "%Auto_"^(string_of_int r.r_simplx.Rule.id) 

let print_option prefix log  = 
  match log with None -> (fun _  -> ())
  |  Some log -> ((fun x -> Printf.fprintf log "%s%s" (fst prefix) x;
		  flush log))
let empty_prefix = ("",[])

let init_counters ()  = 
  let rec init_ticks n = 
    if n > !Data.clock_precision 
    then (prerr_string "\n"; flush stderr ; IntSet.empty)
    else 
      begin
	prerr_string "_" ; flush stderr ;
	IntSet.add n (init_ticks (n+1))
      end
  in
  let c =  init_ticks 1 in
  c

let rec ticking clock c = 
  let c = 
    if IntSet.mem clock c then 
      begin
	prerr_string Data.tick_string ; 
	flush stderr ; 
	ticking (clock-1) (IntSet.remove clock c)
      end
    else (flush stderr ; c)
  in c 

let end_ticking () = (prerr_string "\n";flush stderr)

let get_option ()  =
  let _ = SuperargTk.parse options input_file  in 
  let _ = set_memory_usage 
      (!Config_complx.memory_limit)
      (fun () -> raise Exceptions.Memory_overflow)
  in ()
  
let sublist (l:string list) = 
  let rec aux l (sol:string list list) = 
    match l with 
      [] -> sol
    | (a,[])::q -> aux q ((List.rev a)::sol)
    | (a,t::q')::q -> aux ((t::a,q')::(a,q')::q) sol 
  in aux [[],l] []     

let rec ltrim x = 
  let n = String.length x in 
  let rec aux k = 
    if k=n then ""
    else if String.get x k = ' ' then aux (k+1)
    else String.sub x k (n-k) 
  in aux 0 


let string_prefix x y = 
  x = y 
    or 
  (let n = String.length x in 
  let n'= String.length y in 
  if n<n' then x = String.sub y 0 n 
  else false)


(*let unify_int_string_map a b = 
  let sola = ref StringMap.empty in
  let solb = ref StringMap.empty in 
  let _ = 
    IntMap.iter2i 
      (fun a b -> (sola:=StringMap.add b a !sola))
      (fun a b -> (solb:= StringMap.add b a !solb))
      (fun a b c -> 
	if b=c 
	then () 
	else (sola:=StringMap.add b a !sola;
              solb:=StringMap.add c a !solb))
      a
      b in
  let map = ref IntMap.empty in
  let _ = 
    StringMap.iter2 
    (fun _ _  -> raise Exit)
    (fun _ _  -> raise Exit)
    (fun a b c -> map:=IntMap.add c b !map)
    !sola !solb
  in
  let map = !map in 
  let sigma x = 
    try 
      IntMap.find x map
    with 
      Not_found -> x in 
  sigma 
*)
