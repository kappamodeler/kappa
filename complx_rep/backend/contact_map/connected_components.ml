open Tools 
open Tools2
open Data_structures
open Pb_sig 

type algo = Exploration | Warshall 

let algo = Warshall 

let debug = false 
let exit i = 
  frozen_unsafe None (Some "Connected_components") None (Some (string_of_int i)) (fun () -> raise Exit) 

type io = IN of (string*string) | OUT of (string*string)
module IoMap = Map2.Make (struct type t = io let compare = compare end)
module IoSet = Set.Make (struct type t = io let compare = compare end)

let print_io x = 
  match x with 
    IN(x,s) -> (print_string "IN";print_string x;print_string ".";print_string s)
  | OUT(x,s) -> (print_string "OUT";print_string x;print_string ".";print_string s) 

let print_path l = 
  let _ = List.iter print_io l in 
  print_newline ()


let print_a_cycle a = 
  let _ = List.fold_left 
	     (fun old a -> 
	       match old 
	       with 
		 0 -> (print_string ((fst a)(*^"."^(snd a)*));1)
	       | 1 -> (print_string (""(*"--"^(fst a)(*^"."^(fst a)*)*));2)
	       | 2 -> (print_string ("--"^(fst a));1)
	       | _ -> exit 36 )
		          0 a in
  print_string ("--"^(fst (List.hd a)));
  print_newline () 

let print_cycles a = 
  match a with 
    None -> print_string "Internal error"
  | Some a -> 
      (print_string "Potential cycles:\n";
       List.iter 
	 (fun x -> 
	   print_a_cycle x)
	 a)



let detecte_connected_components  pb = 
  try (
    match pb with None -> exit 10
    | Some pb -> 
	begin
	  match pb.Pb_sig.first_encoding 
	  with None -> exit 14
	  |  Some cpb -> 
	      let add a x map = 
		let f a x map = 
		  let old = 
		    try 
		      StringMap.find a map 
		    with
		      Not_found -> StringSet.empty in
		  StringMap.add a (StringSet.add x old) map in
		f a x (f x a map) in
	      let relation = 
		List.fold_left 
		  (fun map ((a,x),(b,y)) -> 
		    add a b map)
		  StringMap.empty
		  (match pb.Pb_sig.contact_map
		  with None -> exit 36
		  | Some a -> a.Pb_sig.relation_list) in
	      let set = 
		match pb.Pb_sig.intermediate_encoding 
		with 
		  None -> 
		    begin
		    match pb.Pb_sig.gathered_intermediate_encoding 
		    with 
		    Some a -> a
		    | None ->
		      exit 80
		    end
		| Some a -> a in
	      let set = 
		List.rev_map (fun (a,_,_) -> a) set.Pb_sig.cpb_interface  in 
	      let set = 
		List.fold_left
		  (fun set a -> StringSet.add a set)
		  StringSet.empty 
		  set in
	      let rec vide set sol = 
		if StringSet.is_empty set 
		then sol
		else
		  begin
		    let a = StringSet.min_elt set in
		    let rec visit t black  = 
		      if StringSet.is_empty t then black 
		      else
			let elt = StringSet.min_elt t in
			let to_visit = StringSet.remove elt t in 
			let neigh = 
			  try 
			    StringMap.find elt relation
			  with
			    Not_found -> StringSet.empty in
			let neigh = 
			  StringSet.diff neigh black in
			let black = 
			  StringSet.union black neigh in
			let to_visit = StringSet.union neigh to_visit in
			visit to_visit black in
		    let comp =
		      visit 
			(StringSet.singleton a)
			(StringSet.singleton a) in
		    vide 
		      (StringSet.diff set comp)
		      ((StringSet.fold (fun a b -> a::b) comp [])::sol)
		  end
	      in
	      let rep = vide set [] in 
	      let _ = List.iter
		  (fun x -> 
		    let _ = List.fold_left 
		      (fun bool x -> 
			(if bool then print_string ",");
			print_string x;true) false x in 
		      print_newline ())
		  rep in
	      Some rep
	end)
  with 
    _ -> None 
		      
