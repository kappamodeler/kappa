open Tools 
open Tools2
open Data_structures
open Pb_sig 

type algo = Exploration | Warshall 

let algo = Warshall 

let debug = false 
let exit i = 
  frozen_unsafe None (Some "find_cycles.ml") None  (Some (string_of_int i)) (fun () -> raise Exit) 

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
		 0 -> (print_string ((fst a)(**)^"."^(snd a)(**));1)
	       | 1 -> (print_string ("--"^(snd a)^".");2)
	       | 2 -> (print_string ((fst a)^"."^(snd a));1)
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



let find_cycles limit pb = 
  let too_long = 
    match limit 
    with None -> (fun _ -> false)
    | Some y -> (fun x -> x>y) in
  try (
    match pb with None -> exit 10
    | Some pb -> 
	begin
	  match pb.Pb_sig.first_encoding 
	  with None -> exit 14
	  |  Some cpb -> 
	      let access = (fun (a,b) -> 
		[(b,b,a)]) in  
	      let access2 = 
		(fun a -> 
		  try 
		    String2Map.find 
		      a 
		      (match pb.Pb_sig.contact_map
		      with  
			None -> exit 21
		      | Some a -> a.Pb_sig.link_of_site)
		  with Not_found -> ([]))  in
	      let access = access2 in 
	      let interface = 
		StringMap.fold 
		  (fun a (_,b) sol -> 
		    StringMap.add a 
		      (let set = List.fold_left 
			 (fun img s -> 
			   List.fold_left 
			     (fun img (a',_,s') -> 
			       StringSet.add a' img)
			     img 
			     (access2 (a,s)))
			 StringSet.empty
			 b
		      in StringSet.fold (fun a b -> a::b) set [])
			 sol)
		  cpb.Pb_sig.cpb_interface_of_agent StringMap.empty in
	      let _ = 
		if debug 
		then 
		  let _ = print_string "INTERFACE\n" in
		  let _ = 
		    StringMap.iter 
		      (fun a b -> 
		      print_string a;
			List.iter 
			  (fun (a) -> 
			    let _ = print_string a in
			    let _ = print_string "|" in
			   ())
			  b;
			print_newline ()) interface in
		  ()
	      in 
	      let set = 
		StringMap.fold
		  (fun x b sol -> 
		    List.fold_left
		      (fun sol s -> (x,s)::sol)
		      sol b)
		       interface [] in
			    
	      let interface = 
		(fun a -> 
		  try 
		    StringMap.find a interface
		  with 
		    Not_found -> []) in 
	      match algo,limit with 
		Exploration,_ | _,Some _ -> 
		  begin (*Exploration*)
		    Some 
		      (List.fold_left  
			 (fun cycles x ->
			   let rec aux to_visit cycles = 
			     match to_visit with 
			       [] -> cycles 
		       | (path,size,blackagent,blackport,blackmap,last)::q when too_long size -> aux q cycles 
		       | (path,size,blackagent,blackport,blackmap,last)::q -> 
			   match last with 
			     IN(a,s) -> 
			       let to_visit,cycles = 
				 begin
				   List.fold_left 
				     (fun (to_visit,cycles)  s' -> 
				       if s = s' or compare x (a,s')<0 then 
					 to_visit,cycles 
				       else if String2Set.mem (a,s') blackport 
				 then 
					 (if a=(fst x) 
					 then (print_a_cycle ((a,s')::path);
					       to_visit,(*((a,s')::path)::*)cycles)
					 else to_visit,cycles)
				       else
					 ((a,s')::path,size,
					  blackagent,
					  String2Set.add (a,s') blackport,
					  StringMap.add a s' blackmap,
				    OUT(a,s'))::to_visit,cycles)
				     (q,cycles) (interface a)
				 end
			       in
			       aux to_visit cycles
			   | OUT(a,s) -> 
			       let to_visit,cycles = 
				 begin
				   List.fold_left 
				     (fun (to_visit,cycles) (_,a',s') -> 
				       if compare x (a',s')<0 
				       then to_visit,cycles
				       else if StringSet.mem a' blackagent 
					   && not ((s'=(StringMap.find a' blackmap)) )
				       then 
					 (
					 if a'=(fst x)
					 then 
					   (print_a_cycle ((a',s')::path);
					    to_visit,((a',s')::path)::cycles)
					 else
					   to_visit,cycles)
				       else
					 (
					 (a',s')::path,size+1,
					 StringSet.add a' blackagent,
					 String2Set.add (a',s') blackport,
					 blackmap,
					 IN(a',s'))::to_visit,cycles)
			             (q,cycles) 
				     (access (a,s)) 
				 end
			       in
			       aux to_visit cycles
			   in
			   aux 
			     [[x],1,
			       StringSet.singleton (fst x),
			       String2Set.singleton x,
			       StringMap.add (fst x) (snd x) StringMap.empty,
			       OUT(x)] cycles)
			 [] set  )
		      end(*Exploration*)
	      |	Warshall,None -> 
		  let add_path x y path matrice = 
		    let old = 
		      try 
			IoMap.find x matrice 
		      with 
			Not_found -> IoMap.empty
		    in
		    let old2 = 
		      try 
			IoMap.find y old 
		      with 
			Not_found -> [] in
		    IoMap.add 
		      x
		      (IoMap.add y (path::old2) old)
		      matrice in
		  let add_path x y path (matrice,matriceop) = 
		   (* let _ = print_string "X: " in
		    let _ = print_io x in
		    let _ = print_string "\nY: " in
		    let _ = print_io y in
		    let _ = print_newline () in
		    let _ = print_path (fst path) in
		    let _ = print_newline () in
		    let _ = print_newline () in *)
		    add_path x y path matrice,
		    add_path y x path matriceop in
		  let next io = 
		    match io with IN(x,s) -> 
		      let y = interface x in
		      List.fold_left
			(fun sol y -> if y=s then sol else (OUT(x,y)::sol))
			[] y  
		    | OUT(x,s) -> [IN(s,x)] in
		
		  Some 
		    (let matrice,matrice_op = 
		       (List.fold_left 
			  (fun matrices x -> 
			    let _ = print_io x in
			    let _ = print_newline () in 
			    let ylist = next x in
			      List.fold_left
				(fun matrices y -> 
				  let matrice,matrice_op = 
				    add_path x y ([x;y],
				    IoSet.empty) 
				      matrices in
				  let matrice,matrice_op = 
				    IoMap.fold 
				      (fun x' path_x'_x_list (matrice,matrice_op) -> 
					IoMap.fold
					  (fun y' path_y_y'_list (matrice,matrice_op) -> 
					    List.fold_left
					      (fun  (matrice,matrice_op) (path_x'_x,setx) -> 
						List.fold_left
						  (fun (matrice,matrice_op) (path_y_y',sety)  -> 
						    if not (IoSet.is_empty 
							      (IoSet.inter setx sety)) or y'=x or x'=y 
						    then
						    (matrice,matrice_op)
						    else
						      let matrice,matrice_op =
							add_path 
							  x' 
						      y' 
							  (path_x'_x@path_y_y',
							   IoSet.add x 
							     (IoSet.add y 
								(IoSet.union setx sety))) 
							  (matrice,matrice_op) in
						      (matrice,matrice_op))
						(matrice,matrice_op)
						  path_y_y'_list 
						  )
					      (matrice,matrice_op)
					      (path_x'_x_list:((io list * IoSet.t)list))
					    )
					  (try 
					    IoMap.find y matrice
					  with 
					  Not_found -> IoMap.empty)
					  (matrice,matrice_op))
				      (try
					IoMap.find x matrice_op
				      with Not_found -> IoMap.empty)
				      (matrice,matrice_op) 
				      in 
				  let y_to_x = 
				    try 
				      IoMap.find x 
					(IoMap.find y matrice)
				    with 
				      Not_found 
					-> [] in
				  List.fold_left 
				    (fun matrices p -> 
				      add_path x x p matrices)
				    (matrice,matrice_op)
				    y_to_x )
			      matrices ylist   )
				  
			  (IoMap.empty,IoMap.empty)  
			  ((List.map (fun x -> IN x) set)@(List.map (fun x -> OUT x) set)))
			in 
		    IoMap.fold
		      (fun x xmap sol -> 
			(List.map 
			   (fun x -> 
			     List.map 
			     (fun x -> 
			       match x 
			       with IN x -> x
			       | OUT x -> x)
			       (fst x))
			begin
			  try IoMap.find x xmap
			  with Not_found -> []
			end)@sol)
		      matrice [])
			   
		    
		
		
	      end )
  with Exit -> (print_string "BOF";None)
      
   
