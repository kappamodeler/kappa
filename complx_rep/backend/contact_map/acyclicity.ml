open Data_structures

let is_acyclic pb = 
  try (
    match pb.Pb_sig.first_encoding 
    with None -> raise Exit
    |  Some cpb -> 
	let add_relation x y (set,map,mapop) = 
	  if x=y then raise Exit 
	  else 
	    let set' = String2Set.add x (String2Set.add y set) in
	    let map' = 
	      let old = 
		try Pb_sig.String2Map.find x map 
		with Not_found -> String2Set.empty in
	      Pb_sig.String2Map.add x (String2Set.add y old) map in
	    let mapop' = 
	       let old = 
		try Pb_sig.String2Map.find y mapop 
		with Not_found -> String2Set.empty in
	      Pb_sig.String2Map.add y (String2Set.add x old) mapop in
	    set',map',mapop' in
	let empty = 
	  String2Set.empty,Pb_sig.String2Map.empty,Pb_sig.String2Map.empty in
	let (set,map,map') = 
	  Pb_sig.String22Set.fold 
	    (fun (x,(a,s)) ->
	      List.fold_right  
		(fun s' -> if s=s' then (fun x -> x)
		else add_relation x (a,s') 
			)
		(snd (StringMap.find a cpb.Pb_sig.cpb_interface_of_agent)))
	    (match pb.Pb_sig.contact_map 
	    with None -> raise Exit
	    |	Some a -> a.Pb_sig.relation_set)
	    
	    empty
	in 
	let _ = 
	  String2Set.fold 
	    (fun x (map,mapop)->
	      let succ = 
		try 
		  Pb_sig.String2Map.find x map 
		with Not_found -> String2Set.empty in
	      let pred = 
	      try
		Pb_sig.String2Map.find x mapop
	      with Not_found -> String2Set.empty in
	      let map' = 
		String2Set.fold 
		  (fun predx -> 
		    if String2Set.mem predx succ then raise Exit
		    else
		      let old = 
			try 
			  Pb_sig.String2Map.find predx map 
			with Not_found -> String2Set.empty in
		    Pb_sig.String2Map.add predx (String2Set.union old succ))
		  pred map in
	      let mapop' = 
		String2Set.fold 
		  (fun succx -> 
		    if String2Set.mem succx pred then raise Exit
		    else
		      let old = 
			try 
			  Pb_sig.String2Map.find succx mapop 
		      with Not_found -> String2Set.empty in
		      Pb_sig.String2Map.add succx (String2Set.union old pred))
		  succ mapop in
	      (Pb_sig.String2Map.remove x map',
	       Pb_sig.String2Map.remove x mapop')
		) 
	    set (map,map') in true)
  with Exit -> false
 
