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
        let g (x,(a,s)) = 
          List.fold_right  
	    (fun s' -> if s=s' then (fun x -> x)
	     else add_relation x (a,s') 
	    )
	    (snd (StringMap.find a cpb.Pb_sig.cpb_interface_of_agent))
        in 
	let (set,map,map') = 
	  Pb_sig.String22Set.fold 
	    (fun (x,x') sol  -> g (x,x') (g (x',x) sol))
	    (match pb.Pb_sig.contact_map 
	    with None -> raise Exit
	    |	Some a -> a.Pb_sig.relation_set )	    
	    empty
	in 
        let rec aux next to_visit = 
          match next with 
              [] -> 
                begin
                  if String2Set.is_empty to_visit 
                  then 
                    true 
                  else 
                    let next = String2Set.min_elt to_visit 
                    in 
                      aux [next,None,String2Set.singleton next] (String2Set.remove next to_visit)
                end
            | (t,from,black)::q -> 
                let succ = 
                  try 
                    Pb_sig.String2Map.find t map 
                  with 
                      Not_found -> String2Set.empty
                in 
                let succ = 
                  match from with None -> succ
                    | Some a -> String2Set.remove a succ 
                in 
                  aux 
                    (String2Set.fold 
                       (fun a l -> 
                          if String2Set.mem a black
                          then raise Exit 
                          else 
                            (a,Some t,String2Set.add t black)::l
                       )
                       succ 
                       q)
                    (String2Set.diff to_visit succ)
        in 
          if String2Set.is_empty set then true 
          else 
            let start = String2Set.min_elt set in 
              aux 
                [start,None,String2Set.singleton start]
                (String2Set.remove start set)
  )                
  with Exit -> false
 
