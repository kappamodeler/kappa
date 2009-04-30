(* 2009/01/08*)
(* Meta language for Kappa *)
(* Jerome Feret*)
(* Agent interface*)
(* agent_interfaces.ml *)

open Error_handler 
open Data_structures_metaplx 


let error i = 
  unsafe_frozen None (Some "agent_interfaces.ml") None (Some ("line "^(string_of_int i))) (fun () -> (failwith ("ERROR"^(string_of_int i)))) 



let convert_action action res = 
  match action with 
    Add_site _ | Rename _ -> action::res
  | Delete_site site -> (Rename (site,[]))::res
  | Mutate_site (site1,site2) -> (Rename (site1,[]))::(Add_site site2)::res

let compute_interface starting_interface directives = 
  let map = 
    SiteSet.fold
      (fun (x:site) map  -> SiteMap.add x [x] map)
      starting_interface 
      SiteMap.empty 
  in
  let map,newsites,sources,targets = 
    List.fold_left 
      (fun (map,newsites,sources,targets) d -> 
	match d with 
	  Add_site (site) -> 
	    if 
	      SiteSet.mem site targets 
	    then 
	      failwith ("Site "^site^" occurs several time as a new site")
	    else
	      (map,SiteSet.add site newsites,sources,SiteSet.add site targets)
	| Rename (site,l) -> 
	    if 
	      SiteSet.mem site sources 
	    then 
	      failwith ("Site "^site^" occurs several time as a modified site")
	    else if 
	      try 
		let _ = 
		  SiteMap.find site map
		in false
	      with 
		Not_found -> true 
	    then 
	      failwith ("Site "^site^" is not defined")
	    else 
	      SiteMap.add site l map,
              newsites,
	      SiteSet.add site sources,
	      List.fold_left
		(fun targets site -> 
		  if SiteSet.mem site targets 
		  then 
		    failwith ("Site "^site^" occurs several time as a new site")
		  else
		    SiteSet.add site targets)
		targets l
	| _ -> error 34)
      (map,SiteSet.empty,SiteSet.empty,SiteSet.empty) directives
  in 
  map,newsites 

let compute_interface_portion  starting_interface directives = 
  let map = 
    List.fold_left 
      (fun map x -> SiteMap.add x [x] map)
      SiteMap.empty 
      starting_interface 
  in
  let map,newsites,sources,targets = 
    List.fold_left 
      (fun (map,newsites,sources,targets) d -> 
	match d with 
	  Add_site (site) -> 
	    if 
	      SiteSet.mem site targets 
	    then 
	      failwith ("Site "^site^" occurs several time as a new site")
	    else
	      (map,
	       SiteSet.add site newsites,
               sources,
	       SiteSet.add site targets)
	| Rename (site,l) -> 
	    if 
	      SiteSet.mem site sources 
	    then 
	      failwith ("Site "^site^" occurs several time as a modified site")
	    else if 
	      try 
		let _ = 
		  SiteMap.find site map
		in false
	      with 
		Not_found -> true 
	    then 
	      (map,newsites,sources,targets)
	    else 
	      SiteMap.add site l map,
	      newsites,
	      SiteSet.add site sources,
	      List.fold_left
		(fun targets site -> 
		  if SiteSet.mem site targets 
		  then 
		    failwith ("Site "^site^" occurs several time as a new site")
		  else
		    SiteSet.add site targets)
		targets l
	| _ -> error 34)
      (map,
       SiteSet.empty,
       SiteSet.empty,
       SiteSet.empty) 
      directives
  in 
  map,newsites 


let abstract map = 
  (fun x -> 
    try 
      SiteMap.find x map
    with 
      Not_found -> error 76)
  
let compute_subs (starting_subs) directives =
  SiteMap.map 
    (fun l -> 
       let a,b = compute_interface_portion l directives in 
	 SiteMap.fold 
	   (fun l l2 sol -> 
	      List.fold_left 
		(fun q a -> a::q)
		(if SiteSet.mem l b then l::sol else sol) 
		l2)
	     a
	   [])
    starting_subs 

