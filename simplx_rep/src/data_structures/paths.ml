open Mods2
type t = (string,StringSet.t IntMap.t) Hashtbl.t

let empty() = Hashtbl.create 10

let add name id site paths = 
  let m = try Hashtbl.find paths name with Not_found -> IntMap.empty in
  let set = try IntMap.find id m with Not_found -> StringSet.empty in
  let set' = StringSet.add site set in
  let m' = IntMap.add id set' m in
    Hashtbl.add paths name m' ;
    paths

let clashing_on_names ?(debug=false) (no_helix,no_poly) paths1 paths2 = 
  if no_helix or no_poly then
    try
      Hashtbl.iter (fun name m ->
		      try
			let m' = Hashtbl.find paths2 name in
			  if no_helix then
			    let _ = 
			      if debug then (Printf.printf "Clash on name %s and constraints impose no helix formation" name ; flush stdout)
			      else ()
			    in
			      raise True
			  else
			    let _ = 
			      if debug then (Printf.printf "Checking whether rule application is polymerizing..." ; flush stdout)
			      else ()
			    in
			      IntMap.iter (fun _ set -> 
					     IntMap.iter (fun _ set' ->
							    if StringSet.equal set set' then
							      if debug then
								begin
								  Printf.printf "Clash on name %s but %s=%s\n" 
								    name 
								    (string_of_set (fun x->x) StringSet.fold set) 
								    (string_of_set (fun x->x) StringSet.fold set')  ;
								  flush stdout
								end
							      else ()
							    else
							      let _ =
								if debug then 
								  begin
								    Printf.printf "Clash on name %s and %s<>%s\n" 
								      name 
								      (string_of_set (fun x->x) StringSet.fold set) 
								      (string_of_set (fun x->x) StringSet.fold set')  ;
								    flush stdout 
								  end 
							      in
								raise True
							 ) m'
					  ) m
		      with Not_found -> ()
		   ) paths1 ;
      false
    with True -> true
  else
    false
