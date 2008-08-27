
type agent_id = string
type agent_type = string
type site_type = string

module SitetypeMap = Map2.Make (struct type t = site_type  let compare = compare end)

(** type for relative path, i.e. sequence of type bonds *)
type path = ((agent_type*site_type)*(agent_type*site_type)) list

(** type for absolute path, i.e. relative path with an anchor that is an agent id*)
type rooted_path = 
    {root:agent_id;
     path:path}

let build_rpath a b = 
  {root = a;path=b}

let empty_rpath = 
  build_rpath "" []

module PathMap = Map2.Make (struct type t = path let compare = compare end)
module RPathMap = Map2.Make (struct type t = rooted_path let compare = compare end)
module RPathSet = Set.Make (struct type t = rooted_path let compare = compare end)

(** function to get a value associated to a rooted path in a map, return None is there is no association *)

let get_rpath_value a b map = 
  try  
    Some(RPathMap.find {root=a;path=b} map) 
  with 
    Not_found -> None

(** function to add an association between a rooted path and a value, return None in case of conflict*)
let add_rpath_value a b c map = 
  match get_rpath_value a b map 
  with None -> Some (RPathMap.add {root=a;path=b} c map) 
  | Some d when d=c -> Some map
  | _ -> None 

(** unify two map between rooted paths and values, if it is possible, return None otherwise*)
(*let unify_comp map1 map2 = RPathMap.unify*)

let build_empty_path x = {root=x;path=[]}
