(** Implementation of fragments for ODE *)

open Tools 
open Data_structures
open Annotated_contact_map 
open Views 
open Pb_sig
open Fragments_sig 

(** Set this boolean to true to dump more debugging information *)
let trace = false 

let error i s = 
  unsafe_frozen None (Some "fragments.ml") s (Some ("line  "^(string_of_int i))) (fun () -> raise Exit)


module Fragment = 
  (struct 

(** former definition of fragments, a fragment was a list of views *)
type fragment = views_id list 

(** former definition of subspecies, a subspecies was a list of views *)
type subspecies = views_id StringMap.t 

(** definition of the empty fragment for the new type definition *)
let empty_fragment = []
let empty_species = StringMap.empty 

(** Pretty-print function for new fragment type *)
let print_fragment a =
  let _ = 
    List.fold_left 
      (fun i j -> 
	let _ = if i>0 then print_string "," in
	let _ = print_string ":" in
	let _ = print_int j in
	(i+1))
    0 a 
  in () 
  
(** optional pretty print function for new fragment, it only pretty print when the compilation has been made with trace=true *)
let trace_print_fragment = 
  if trace then print_fragment 
  else (fun _ -> ())
  

let print_species x = 
  let _ = 
    StringMap.fold
      (fun a v bool -> 
	let _ = 
	  if bool then print_string ","
	in
	let _ = print_string a in
	let _ = print_string ":" in
	let _ = print_int v in
	true)
      x false
  in print_newline () 



(** compute the cannonical fragment associated with a subspecies *)
let canonical_fragment_of_subspecies subspecies =
  let l = 
    StringMap.fold 
      (fun _ a l -> a::l)
      subspecies []
  in 
  List.sort compare l 


(** emptyness test for fragments *) 
let is_empty_fragment x = x=[]

(*
let from_views_id_list x = {fragment=x;agent_set = StringSet.empty}
let to_views_id_list x = x.fragment 
*)

let concat a b = if b = [] then a else a@b



(**If the boolean is true then this function associates a maximal list of compatible fragments to a bond 
If the boolean is false then this function associated a maximal list of fragments to a bond 
This function is hash consed *)
let get_denum bool (agent_to_int_to_nlist,view_of_tp_i,ode_handler) = 
  let hash = Hashtbl.create 21 in
  let f x = 
    try 
      Hashtbl.find hash x
    with 
      Not_found -> 
	let rec aux current compatibility sol = 
	  match current with 
	    [] -> sol
	  | ([],black,stack)::q -> 
	      let compatibility' = 
		if not bool then compatibility 
		else 
		  StringMap.fold 
		    (fun _ tp_i comp  -> 
		      let view = view_of_tp_i tp_i in 
		      let agent = agent_of_view view in 
		      let interface = interface_of_view view in 
		      if 
		      try 
			StringMap.find agent comp = interface 
		      with Not_found -> true 
		      then 
			StringMap.add agent interface comp 
		      else
			error 40 (Some "Incompatible agents")
			  )
		    stack 
		    compatibility 
	      in 
	      aux q compatibility' (stack::sol) 
	  | ((a,s,a',s')::b,black,stack)::q ->
	      let _ = 
		if StringSet.mem a black 
		then 
		  error 47 (Some "Infinite number of fragments") 
	      in
	      let black' = StringSet.add a black in 
	      let ag1,s1,ag2,s2 = (a,s,a',s') in 
	      let tp = 
		try 
		  StringListMap.find 
		    [s] 
		    (StringMap.find a agent_to_int_to_nlist)
		with Not_found -> error 1135 None 
	      in
	      let tp = 
		List.filter 
		  (fun tp -> 
		    let view = view_of_tp_i tp in 
		    let interface = interface_of_view view  in 
		    let agent = agent_of_view view in
		    try 
		      (not bool) or StringMap.find agent compatibility = interface 
		    with Not_found -> true )
		  tp in 
	      let q' =
		List.fold_left
		  (fun q' n_tp -> 
		    let view = view_of_tp_i n_tp in 
		    if 
		      let rec aux l = 
			match l with [] -> false
			| t::q -> 
			    begin
			      match ode_handler.b_of_var (fst t),snd(t) with AL((x,y,z),(t,u)),bool 
				when x=ag1 && y=ag1 && z=s1 && t=ag2 && u = s2 -> bool
			      | _ -> aux q
			    end
		      in aux (valuation_of_view view)
		    then 
		      
		      let b' = 
			String4Set.fold 
			  (fun ((ag1,s1),(ag2,s2)) b' -> 
			    if ag2 = a' && s'=s2  then b'
			    else (ag2,s2,ag1,s1)::b')
			  (pending_edges view) b 
		      in
		      (b',black',StringMap.add a n_tp stack)::q'
		    else q')
		  q tp in
	      aux q' compatibility sol in
	let rep = aux [[x],StringSet.empty,StringMap.empty] StringMap.empty [] in
	(Hashtbl.add hash x rep;
	 rep)
  in f 

    let is_empty_fragment x = x=[]
    let is_empty_species = StringMap.is_empty

  let complete_subspecies  (pending_edges,view_of_tp_i,keep_this_link,get_denum) (tp:subspecies) = 
    (** This function takes a subfragment and build the list of the fragments that contains it *)
    let root,target,map = 
      StringMap.fold 
	(fun _ tp (root,target,map)  -> 
	  let pending_edges = pending_edges (view_of_tp_i tp) in
	  String4Set.fold 
	    (fun (y1,y2) (root,target,map) ->
	      (String2Set.add y1 root,
	       String2Set.add y2 target,
	       String2Map.add y2 y1 map))
	    pending_edges (root,target,map))
	tp 
	(String2Set.empty,String2Set.empty,String2Map.empty) 
    in
    let target = String2Set.diff target root in
    let (target2) = 
      String2Set.fold
	(fun y2 liste ->
	  let y1 = 
	    try String2Map.find y2 map 
	    with Not_found -> 
	      error 118 None  in
	  if keep_this_link y1 y2 
	  then 
	    (get_denum (fst y2,snd y2,fst y1,snd y1))::liste
	  else liste)
	
	target [] in
    let sol = 
      let rec aux l (sol:subspecies list) = 
	match l with [] -> sol
	| (t:subspecies list)::q -> 
	    aux q 
	      (List.fold_left 
		 (fun liste elt_t -> 
		   List.fold_left  
		     (fun sol b -> 
		       (match 
			 (StringMap.unify elt_t (b:'a StringMap.t))
		       with Some a -> a 
		       | None -> error 222 None)::liste)
		     liste sol)
		 [] 
		 t) in 
      aux 
	target2 
	[tp] in 
    sol 



(** this primitive split a species into a list of maximal connected subspecies *)
let split_subspecies views_data_structure ode_handler annotated_contact_map subspecies = 
  let tp_set = 
    StringMap.fold 
      (fun a _ sol -> StringSet.add a sol)
      subspecies 
      StringSet.empty 
  in
  let ag_to_tp = subspecies in 
  let _ = 
    if debug then 
      let _ = print_string "\n SPLIT_TP_LIST \n" in
      let _ = print_newline () in
      let _ = 
	StringMap.iter (fun a i -> 
	  print_string a;
	  print_int i)
	  ag_to_tp in () 
  in 
  let rec vide sol_en_cours interface reste sol = 
    let _ = 
      if debug 
      then 
	let _ = print_string "RESTE \n" in
	let _ = StringSet.iter print_string reste in
	let _ = print_newline () in () in  
    match interface with 
      [] -> 
	begin
	  if StringSet.is_empty reste 
	  then 
	    sol_en_cours::sol
	  else
	    let min = StringSet.min_elt reste in
	    let reste = StringSet.remove min reste in
	    let image = 
	      try 
		StringMap.find min subspecies 
	      with 
		Not_found -> error 270 None in 
	    vide 
	      (StringMap.add min image StringMap.empty) 
	      (compute_interface_tp_i  image views_data_structure.interface_map ode_handler annotated_contact_map) reste (sol_en_cours::sol)
	end
    | c::q ->
	if StringSet.mem c reste 
	then 
	  let image = 
	    try 
	      StringMap.find c subspecies
	    with 
	      Not_found -> error 280 None 
	  in 
	  
	  vide 
	    (StringMap.add c image sol_en_cours) 
	    ((compute_interface_tp_i image views_data_structure.interface_map ode_handler annotated_contact_map)@q)
	    (StringSet.remove c reste)
	    sol
	else
	  vide 
	    sol_en_cours
	    q
	    reste
	    sol in
  let sollist = 
    let reste = tp_set in 
    if StringSet.is_empty reste then 
      []
    else
      let min = StringSet.min_elt reste in
      let reste = StringSet.remove min reste in
      let image = 
	try StringMap.find min subspecies 
	with 
	  Not_found -> error 305 None 
      in
      vide 
	(StringMap.add min image StringMap.empty) 
	(compute_interface_tp_i image views_data_structure.interface_map  ode_handler annotated_contact_map) reste [] 
  in 
  let _ = if debug then 
    let _ = print_string "\n SOL \n" in
    let _ = 
      List.iter 
	(fun x -> 
	  StringMap.iter 
	    (fun a y -> print_string a;print_int y;print_string ",")
	    x;
	  print_newline ())
	sollist in () in  
  sollist 


let canonical_form = canonical_fragment_of_subspecies 
    
let species_from_fragment x = x 

let fold_views f a b = List.fold_left (fun a b -> f b a) b a
let iter_views  = List.iter 
let iter_views_in_species f = StringMap.iter (fun _ b -> f b)
let scan_views f x = 
  let rec aux l = 
    match l with [] -> None
    | t::q -> 
	begin
	  match f t with
	    None -> aux q
	  | x -> x 
	end
  in
  aux x 

let scan_views_in_species f x= 
  let rep = ref None in
  let _ = 
    try 
      StringMap.iter 
	(fun _ a -> 
	  match f a with 
	    None -> ()
	  | Some a -> rep:=Some a;raise Exit)
	x
    with Exit -> () in 
  (!rep)

let species_of_fragment x = x

let plug_views_in_subspecies a x l = StringMap.add a x l 

let get_views_from_agent_id views_of_tp_i agent_id agent_type l = 
  try 
    Some (let a = StringMap.find agent_id l in 
          a,views_of_tp_i a)
  with 
    Not_found -> None 


let get_neighbour t (a,b) c = c 

let build_species agent_of_view_id subspecies ext = 
   List.fold_left
    (fun (species:subspecies) (_,tp_i) -> StringMap.add (agent_of_view_id tp_i) tp_i species)
    subspecies 
    ext 


let apply_blist_with_species ode_handler views_data_structures keep_this_link rule_id species context_update  = 
  let context_update = 
    List.fold_left
      (fun list (b,bool) -> 
	match b,bool  with 
	  AL((_,b,c),(d,e)),false when  keep_this_link (b,c) (d,e) -> 
	    ((B(d,d,e),false)::(AL((d,d,e),(b,c)),false)::list)
	| _ -> list)
      context_update
      context_update 
  in 

  begin
    let fadd x b bool map = (* add a boolean attribute for the agent_type x *)
      let old = 
	try
	  StringMap.find x map 
	with 
	  Not_found -> 
	    BMap.empty in
      StringMap.add x 
	(match b with 
	| M((_,y,_),_) 
	| AL((_,y,_),_) 
	| B(_,y,_) when y=x -> BMap.add b bool old
	| _ -> old)
	map  in
    let faddprim x b bool map = (* modifies a boolean attirbute if it is already defined, skip otherwise *)
      let old = 
	try 
	  StringMap.find x map 
	with 
	  Not_found -> 
	    BMap.empty in
      try 
	BMap.find b old ; fadd x b bool map 
      with 
	Not_found -> 
	  map in 
    let bmap = StringMap.empty,StringMap.empty,StringMap.empty in
    let bmap,id_to_type,type_to_id = 
      StringMap.fold 
	(fun agent_id views_id (bmap,id_to_type,type_to_id) -> 
	  let view = view_of_tp_i views_id  views_data_structures.interface_map in
	  let agent = agent_of_view view in 
	  let bmap = (* create the attribute map for the view_id  *)
	    List.fold_left 
	      (fun bmap  (b,bool) ->
		fadd 
		  agent 
		  (ode_handler.b_of_var b) 
		  bool 
		  bmap)
	      bmap (valuation_of_view view)
	  in
	  let bmap = (* take into account the modification *)
	    List.fold_left 
	      (fun map (b,bool) -> 
		faddprim 
		  agent 
		  b 
		  bool 
		  map)
	      bmap context_update 
		in
	  bmap,
	  StringMap.add agent_id agent id_to_type,
	  StringMap.add agent agent_id type_to_id)
	species
	bmap 
    in
    StringMap.fold 
      (fun x bmap sol ->
	      (*replace each attribute map with the new view id *)
	let stringblist = (x,List.sort compare (BMap.fold (fun b bool l -> (b,bool)::l) bmap []))
	in
	(
	let view_id = 
	  try 
	    StringBListMap.find stringblist views_data_structures.blist_to_template
	  with Not_found -> (* the view does not exist : this is an error *)
	    unsafe_frozen 
	      None 
	      None 
	      None 
	      (Some "line 1827") 
	      (fun () ->
		let _ = Printf.fprintf stdout "Rule: %s \n" rule_id in
		let _ = 
		  StringMap.iter 
		    (fun _ a ->  
		      let _ = Printf.fprintf stdout "Piece: %d \n" a in 
		      let view = view_of_tp_i a views_data_structures.interface_map in 
		      let _ = Printf.fprintf stdout "%s" (agent_of_view view) in
		      let _ = 
			List.iter
			  (fun (b,bool) -> 
			    let _ = print_b (ode_handler.b_of_var b) in 
			    let _ = 
			      Printf.fprintf 
				stdout 
				"%s" 
				(if bool then "T" else "F") 
			    in ())
			  (valuation_of_view view)  
		      in 
		    let _ = print_newline () 
		    in ())
		    species in
		let _ = Printf.fprintf stdout "%s" (fst (stringblist)) in
		let _ = 
		  List.iter 
		    (fun (a,b) -> print_b a;
		      Printf.fprintf stdout 
			"%s"
			(if b then "true" else "false"))
		    (snd stringblist) in
		1)
	in
	StringMap.add (StringMap.find x type_to_id) view_id sol))
      bmap StringMap.empty
  end

let merge  = StringMap.merge

type hash = string list StringMap.t

let empty_hash = StringMap.empty 

let check_compatibility data_structure hash subspecies = 
  let l = 
    StringMap.fold 
      (fun _ a sol -> a::sol) 
      subspecies [] in 
  let rec aux hash' l = 
    match l with [] -> hash',true 
    | t::q -> 
	let view = view_of_tp_i t data_structure.interface_map in
	let int = 
	  StringSet.fold
	    (fun a b -> a::b)
	    (interface_of_view view).kept_sites
	    []
	in
	let agent = agent_of_view view in 
	try 
	  if 
	    StringMap.find agent hash = int
	  then 
	    aux hash' q
	  else
	    hash,false
	with 
	  Not_found -> 
	    aux 
	      (StringMap.add agent int hash)
	      q 
  in
  aux hash l


let is_agent_in_species ag sp = 
  try 
    let _ = StringMap.find ag sp in
    true 
  with 
    Not_found -> 
      false

let add_bond_to_subspecies x _ _ = x 

module FragMap = Map2.Make (struct type t = fragment let compare = compare end)
end:Fragments)
