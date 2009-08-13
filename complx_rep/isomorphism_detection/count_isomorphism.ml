(** module to detect and count automorphisms within left hand side of rules *)

open Data_structures 
open Pb_sig 
open Tools 
open Error_handler 

let error i y = 
  unsafe_frozen 
    None
    (Some "Complx")
    (Some "count_isomorphism.ml")
    None
    (Some i)
    y

(** A view is encoding as an ordered list of association between sites and their states *)

type bound_type = Nil | Free | Bound | Typed_bound of (string*string)
type view = string * ((string*(string*bound_type)) list)

module ViewMap = Map2.Make (struct type t = view let compare = compare end) 

(**In canonic_connected components, the list of views described the views that are encounteres, starting with the smallest one (according the polymorphic function compare) and performing a depth-first visit are given in the first field. 
Any edge that are not visited in the depth-first visit are called back edges and are encoding in the second field (each edge (A,s),(A',s') is encoded twice as an association between (A,s) and (A',s') and an association between (A',s) and (A,s), agent are denoted by their position in the list (0 for the first one).
When the smallest view occurs several time, the smallest solution for the function compare is used.

This encoding is canonic, so when two connected components are isomorphic, then they are equal.*)
type canonic_connected_components = 
    {views:view list ;
     back_edges:((int*string)*(int*string)) list }

module CanMap = Map2.Make (struct type t = canonic_connected_components let compare = compare end)


(** the function split_bmap takes the map from boolean predicates to booleans) 
and returns a list of such maps (one element per connected components) *)
let split_bmap bmap = 
  let add_relation a b map = 
    let old = 
      try 
	StringMap.find a map 
      with 
	Not_found -> StringSet.empty in
    let old' = StringSet.add b old in
    StringMap.add a old' map in 
  let views_id,connection  = 
    BMap.fold 
      (fun b bool (set,map) -> 
	match b with 
	  H(a,_) | AL((a,_,_),_) | B(a,_,_) | M((a,_,_),_) -> StringSet.add a set,map 
	| L((a,_,_),(b,_,_)) -> 
	    StringSet.add a (StringSet.add b set),
	    add_relation a b (add_relation b a map)
	| _ -> set,map)
      bmap
      (StringSet.empty,StringMap.empty) in
  
  let rec visit to_visit current_connected_components found_connected_components others = 
    match to_visit 
    with
      [] ->
	begin
	  let found_connected_components = current_connected_components::found_connected_components in 
	  if StringSet.is_empty others 
	  then found_connected_components 
	  else 
	    let min = StringSet.min_elt others in
	    visit [min] [] found_connected_components others
	end
    | t::q when StringSet.mem t others -> 
	let next = 
	  try 
	    StringMap.find t connection 
	  with
	    Not_found -> StringSet.empty in
	let next = StringSet.inter next others in 
	let to_visit = 
	  StringSet.fold
	    (fun a l -> a::l)
	    next q 
	in
	visit 
	  to_visit 
	  (t::current_connected_components)
	  found_connected_components
	  (StringSet.remove t others)
    | t::q -> 
	visit q current_connected_components found_connected_components others
  in
  let classes = visit [] [] [] views_id in 
  let give_rep = 
    List.fold_left 
      (fun map l -> 
	match l with 
	  [] -> map
	| [t] -> StringMap.add t t map 
	| t::_ -> 
	    List.fold_left 
	      (fun map t' -> StringMap.add t' t map)
	      map l)
      StringMap.empty classes in
  let map = 
    BMap.fold 
      (fun b bool map -> 
	match b with 
	  H(a,_) | B(a,_,_) | AL((a,_,_),_) | L((a,_,_),_) | M((a,_,_),_) -> 
	    let rep = 
	      try 
		StringMap.find a give_rep 
	      with 
		Not_found -> 
		  error "96" (fun () -> a) in
	    let old = 
	      try 
		StringMap.find rep map
	      with 
		Not_found -> 
		  BMap.empty
	    in
	    StringMap.add rep (BMap.add b bool old) map 
	| _ -> map)
      bmap StringMap.empty in
  StringMap.fold
    (fun _ b l -> 
      if 
	BMap.fold 
	  (fun a b sol -> 
	    match a,b with H(_),false -> sol 
	    | _ -> true) b false then   b::l else l )
    map 
    []
	  

	
(** this function computes the canonic connected_component of a connected bmap. it also returns the number of automorphism *)
let cannonize_connected_bmap bmap = 
  let compute_views bmap = 
    let add_free id s map = 
      let oldmap = 
	try 
	  StringMap.find id map 
	with 
	  Not_found -> StringMap.empty in
      let old_state = 
	try 
	  StringMap.find s oldmap 
	with 
	  Not_found -> ("",Nil)
      in
      match snd old_state 
      with Free -> map
      |	Nil -> 
	  StringMap.add 
	    id
	    (StringMap.add s (fst old_state,Free) oldmap)
	    map 
      |	_ -> error "145" (fun () -> map) 
    in
    let add_bound id s map = 
      let oldmap = 
	try 
	  StringMap.find id map 
	with 
	  Not_found -> StringMap.empty in
      let old_state = 
	try 
	  StringMap.find s oldmap 
	with 
	  Not_found -> ("",Nil)
      in
      match snd old_state 
      with Bound | Typed_bound _ -> map
    |	Nil -> 
	  StringMap.add 
	  id
	    (StringMap.add s (fst old_state,Bound) oldmap)
	    map 
      |	_ -> error "165" (fun () -> map) 
    in
    let add_typed_bound id s target map = 
      let oldmap = 
	try 
	  StringMap.find id map 
	with 
	  Not_found -> StringMap.empty in
      let old_state = 
	try 
	  StringMap.find s oldmap 
	with 
	  Not_found -> ("",Nil)
      in
      match snd old_state 
      with Typed_bound s' when s'=target -> map
      |	Nil | Bound -> 
	  StringMap.add 
	    id
	    (StringMap.add s (fst old_state,Typed_bound target) oldmap)
	    map 
      |	_ -> error "186" (fun () -> map) 
    in
    let add_mark id s mark map = 
      let oldmap = 
	try 
	  StringMap.find id map 
	with 
	  Not_found -> StringMap.empty in
      let old_state = 
	try 
	StringMap.find s oldmap 
	with 
	  Not_found -> ("",Nil)
      in
      match fst old_state 
      with "" -> 
	StringMap.add 
	  id
	  (StringMap.add s (mark,snd old_state) oldmap)
	  map 
      |	s when s=mark ->  map
      |	_ -> error "208" (fun () -> map)
    in
    let add_binding k1 k2 binding = 
      let add k1 k2 map = 
	let old = 
	  try 
	    StringMap.find (fst k1) binding 
	  with 
	    Not_found -> StringMap.empty in
	StringMap.add 
	  (fst k1) 
	  (StringMap.add 
	     (snd k1)
	     (k2)
	     old)
	  binding in 
      add k1 k2 (add k2 k1 binding) in
    let view_map,binding,here  = 
      BMap.fold 
	(fun b bool (map,binding,here) -> 
	  match b,bool with 
	    B(id,a,s),true -> add_bound id s map,binding,here
	  |	B(id,a,s),false -> add_free id s map,binding,here
	  |	M((id,a,s),m),true -> add_mark id s m map,binding,here
	  |	AL((id,a,s),tb),true -> add_typed_bound id s tb map,binding,here
	  |	L((id,a,s),(id',a',s')),true  -> 
	      add_typed_bound id s (a',s') 
		(add_typed_bound id' s' (a,s) 
		   map),add_binding (id,a) (id',s') binding,here
	  | H(id,a),true  -> map,binding,StringMap.add id a here
	  |  _ -> map,binding,here
		) 
	bmap 
	(StringMap.empty,StringMap.empty,StringMap.empty)
    in 
    let view_map = 
      StringMap.map 
	(fun map -> 
	  List.rev 
	    (StringMap.fold 
	       (fun s st l -> (s,st)::l)
	       map []))
	view_map in
    let view_map = 
      StringMap.map2 
	(fun a b  -> ("",[]))
	(fun a c  -> (c,[]))
	(fun a b c  -> (c,b))
	view_map 
	here 
    in 
    view_map,binding  in 
  let hash_view view_map = 
    StringMap.fold 
      (fun a l hash -> 
	if l = ("",[]) 
	then hash 
	else
	let old,i = 
	  try 
	    ViewMap.find l hash 
	  with Not_found -> ([],0) 
	in 
	ViewMap.add l ((a::old),i+1) hash)
      view_map
      ViewMap.empty in 
  let best hash = 
    if ViewMap.is_empty hash 
    then ([],1)
    else 
      ViewMap.fold 
	(fun view (list,n) (list',n') -> 
	  if n<n' or ((n=n') && compare list list' < 0) 
	  then (list,n)
	  else (list',n'))
	hash  
	(snd (ViewMap.min_binding hash ("",[]) ([],0))) in 
  let visit contact root = 
    let rec aux stack black visited back_edges = 
      match stack with 
	[] -> List.rev visited,back_edges 
      |	(a,s)::q -> 
	  let contact = try StringMap.find a contact with Not_found -> StringMap.empty in 
	  let contact = StringMap.remove s contact in 
	  let stack,back_edge,black  = 
	    StringMap.fold 
	      (fun s (a',s') (stack,back_edges,black) -> 
		if StringSet.mem a black 
		then stack,((a,s),(a',s'))::back_edges,black
		else (a',s')::stack,back_edges,StringSet.add a' black)
	      contact
	      (q,back_edges,black) in
	  aux stack black (a::visited) back_edges
    in 
    aux [root,""] (StringSet.singleton root) [] [] in
  let rename (a,b) = 
    let sigma,n  = 
      List.fold_left 
	(fun (map,i) a -> StringMap.add a i map,i+1)
	(StringMap.empty,0) a 
    in
    let sigma x = 
      try 
	StringMap.find x sigma 
      with 
	Not_found -> 
	  error "331" (fun () -> -1)
    in
    (a,List.sort compare (List.map (fun ((a,b),(c,d)) -> ((sigma a,b),(sigma c,d))) b)) 
  in
  let view_map,binding  = compute_views bmap in  
  let hash = hash_view view_map in 
  let (best,n)  = best hash in
  let list = List.map (visit binding) best in 
  let list = List.map rename list in 
  let list = 
    List.map 
      (fun (view,back_edge) -> 
	List.map 
	  (fun id -> 
	    try 
	      StringMap.find id view_map
	    with 
	      Not_found -> print_string id;print_newline ();error "358" (fun () -> "",[])) view,back_edge) 
      list in 
  let list = List.sort compare list in
  let n,(view,back_edge) = 
    match list with 
      [] -> 0,([],[])
    | t::q -> 
	let rec aux l n = 
	  match l 
	  with t'::q when t=t' -> aux q (n+1)
	  | _ -> n,t
	in 
	aux q 1 in
  {views=view;
    back_edges=back_edge},n

let count_isomorphism_in_bmap bmap = 
  let list = split_bmap bmap in 
  let hash = 
    List.fold_left
      (fun hash a -> 
	let (key,n) = cannonize_connected_bmap a in
	let old = 
	  try 
	    CanMap.find key hash 
	  with 
	    Not_found -> [] 
	in
	CanMap.add key (n::old) hash)
      CanMap.empty
      list in
  let facto n = 
    let rec aux k rep = 
      if k=1 then rep
      else aux (k-1) (k*rep) 
    in aux n 1 in
  let power a n = 
    let rec aux k rep = 
      if k = 0 then 1 
      else if k=1 then rep
      else
	if k mod 2 = 0 then aux (k/2) (rep*rep)
	else aux (k/2) (a*rep*rep)
    in aux n a
 in  
  if CanMap.is_empty hash then 1  (* #aut of empty lhs is equal to 0, but we put 1 to prevent division by 0 later in the program (simulation/compression) *)
 
  else
    CanMap.fold 
      (fun _ l rep -> 
	match l with [] -> error "154" (fun () -> rep)
	|	t::q -> 
	    let a = List.length l in 
	  (power t a)*((facto a)*rep)
	      )
      hash 
      1

let count_isomorphism rule = 
  let bmap = 
    List.fold_left 
      (fun map (b,bool) -> 
	BMap.add b bool map)
      BMap.empty 
	rule.injective_guard  in 
  count_isomorphism_in_bmap bmap


let get_id r = r.Pb_sig.r_simplx.Rule.id 

  
let count_isomorphism_in_rule_system pb boolean_encoding log = 
  let a,b = 
    List.fold_left 
      (fun (sol,log) rule_class -> 
	List.fold_left 
	  (fun (sol,log) rule -> 
	    let n = count_isomorphism rule in
	    List.fold_left 
	      (fun 
		(sol,log) 
		  id 
		-> 
		  IntMap.add (get_id id) n sol,log)
              (sol,log) 
	      rule.labels)
	  (sol,log)
	  rule_class.rules 
	  )
      (IntMap.empty,log)
      boolean_encoding.system in
  {pb with automorphisms = Some a},b
    
let dump_automorphisms_in_XML channel rules rel = 
  let rule_map = 
    List.fold_left 
      (fun map a -> 
	let id = a.Rule.id in
	IntMap.add id a map)
      IntMap.empty rules 
  in 
  begin
    let print_string s = Printf.fprintf channel "%s" s in
    let print_int i = Printf.fprintf channel "%i" i in
    let _ = print_string "<Automorphisms>\n "  in 
    let print_rule s = 
      let _ = print_string "\"" in 
      let _ = print_string (string_of_int s.Rule.id) in
      let _ = print_string "\"" in () 
    in
    let _ = 
      IntMap.iter 
	(fun i n -> 
	  let rulei = IntMap.find i rule_map in
	  let _ = print_string "<Rule Id=" in
	  let _ = print_rule rulei in 
	  let _ = print_string " Automorphisms=\"" in 
	  let _ = print_int n in 
	  let _ = print_string "\"/>\n" in
	  ())
	rel in 
    let _ = print_string "</Automorphisms>\n" in 
      ()
    end

let kyn_factor_of_rule a auto = 
  try 
    Some (a.r_simplx.Rule.kinetics/.(auto a.r_simplx.Rule.id))
  with 
    _ -> None 

let compute_kyn_factor a auto = 
  match a 
  with [] -> None 
  | t::q -> 
      let a = kyn_factor_of_rule t auto in 
      if List.for_all 
	  (fun x -> (kyn_factor_of_rule x auto) = a) 
	  q 
      then 
	a
      else None 

let compute_kyn_factor2 a (auto:'a -> float) = 
  let rep = 
    match a 
    with [] -> None 
    | (t,a,b)::q -> 
	let x = compute_kyn_factor t auto in 
	if 
	  List.for_all 
	    (fun (a,b,c) -> 
              List.for_all 
	      (fun y -> (kyn_factor_of_rule y auto) = x) 
		a)
	    q 
	then 
	  x
	else None 
  in 
  rep 

