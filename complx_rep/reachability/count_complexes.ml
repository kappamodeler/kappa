(* 06/06/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Complexes numbering *)
(* count_complexes.ml *)

open Tools
open Data_structures 
open Pb_sig
open Config_complx
open Concretization


type binding = string*string*string*string

type bint = Big_int.big_int
let zero = Big_int.zero_big_int
let un = Big_int.unit_big_int
let plus = Big_int.add_big_int
let mult = Big_int.mult_big_int 
let div = Big_int.div_big_int
let minus = Big_int.sub_big_int 

let print_binding (a,b,c,d) = 
  print_string a;
  print_string b;
  print_string c;
  print_string d;
  print_newline () 

module BindingSet= Set.Make (struct type t = binding let compare = compare end)
module BindingMap = Map.Make (struct type t = binding let compare = compare end)
module InterfaceMap = Map.Make (struct type t = binding list let compare = compare end)
module String2Set = Set.Make (struct type t = string*string let compare = compare end)

let binding_list_of_set x = 
  List.sort compare 
    (BindingSet.fold 
    (fun x sol -> x::sol)
       x [])
    
let binding_set_of_list x = 
  List.fold_left 
    (fun sol x -> BindingSet.add x sol)
    BindingSet.empty x
    
let remove_binding list b = 
  let rec aux list rep = 
    match list with 
      t::q when t=b -> 
	List.rev (List.fold_left (fun sol t -> t::sol) rep q)
    | t::q -> aux q (t::rep)
    | [] -> raise Exit
  in aux list []

type structure = 
    {
    pieces: (String2Set.t *bint*bint) InterfaceMap.t  ;
    pieces_of_binding: binding list list BindingMap.t ;
    unary: binding list;
    self: (string*string) list;  
    } 


   
let borne_sup structure = 
  let degree,agents = 
    InterfaceMap.fold 
      (fun l _ (a,agents) -> 
	 (max 
	    a 
	    (List.length l),
	  List.fold_left  
	    (fun agents (a,_,b,_)  -> 
	       StringSet.add a 
		 (StringSet.add b agents)) agents l))
      structure.pieces 
      (0,StringSet.empty) in
  let nagents = 
    StringSet.fold
      (fun _ x -> x+1) agents 0 in
  let rec bound n k = 
    if k=0 then un  else 
      let bbound = bound n (k-1) in 
        plus (mult (Big_int.big_int_of_int n) bbound) un 
  in bound degree (3*nagents)
     
	 
  
let print_structure x = 
  print_string "PIECES \n";
  InterfaceMap.iter
    (fun int (_,n,_) -> 
      print_string "Interface:\n";
      List.iter print_binding int;
      print_string "OCC:";
      print_string (Big_int.string_of_big_int  n);print_newline ())
    x.pieces;
  print_string "CHAINING\n";
  BindingMap.iter 
    (fun b l -> 
      print_string "LINK";
      print_binding b;
      List.iter 
	(fun x -> List.iter print_binding x;print_newline ())
	l)
    x.pieces_of_binding;
  print_string "UNARY";
  List.iter print_binding x.unary 


let empty = 
  {pieces = InterfaceMap.empty;
    pieces_of_binding = BindingMap.empty;
    unary = [];
  self = [] }

let add_piece interface (black,weigth,size) structure = 
  try (
    let oldb,oldw,olds = InterfaceMap.find interface structure.pieces in
    {structure with 
      pieces = InterfaceMap.add interface 
(String2Set.union black oldb,plus oldw weigth,Big_int.max_big_int size olds) structure.pieces}
      )
  with Not_found -> 
    let unary,self = 
      match interface with 
	[a,sa,b,sb] when a=b && sa = sb -> 
	  structure.unary,(a,sa)::structure.self
      |	[x] -> x::structure.unary,structure.self 
      |	_ -> structure.unary,structure.self in
    {pieces = InterfaceMap.add interface (black,weigth,size) structure.pieces ;
     pieces_of_binding = 
      List.fold_left
	(fun sol binding -> 
	  let old = 
	    try BindingMap.find binding sol
	    with Not_found -> [] in 
	  BindingMap.add binding (interface::old) sol)
	structure.pieces_of_binding 
	interface ;
      unary = unary;
      self = self}


let remove_piece interface structure = 
  {structure with pieces = InterfaceMap.remove interface structure.pieces}
		 

let build_structure specie_map = 
  let rec aux k sol = 
    if k=0 then sol
    else 
      let gspec = IntMap.find k specie_map.g_species in 
      let a = gspec.name in 
      let interface_list = 
	List.fold_left 
	  (fun sol1 site_a -> 
	    fst (List.fold_left
	      (fun (sol2,seen) (kb,site_b) -> 
		 if specie_map.good_binding (k,site_a) (kb,site_b) then 
		   let b = (IntMap.find kb specie_map.g_species).name  in 
		     if String2Set.mem (b,site_b) seen then 
		       (sol2,seen)
		     else 
		       (List.fold_left 
			  (fun sol2 pref -> ((a,site_a,b,site_b)::pref)::sol2)
			  sol2 sol1
			  ,(String2Set.add (b,site_b) seen))
		 else (sol2,seen))
	      ([],String2Set.empty)
	      (specie_map.which_binding (k,site_a))))
	  [[]] gspec.linkable_site in 
      aux (k-1) 
	(List.fold_left 
	(fun sol x -> 
	  add_piece (List.sort compare x) 
	    (String2Set.empty,Big_int.big_int_of_int gspec.weigth,un) sol) sol interface_list)
  in
  aux specie_map.number empty
	

let count x  = 
  try (let rep = build_structure x in
  let borne = borne_sup rep in
  let rec aux rep = 
    (*print_structure rep;
    print_newline ();*)
    match rep.unary with 
      [] -> rep
    | a::q -> 
	let rep' = {rep with unary = q} in
	let rep' = remove_piece [a] rep' in
	let (x,site_x,y,site_y) = a in
	let black,weigth,size = 
	  try (InterfaceMap.find [a] rep.pieces)
	      with Not_found -> (String2Set.singleton ("","")),zero,zero in 
	if String2Set.mem (x,site_x) black then raise Exit 
	else 
	  let rep'' = 
	    if x=y && site_x=site_y then 
	      add_piece [] 
		(black,
		 plus 
		   (div (mult weigth (minus weigth un))
		      (plus un un))
		   weigth,
		 plus size size) 
		rep'
	    else 
	      List.fold_left
		(fun rep'' interface -> 
		  let black',weigth',size' = 
		    try (InterfaceMap.find interface rep.pieces)
		    with Not_found -> String2Set.empty,zero,zero in
		  let size''= plus size size' in
		  if Big_int.compare_big_int size'' borne> 0 then raise Exit
		  else
		    let interface' = remove_binding interface (y,site_y,x,site_x) 
		    in
		    add_piece 
		      interface' 
		      (String2Set.add (x,site_x) black,mult weigth' weigth,
                       plus size' size) rep'')
		rep'
		(try (BindingMap.find (y,site_y,x,site_x) rep.pieces_of_binding)
		with Not_found -> [])
	  in aux rep'' 
  in
  try (let rep = aux rep in
  let rec aux2 rep = 

    List.fold_left
      (fun rep' (a,sa) -> 
	let rep' = remove_piece [a,sa,a,sa] rep' in
	let black,weigth,size = 
	  try (InterfaceMap.find [a,sa,a,sa] rep.pieces)
	      with Not_found -> String2Set.empty,zero,zero in 
	let rep'' = 
	  add_piece 
	    [] 
	    (black,plus 
	       weigth 
	       (div 
		  (mult weigth
		     (minus weigth un))
		  (plus un un)),
	     plus size size) rep'
	in rep'')
      rep rep.self in
  let rep = 
    {(aux2 rep) with self = []}
  in
(*  let _ = print_structure rep in *)
  let _,n,_ = 
    try (InterfaceMap.find [] rep.pieces) 
    with _ -> String2Set.empty,zero,zero in
    Some (Bounded (let rep = Big_int.string_of_big_int  n in (*print_string rep;print_newline ();*)rep)))
  with Not_found -> Some Unbounded)
  with Exit -> Some Unbounded 


