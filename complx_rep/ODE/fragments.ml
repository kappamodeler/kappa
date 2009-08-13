(** Implementation of fragments for ODE *)

open Tools 
open Config_complx
open Data_structures
open Annotated_contact_map 
open Views 
open Pb_sig
open Rooted_path 
open Fragments_sig
open Error_handler 
open Ode_print 

(** Set this boolean to true to dump more debugging information *)
let trace = false
let debug = false
let cannonical_debug = false
let merge_debug = false
let map_debug = false
let complete_debug = false
let split_debug = false
let apply_blist_debug = false
let release_debug = false
let get_denum_debug = false

let error i s = 
  unsafe_frozen None (Some "fragments.ml") s (Some ("line  "^(string_of_int i))) (fun () -> raise Exit)

module New_Fragment = 
  (struct



(** new definition of fragments, a fragments is an ordered list of views and a list of back bonds: in the list of views, the head is the view that has the smallest id, then the list give the depth-first exploration of the graph; back_bonds denote the edges that are not visited during the depth-first exploration integer denotes positions in the list starting from 0. 
In the case when there is two views with minimal indices, we chose the one that give the smallest final result, with respect to the polymorphic function compare *)
type fragment = 
    {
    views:int list ;
    back_bonds: ((int * string) * (int * string)) list   
  } 

(** definition of the empty fragment for the new type definition *)
let empty_fragment = 
  {views = [];
   back_bonds = []}

(** Pretty-print function for new fragment type *)
let print_fragment a =
  let _ = print_string "Fragments\n" in 
  let _ = print_string "Views: " in
  let _ = 
    List.fold_left 
      (fun i j -> 
	let _ = if i>0 then print_string "," in
	let _ = print_int j in
	(i+1))
      0 a.views in 
  let _ = print_newline () in 
  let _ = print_string "Back_bonds:" in
  let _ = 
    List.fold_left
      (fun bool ((i,s),(i',s')) -> 
	let _ = if bool then print_string "," in
	let _ = print_int i in
	let _ = print_string "." in 
	let _ = print_string s in 
	let _ = print_string "--" in 
	let _ = print_string s' in 
	let _ = print_string "." in 
	let _ = print_int i' in 
	true)
      false
      a.back_bonds in
  let _ = print_newline () in 
  ()




(** optional pretty print function for new fragment, it only pretty print when the compilation has been made with trace=true *)
let trace_print_fragment = 
  if trace then print_fragment 
  else (fun _ -> ())
  



type view_id = int 
type node_id = string
module NodeMap = Map2.Make (struct type t = node_id let compare = compare end)


(** new type definition for subspecies: 
      rooted paths are associated with views,
      bonds are encoded both as a list and a map,
      each bond has to be encoded in both direction *)
type subspecies = 
    {
    bonds_map: (rooted_path*site_type) SitetypeMap.t RPathMap.t;
    subspecies_views:view_id RPathMap.t;
  } 

let is_empty_species s = RPathMap.is_empty s.subspecies_views 

let iter_views_in_species f sp = 
  RPathMap.iter (fun _ -> f) sp.subspecies_views 

let iter_views f fragment = 
  List.iter f fragment.views 


let fold_bonds f species = 
  RPathMap.fold 
    (fun rp -> 
      SitetypeMap.fold
	(fun site (rp',site') -> f ((rp,site),(rp',site')))
	)
    species.bonds_map 


let iter_bonds f species = 
  RPathMap.iter
    (fun rp -> 
      SitetypeMap.iter
	(fun site (rp',site') -> f ((rp,site),(rp',site')))
	)
    species.bonds_map 

(** pretty print*)  


let print_species sp = 
  let _ = print_string "SPECIES: \n" in 
  let _ = print_string " VIEWS: \n" in 
  let _ = 
    RPathMap.iter 
      (fun rp i -> 
	print_string "  ";
	print_rpath rp;
	print_string ":";
	print_int i;
	print_newline ())
      sp.subspecies_views in
  let _ = print_string " BONDS: \n" in
  let _ = 
    iter_bonds 
      (fun ((rp,s),(rp',s')) -> 
	print_string "  ";
	print_rpath rp;
	print_string ".";
	print_string s;
	print_string "--";
	print_string s';
	print_string ".";
	print_rpath  rp';
	print_newline ())
      sp 
  in 
  let _ = print_newline () in 
  ()
    
(** empty species *)
let empty_species = 
  {
(*  bonds=[];*)
  bonds_map=RPathMap.empty;
  subspecies_views=RPathMap.empty
}

(** to add a bond within a subspecies *)
let add_bond_to_subspecies subspecies (rp,s) (rp',s') = 
  let bonds_map = 
    let fadd a b c map = 
      let oldmap = 
	try 
	  RPathMap.find a map 
	with 
	  Not_found -> 
	    SitetypeMap.empty 
      in
      RPathMap.add a (SitetypeMap.add b c oldmap) map 
    in
    fadd rp s (rp',s') 
      (fadd rp' s' (rp,s) subspecies.bonds_map) in
  {subspecies 
  with bonds_map = bonds_map} 

let release_bond_from_subspecies subspecies (rp,s) (rp',s') = 
  let _ = 
    if release_debug 
    then 
      begin
	print_string "RELEASE BOND: \n";
	print_rpath rp;
	print_string "\n";
	print_string s;
	print_string "\n";
	print_rpath rp';
	print_string "\n";
	print_string s';
	print_string "\n";
	print_species subspecies 
      end in
   let bonds_map = 
    let update a b map = 
      let oldmap = 
	try 
	  RPathMap.find a map 
	with 
	  Not_found -> 
	    SitetypeMap.empty 
      in
      let site_map = SitetypeMap.remove b oldmap in 
      if SitetypeMap.is_empty site_map 
      then 
	RPathMap.remove a map
      else 
	RPathMap.add a (site_map) map 
    in
    update rp s 
      (update rp' s'  subspecies.bonds_map) in
   let rep =  {subspecies with bonds_map = bonds_map} in
   let _ = 
     if release_debug 
     then 
       begin
	 print_string "RELEASE BOND RESULT: \n";
	 print_species rep
       end in
   rep

let fetch_partner subspecies (rp,s) = 
  try 
    Some 
      (SitetypeMap.find 
	 s 
	 (RPathMap.find rp subspecies.bonds_map
	    )
	 )
  with 
    Not_found -> None 
      

(** to add a view to a subspecies *)
let add_view_to_subspecies subspecies rp view = 
  let subspecies = 
    {subspecies 
    with subspecies_views = RPathMap.add rp view subspecies.subspecies_views} in
  match rp.path 
  with [] -> subspecies
  | t::q -> 
      let ((_,s),(_,s')) = t in 
      add_bond_to_subspecies subspecies (rp,s) ({rp with path = q},s')




(** compute the cannonical fragment associated with a subspecies *)
let canonical_fragment_of_subspecies graph  = 
  let _ = 
    if cannonical_debug 
    then 
      begin
	print_string "START CANONICAL_FRAGMENT\n";
	print_species graph;
      end
  in
  let result = 
    RPathMap.fold 
      (fun path i key ->
	match key 
	with None -> Some ([path],i)
	| Some (path',i') ->
	    begin 
	      match compare i i' with 
	      -1 -> Some ([path],i)
	    |  0 -> Some (path::path',i')
	    |  1 -> Some (path',i')
	    |  _ -> error 46 None
	    end)
      graph.subspecies_views 
      None 
  in 
  let path,key = 
    match result with 
      None -> error 168 None
    | Some (a,b) -> a,b in 
  let candidate = 
    List.map 
      (fun path  -> 
	let rec vide working_list n black_list sol =
	  match working_list with 
	    [] -> sol
	  | (bond,t)::q ->
	      try 
		let _ = RPathMap.find t black_list in
		vide q n black_list sol 
	      with Not_found -> 
		let black_list = RPathMap.add t n black_list in 
		let edges = 
		  try 
		    RPathMap.find t graph.bonds_map 
		  with 
		    Not_found -> SitetypeMap.empty 
		in
		let edges = 
		  match bond 
		  with None -> edges
		  | Some (_,_,s') -> SitetypeMap.remove s' edges 
		in 
		let working_list',sol' = 
		  SitetypeMap.fold
		    (fun s (a',s') (wl,sol) ->
		      try 
			let n' = RPathMap.find a' black_list in 
			(wl,{sol with back_bonds = ((n,s),(n',s'))::sol.back_bonds})
		      with
			Not_found ->
			  (Some (t,s,s'),a')::wl,sol)
		    edges (q,sol) in 
		let sol' = 
		  {sol' with 
		    views = 
		    (try 
		      RPathMap.find t graph.subspecies_views::sol'.views
		    with 
		      Not_found -> 
			begin
			  print_string "ERROR 311\n";
			  RPathMap.iter
			    (fun rp v -> 
			      (try
				(let _ = RPathMap.find rp graph.subspecies_views in ()) 
			      with 
				Not_found -> print_string "!!!ERROR");
			      print_rpath rp;
			      print_string ":";
			      print_int v;
			      print_string ";";
			      print_newline ())
			    graph.subspecies_views;
			  print_species graph;
			  print_newline ();
			  print_rpath t;
			  print_string ";";
			  print_newline ();
			  print_rpath path;
			  print_string ";";
			  print_newline ();
			  error 311 None
			end)} in
		vide working_list' (n+1) black_list sol'
	in
	let sol = vide [None,path] 0 RPathMap.empty empty_fragment in 
	let sol = {back_bonds = List.rev sol.back_bonds;
                   views = List.rev sol.views} in 
	sol)
      path in 
  let rec aux a b n = 
    match a with 
      (t:fragment)::q -> 
	trace_print_fragment t;
	if compare t b <0 
	then aux q t 1
	else if compare t b = 0 
	then aux q b (n+1)
	else aux q b 1 
    | [] -> (b,n) in
  let sol = 
    match candidate with t::q -> (trace_print_fragment t;aux q t 1)
    | [] -> error 105 None 
  in
  let _ = 
    if cannonical_debug 
    then 
      print_string "END_CANONICAL\n"
  in
  sol 
	
(*
(* TEST *)
let _ = 
  if debug then 
    let inv (a,b) = (b,a) in
    let g b = {path = b;root= ""} in
    let a = 
      {(*embedding=
	  StringMap.add "1" []
	  (StringMap.add "2" [("2","a"),("1","a")]
	  (StringMap.add "3" [("3","a"),("1","b")]
	  (StringMap.add "4" [("4","b"),("2","b");("2","a"),("1","a")]
	  StringMap.empty))) ;
	  reverse_embedding = 
	  PathMap.add  [] "1"
	  (PathMap.add  [("2","a"),("1","a")] "2"
	  (PathMap.add  [("3","a"),("1","b")] "3" 
	  (PathMap.add [("4","b"),("2","b");("2","a"),("1","a")] "4"
	  PathMap.empty))) ;*)
     (* bonds = 
      [(g [],"a"),(g [("2","a"),("1","a")],"a") ;
	(g [],"b"),(g [("3","a"),("1","b")],"a") ;
	(g [("2","a"),("1","a")],"b"),(g [("1","a"),("2","b");("2","a"),("1","a")],"b"); 
	(g [("3","a"),("1","b")],"b"),(g [("1","a"),("2","b");("2","a"),("1","a")],"a");
	inv ((g [],"a"),(g [("2","a"),("1","a")],"a")) ;
	inv ((g [],"b"),(g [("3","a"),("1","b")],"a")) ;
     inv ((g [("2","a"),("1","a")],"b"),(g [("1","a"),("2","b");("2","a"),("1","a")],"b")); 
	inv ((g [("3","a"),("1","b")],"b"),(g [("1","a"),("2","b");("2","a"),("1","a")],"a"))
	  
      ] ;*)
      bonds_map = 
      RPathMap.add (g [])
	(SitetypeMap.add 
	   "a" (g [("2","a"),("1","a")],"a")
	   (SitetypeMap.add 
	      "b" (g [("3","a"),("1","b")],"a")
	      SitetypeMap.empty))
	(RPathMap.add (g [("2","a"),("1","a")])
	   (SitetypeMap.add 
	    "a" (g [],"a")
	      (SitetypeMap.add 
		 "b" (g [("1","a"),("2","b");("2","a"),("1","a")],"b")
		 SitetypeMap.empty))
	   (RPathMap.add (g [("3","a"),("1","b")])
	      (SitetypeMap.add 
		 "a" (g [],"b")
		 (SitetypeMap.add
		    "b" (g [("1","a"),("2","b");("2","a"),("1","a")],"a")
		    SitetypeMap.empty))
	      (RPathMap.add 
		 (g [("1","a"),("2","b");("2","a"),("1","a")])
		 (SitetypeMap.add 
		    "b" (g [("2","a"),("1","a")],"b")
		    (SitetypeMap.add 
		       "a" (g [("3","a"),("1","b")],"b")
		       SitetypeMap.empty))
		 RPathMap.empty)))
	;
      subspecies_views = 
      RPathMap.add  (g []) 1
	(RPathMap.add  (g [("2","a"),("1","a")]) 2
	   (RPathMap.add  (g [("3","a"),("1","b")]) 3 
	      (RPathMap.add (g [("1","a"),("2","b");("2","a"),("1","a")]) 1
		 RPathMap.empty))) } in 
    let _ = print_subspecies a in
    let b = canonical_fragment_of_subspecies a in
    let _ = print_fragment b in ()
*)
(* END OF TEST*)  
 


(** emptyness test for fragments *)
let is_empty_fragment x = x.views=[]


(** the following map shift the roots of a subspecies:
      a function defined the new address (as a rooted path) of some former root,
      the address of child of this root is also updates *)
let shift_subspecies subspecies shift =
  let shift rp = 
    try 
      let rp' = StringMap.find rp.root shift
      in
      {rp' with path=rp.path@rp'.path}
    with
	Not_found -> rp in 
  {
  bonds_map = 
  RPathMap.fold 
    (fun rp site_map bonds_map -> 
      let srp = shift rp in 
      RPathMap.add 
	srp 
	(SitetypeMap.map 
	   (fun (rp',s') -> shift rp',s')
	   site_map)
	bonds_map)
    subspecies.bonds_map 
    RPathMap.empty;
    
    subspecies_views = 
    RPathMap.fold 
      (fun a b map -> 
	let a' = shift a in
	if a = a' then map 
	else 
	  let image = RPathMap.find a map in
	  let map = RPathMap.remove a map in
	  let map = RPathMap.add a' image map in 
	  map)
      subspecies.subspecies_views 
      subspecies.subspecies_views }

let merge sp1 sp2 = 
  let _ = 
    if merge_debug
    then 
      begin
	print_string "MERGE \n";
	print_species sp1 ;
	print_species sp2 
      end
  in
  let sp = 
    {bonds_map = 
      RPathMap.map2 
	(fun _ b -> b)
	(fun _ b -> b)
      (fun _ b c ->
	SitetypeMap.map2 
	  (fun _ b -> b)
	  (fun _ b -> b)
	  (fun a b c -> if b=c then b else error 387 None)
	  b 
	  c)
	sp1.bonds_map 
      sp2.bonds_map ;
      subspecies_views = 
      RPathMap.map2 
	(fun _ b -> b)
	(fun _ b -> b)
	(fun _ b c -> if b=c then b else (error 395 None))
	sp1.subspecies_views sp2.subspecies_views } in
  let _ = 
    if merge_debug 
    then 
      begin
	print_species sp
      end
  in sp 
      


let get_denum_without_recursive_memoisation (agent_to_int_to_nlist,view_of_tp_i,ode_handler) bool = 
  (** If the boolean is true then this function associates a maximal list of compatible fragments to a bond *)
  (** If the boolean is false then this function associated a maximal list of fragments to a bond *)
  (** This function is hash consed *)
  let hash = Hashtbl.create 20001 in
  let f x = 
    try 
      Hashtbl.find hash x
    with 
      Not_found -> 
	let rec aux 
	    current (* contains a list of subspecies to expand *)
	    compatibility (* map each rooted path to an interface *)
	    sol (* list of already built subspecies *)
	    = 
	  match current with 
	    [] -> sol (*computation is over *)
	  | ([],subspecies)::q -> (*a subspecies is now complete *)
	      let compatibility' = (* here we update compatibility map *)
		if not bool then compatibility 
		else 
		  RPathMap.fold 
		    (fun rpath tp_i comp -> 
		      let view = view_of_tp_i tp_i in 
		      let interface = interface_of_view view in 
		      if (* is there a conflict *)
			try 
			  RPathMap.find rpath comp <> interface 
			with 
			  Not_found -> false 
		      then 
			error 536 (Some "Incompatible interfaces in agents")
		      else
			RPathMap.add rpath interface comp)
		    subspecies.subspecies_views 
		    compatibility 
	      in 
	      aux q compatibility' (subspecies::sol) 
	  | (((a,s,a',s'),black,rpath)::b,subspecies)::q ->
             (* a species to be extended *)
	      let _ = 
		if StringSet.mem a black 
		then 
		  error 47 (Some "Infinite number of fragments") 
	      in
	      let black' = StringSet.add a black in 
	      let ag1,s1,ag2,s2 = (a,s,a',s') in 
	        (* a is the new agent, a' belong to the subspecies *)
	      let rpath'= 
		{rpath 
		with path = ((a,s),(a',s'))::rpath.path} 
	      in
	      let tp = (*here is the list of all template piece for agent a containing site s*)
		try 
		  StringListMap.find 
		    [s] 
		    (StringMap.find a agent_to_int_to_nlist)
		with Not_found -> error 1135 None 
	      in
	      let _ = 
		if get_denum_debug 
		then 
		  begin 
		    print_string "TP LIST: \n";
		    List.iter (fun i -> print_int i;print_newline ()) tp ;
		    print_newline ()
		  end
	      in 
	      let tp = 
		List.filter (*filter out the one that are ot compatible*)
                            (* TO DO improve by computing directly the list when compatibility relation is already known *)
		  (fun tp -> 
		    let view = view_of_tp_i tp in 
		    let interface = interface_of_view view  in 
		    try 
		      (not bool) 
			or RPathMap.find rpath' compatibility = interface 
		    with Not_found -> true )
		  tp in 
	      let _ = 
		if get_denum_debug 
		then 
		  begin 
		    print_string "TP LIST2: \n";
		    List.iter (fun i -> print_int i;print_newline ()) tp ;
		    print_newline ();
		    print_string ag1;
		    print_string s1;
		    print_string ag2;
		    print_string s2;
		    print_newline () 
		  end
	      in 
	      let q' =
		List.fold_left
		  (fun q' n_tp -> 
		    let view = view_of_tp_i n_tp in 
		    let _ = 
		      if get_denum_debug
		      then 
			let _ = print_int n_tp  in 
			let _ = print_newline () in 
  List.iter 
			  ((fun (b,bool) -> 
			    print_b (ode_handler.b_of_var b);
			    print_string (if bool then "T" else "F");
			    print_newline ()))
			  (valuation_of_view view)
		    in 
		    if 
		      let rec aux l = (* check that the view contains a bonds *)                                      (* TODO hash cons the function between bonds and views compatible with this bond *) 
			match l with [] -> false
			| t::q -> 
			    begin
			      match ode_handler.b_of_var (fst t),snd(t) with AL((x,y,z),(t,u)),bool 
				when x=ag1 && y=ag1 && z=s1 && t=ag2 && u = s2 -> bool
			      | _ -> aux q
			    end
		      in aux (valuation_of_view view)
		    then 
		      let _ = 
			if get_denum_debug then 
			  begin 
			    print_int n_tp;
			    print_newline ()
			  end
		      in 
		      let b' = 
			String4Set.fold 
			  (fun ((ag1,s1),(ag2,s2)) b' -> 
			    if ag2 = a' && s'=s2  then b'
			    else (
			      let _ = 
				if get_denum_debug
				    then 
				  begin
				    print_string ag2;
				    print_string s2;
				    print_string ag1;
				    print_string s1;
				    print_newline ()
				  end in 
				    (ag2,s2,ag1,s1),black',rpath')::b')
			  (pending_edges view) b 
		      in
		      (b',
		       add_bond_to_subspecies 
			 (add_view_to_subspecies subspecies rpath' n_tp)
			 (rpath',s) (rpath,s'))::q'
		    else q')
		  q tp in
	      aux q' compatibility sol in
	let rep = 
	  aux 
	    [[x,StringSet.empty,empty_rpath],empty_species] 
	    RPathMap.empty 
	    [] 
	in
	(Hashtbl.add hash x rep;
	 rep)
  in 
  let f x = 
    let rep = f x in
    let _ = 
      if get_denum_debug 
      then 
	let _ = print_string "GET_DENUM\n" in 
	let (a,b,c,d) = x in
	let _ = print_string a in
	let _ = print_string "." in
	let _ = print_string b in
	let _ = print_string "|" in
	let _ = print_string c in
	let _ = print_string "." in
	let _ = print_string d in
	let _ = print_newline () in 
	let _ = List.iter (fun x -> print_string "SPECIES:";print_species x;print_newline ();print_string "-----\n") rep 
	in () 
    in rep
  in f



let get_denum_with_recursive_memoization 
    ((agent_to_int_to_nlist:Views.views_id list StringListMap.t StringMap.t),
     (view_of_tp_i:(Views.views_id -> 'a Views.views)),
     (ode_handler:('b,'a,'c,'d,'e,'f,'g) ode_handler)) level bool = 
  (** If the boolean is true then this function associates a maximal list of compatible fragments to a bond *)
  (** If the boolean is false then this function associated a maximal list of fragments to a bond *)
  (** This function is hash consed *)
  let hash = Hashtbl.create 200001 in
  let rec fetch (x:Pb_sig.name_specie*string*string*string) = 
    try Hashtbl.find hash x 
    with 
      Not_found -> 
	let rep = compute x in 
	let _ = 
	  if level > 0 
	  then Hashtbl.add hash x rep 
	in 
	rep 
  and
      compute (a,s,a',s') = 
    let fetch = if level = 2 then fetch else compute in 
    let ag1,s1,ag2,s2 = (a,s,a',s') in 
    let _ = 
      if get_denum_debug 
      then 
	begin 
	  print_string "COMPUTE\n";
	  print_string ag1;
	  print_string s1;
	  print_string ag2;
	  print_string s2;
	  print_string " ";
	  print_string (if bool then "TRUE\n" else "FALSE\n");
	end in 
    let tp_list = (*here is the list of all template piece for agent a containing site s*)
      try 
	StringListMap.find 
	  [s] 
	  (StringMap.find a agent_to_int_to_nlist)
      with Not_found -> error 544 None 
    in
    let _ = 
      if get_denum_debug 
      then 
	begin 
	  print_string "ALL POTENTIAL PARTNER\n";
	  List.iter 
	    (fun i -> print_int i;print_string " ")
	    tp_list;
	  print_newline ();
	end
    in 
    let tp_list = 
      if bool (*dealing with compatibility *)
      then 
	 let classe = (*select a class of sites for the agent *)
	   try 
	     interface_of_view (view_of_tp_i (List.hd tp_list)) 
	   with 
	     _  -> error 550 None
	 in 
	 List.filter (*filter out the one that are ot compatible*)
                  (* TO DO improve by computing directly the list when compatibility relation is already known *)
	   (fun tp -> 
	     let view = view_of_tp_i tp in 
	     (interface_of_view view  = classe)
	       )
	   tp_list 
      else tp_list in 
    List.fold_left 
      (fun liste tp -> 
	let view = view_of_tp_i tp in 
	
	 if 
	   let rec aux l = (* check that the view contains a bonds *)                                      (* TODO hash cons the function between bonds and views compatible with this bond *) 
	     match l with [] -> false
	     | t::q -> 
		 begin
		   match ode_handler.b_of_var (fst t),snd(t) with AL((x,y,z),(t,u)),bool 
		     when x=ag1 && y=ag1 && z=s1 && t=ag2 && u = s2 -> bool
		   | _ -> aux q
		 end
	   in aux (valuation_of_view view)
	 then (* the view has to be kept *)
	   let _ = 
	     if get_denum_debug then 
	       begin 
		 print_int tp;
		 print_newline ()
	       end
	   in 
	   let pending_bonds = 
	     String4Set.fold 
	       (fun ((ag1,s1),(ag2,s2)) pending_bonds -> 
		 if ag2 = a' && s'=s2  
		 then pending_bonds 
		 else (
		   let _ = 
		     if get_denum_debug
		     then 
		       begin
			 print_string ag2;
			 print_string s2;
			 print_string ag1;
			 print_string s1;
			 print_newline ()
		       end in 
		   (ag2,s2,ag1,s1))::pending_bonds)
	       (pending_edges view) []
	   in
	   let rpath = 
	     {empty_rpath 
	     with path = [(a,s),(a',s')]} in 
	   let species = add_view_to_subspecies empty_species rpath tp in 
	   let _ = 
	     if get_denum_debug 
	     then 
	       print_species species 
	   in 
	   let species_list = 
	     List.fold_left 
	       (fun prefix bond -> 
		 let (a,s,a',s') = bond in 
		 let rpath'= 
		   {rpath with 
		     path = ((a,s),(a',s'))::rpath.path} in 
		 let extension = fetch bond in 
		 List.fold_left 
		   (fun liste extension -> 
		     List.fold_left 
		       (fun liste subspecies -> 
			 let shifted_extension = 
			   shift_subspecies 
			     extension 
			     (StringMap.add "" rpath StringMap.empty) in 
			 let ext_subspecies = merge shifted_extension subspecies in 
		       let rep  = 
			 add_bond_to_subspecies 
			   ext_subspecies
			   (rpath',s) 
			   (rpath,s') in 
		       let _ = 
			 if get_denum_debug 
			 then 
			   begin 
			     print_string "RECOMP\n";
			     print_string "BASE\n";
			     print_species subspecies;
			     print_string "\nEXT\n";
			     print_species extension;
			     print_string "\nSHIFTED EXT\n";
			     print_species shifted_extension;
			     print_string "\nPATHs\n";
			     print_rpath rpath;
			     print_string "\n";
			     print_string s;
			     print_string "\n";
			     print_rpath rpath';
			     print_string "\n";
			     print_string s';
			     print_string "\n";
			     print_string "\nRESULT\n";
			     print_species rep  ;
			     print_string "\n"
			   end 
		       in 
		       rep::liste)
		       liste prefix)
		   [] extension)
	       [species] pending_bonds
	   in 
	   let _ = 
	     if get_denum_debug 
	     then 
	       begin 
		 print_string "SPECIE LIST \n" ;
		 List.iter print_species species_list
	       end 
	   in 
		 
	   List.fold_left 
	     (fun list a -> a::list)
	     liste
	     species_list
	     else liste)
      []
      tp_list 
  in fetch 



let get_denum 
    = (fun x  -> 
      get_denum_with_recursive_memoization x 0 ,
      get_denum_with_recursive_memoization x 1 ,
      get_denum_with_recursive_memoization x 2)


let complete_subspecies (pending_edges,view_of_tp_i,keep_this_link,get_denum) subspecies = 
  (** This function takes a subspecies and build the list of the fragments that extend it *)
  let _ = 
    if complete_debug
    then
      begin
	print_string "COMPLETE: ";
	print_newline ();
	print_species subspecies 
      end in
  let target (*set of the typed site to be connected to the frontier of the subspecies by a solid line *)
	, map  (*map each target site to the rooted path of the agent it is connected to and the type of the connected site and the potential extentions of this bond *)
	= 
    RPathMap.fold
      (fun rp tp (target,map) ->
	let pending_edges = pending_edges (view_of_tp_i tp) in
	String4Set.fold 
	  (fun (y1,y2) (target,map) ->
	    if 
	      begin (*not an internal edges *)
		try 
		  let _ = 
		    SitetypeMap.find (snd y1) 
		      (RPathMap.find rp subspecies.bonds_map) in
		  true 
		with 
		  Not_found -> false 
	      end
		&& 
	      begin (*a solid edge *)
		keep_this_link y1 y2 
	      end
	    then 
	      (target,map)
	    else
	      (
	      String2Set.add y1 target,
	      (*String2Map.add y1 (rp,y2,get_denum (fst y2,snd y2,fst y1,snd y1)) map*)
               (y1,rp,y2,get_denum (fst y2,snd y2,fst y1,snd y1))::map))
	  pending_edges 
	  (target,map))
      subspecies.subspecies_views 
      (String2Set.empty,(*String2Map.empty*)[]) in 
  let _ = 
    if map_debug
    then
      begin 
	print_string "MAP:\n";
	(*String2Map.*)List.iter 
	  (fun  (y1,rp,y2,ext_list) -> 
	    print_string (fst y2);
	    print_string ".";
	    print_string (snd y2);
	    print_rpath rp;
	    print_string (fst y1);
	    print_string ".";
	    print_string (snd y1);
	    print_newline ();
	    List.iter print_species ext_list)
	  map 
      end
  in 
  
  let sol = 
    (*String2Map.fold*) List.fold_left  
      (fun sol_list (y1,rp,y2,extension_list) (*sol_list*) -> 
	let rp' = {rp with path = (*(y2,y1)::*)rp.path} in 
	List.fold_left 
	  (fun pre_sol_list extension -> 
	    List.fold_left 
	      (fun sol_list pre_sol -> 
                let extended_sol = 
		  (*add_bond_to_subspecies  *)
		    (merge
		       pre_sol 
		       (shift_subspecies 
			  extension 
			  (StringMap.add "" rp' StringMap.empty)))
		    (*(rp,snd y1) (rp',snd y2)*) in
		extended_sol::sol_list		
				)
	      pre_sol_list sol_list
	      )
	  []  extension_list 
	  
	  ) 
       [subspecies] map in 
  let _ = 
    if complete_debug 
    then 
      begin
	print_string "COMPLETE RESULT\n";
	List.iter print_species sol 
      end
  in
  sol 
    
(** this primitive split a species into a list of maximal connected subspecies *)
let split_subspecies data_structure ode_handler contact_map subspecies = 
  let _ = 
    if split_debug 
    then 
      begin 
	print_string "SPLIT \n";
	print_species subspecies 
      end in
  let rec aux to_visit current_rp_list available_rpaths list = 
    match to_visit with 
      [] -> 
	begin
	  let list' = current_rp_list::list in 
	  if 
	    RPathSet.is_empty available_rpaths
	  then
	    list'
	  else
	    let rpath = RPathSet.min_elt available_rpaths in
	    aux 
	      [rpath] 
	      []
	      available_rpaths
	      list'
	end
    | rpath::q when not (RPathSet.mem rpath available_rpaths) ->
	aux q current_rp_list available_rpaths list 
    | rpath::q -> 
	let available_rpaths' = RPathSet.remove rpath available_rpaths in 
	let q' =
	  let sitemap = 
	    try
	      RPathMap.find rpath subspecies.bonds_map
	    with 
	      Not_found -> 
		SitetypeMap.empty 
	  in
	  SitetypeMap.fold 
	    (fun _ (a,_) q' -> 
	      if RPathSet.mem a available_rpaths 
	      then
		a::q'
	      else
		q')
	    sitemap 
	    q in
	let current_rp_list' = rpath::current_rp_list in 
	aux 
	  q' 
	  current_rp_list' 
	  available_rpaths' 
	  list
  in
  let rp_set = 
    RPathMap.fold
      (fun a _ -> RPathSet.add a)
      subspecies.subspecies_views 
      RPathSet.empty in
  let _ = 
    if split_debug
    then 
      begin
	print_string "RP_SET \n";
	RPathSet.iter 
	  (fun rp -> print_rpath rp;print_newline ())
	  rp_set 
      end 
  in
  let rp_list_list = 
    if RPathSet.is_empty rp_set 
    then
      []
    else
      let min = RPathSet.min_elt rp_set  in
      aux [min] []  rp_set [] 
  in
  let hash,_ = (* hash maps each rp to a class identifier *)
    List.fold_left
      (fun (hash,id) rp_list ->
	List.fold_left
	  (fun hash rp -> 
	    RPathMap.add rp id hash)
	  hash rp_list,id+1)
      (RPathMap.empty,0)
      rp_list_list in
  let views_map = 
    RPathMap.fold
      (fun rp v map -> 
	let i = 
	  try 
	    RPathMap.find rp hash 
	  with
	    Not_found -> 
	      begin 
		print_string "ERROR:\n";
		print_rpath rp; 
		error 829 None
	      end
	in
	let old = 
	  try
	    IntMap.find i map
	  with
	    Not_found -> 
	      RPathMap.empty
	in
	IntMap.add i (RPathMap.add rp v old) map)
      subspecies.subspecies_views IntMap.empty in
  let species_map_without_bonds = 
    IntMap.map (fun list -> {empty_species with subspecies_views = list}) views_map in
  let species_map_with_bonds =
    fold_bonds
      (fun ((a,b),(c,d))  bonds_map -> 
	let i = 
	  try 
	    RPathMap.find a hash 
	  with 
	    Not_found -> 
	      error 810 None 
		
	in
	let old = 
	  try
	    IntMap.find i bonds_map 
	  with 
	    Not_found -> error 858 None 
	in
	IntMap.add i (add_bond_to_subspecies old (a,b) (c,d)) bonds_map)
      subspecies 
      species_map_without_bonds 
  in
  let sol = 
    IntMap.fold 
      (fun _ a l -> a::l) 
      species_map_with_bonds 
      [] 
  in
  let _ =
    if split_debug 
    then 
      begin
	print_string "SPLIT RESULT \n";
	List.iter
	  print_species 
	  sol
      end
  in sol 
      
let is_agent_in_species x s = 
  try
    (let _ = RPathMap.find (build_empty_path x) s.subspecies_views in true)
  with 
    Not_found -> false
	
type hash = string list RPathMap.t

let dump_hash x = 
  let _ = 
  RPathMap.iter 
    (fun rp l -> 
      print_rpath rp ;
      print_string " : ";
      List.iter print_string l;
      print_newline ())
    x in 
  print_newline () 

let empty_hash = RPathMap.empty 

let check_compatibility data_structure hash subspecies = 
  let l = 
    RPathMap.fold 
      (fun a b sol -> (a,b)::sol)
      subspecies.subspecies_views []
  in 
  let rec aux hash' l = 
    match l with [] -> hash',true 
    | (rpath,tp_i)::q -> 
	let view = view_of_tp_i tp_i data_structure.interface_map in
	let int = 
	  StringSet.fold
	    (fun site b -> site::b)
	    (interface_of_view view).kept_sites
	    []
	in
	try 
	  if 
	    RPathMap.find rpath hash = int
	  then 
	    aux hash' q
	  else
	    hash,false
	with 
	  Not_found -> 
	    aux 
	      (RPathMap.add rpath int hash')
	      q 
  in
  aux hash l
    


let apply_blist_with_species ode_handler data_structure keep_link rule_id  species blist free_sites = 
  let _ = 
    if apply_blist_debug 
    then
      begin
	print_string "APPLY BLIST \n";
	print_string " BLIST : \n";
	List.iter 
	  (fun (b,bool) -> 
	    print_string "  ";
	    print_b b;
	    print_string (if bool then "T" else "F");
	    print_newline ())
	  blist;
	print_string " Species : \n";
	print_species species 
      end in 
  let update = RPathMap.empty in
  let get a modified  = 
    try 
      Some (RPathMap.find a modified)
    with 
      Not_found ->
	try
	  let tp_i = RPathMap.find a  species.subspecies_views in
	  let view = view_of_tp_i tp_i data_structure.interface_map in
	    Some (view.agent,view.valuation_map)  

	with 
	    Not_found -> None
  in 
  (* deal with blist *)
   let update,subspecies = 
    List.fold_left 
      (fun (modified,subspecies) (b,bool) -> 
	 let f b rp  modified = 
	   let b' = downgrade_b b in 
	   let ((agent_type:string),
		(v:'a BMap.t)) = 
	     match get rp modified with 
	      None -> error 1255 (Some "update blist")
	       |Some rep -> rep 
	   in 
	   let v' = BMap.add b' bool v in
	   RPathMap.add 
	     rp 
	     (agent_type,v') 
	     modified in
	 
	 match 
	   b
	 with 
	   M((agent_id,_,_),_) when agent_id<> "" -> 
	     f b (build_empty_path agent_id) modified,
	     subspecies
	 | B(agent_id,agent_type,site) | AL((agent_id,agent_type,site),_) when agent_id <> "" -> 
	     let rp = build_empty_path agent_id in 
	     if not bool 
	     then 
	      try 
		let (rp',site') = 
		  SitetypeMap.find
		    site
		    (RPathMap.find
		       (build_empty_path agent_id)
		       species.bonds_map)
		in
		let (agent_type',v') = 
		  match get rp' modified 
		  with None -> error 1284 None 
		    |Some a -> a 
		in 
		let modified  = f (B(agent_type,agent_type,site))  rp modified in
		let modified = f (AL((agent_type,agent_type,site),(agent_type',site'))) rp modified in 
		let modified = f (B(agent_type',agent_type',site')) rp' modified in
		let modified = f (AL((agent_type',agent_type',site'),(agent_type,site))) rp' modified in
		modified,
		release_bond_from_subspecies 
		  subspecies
		  (rp,site)
		  (rp',site')
	      with 
		Not_found -> 
		  f b rp modified,subspecies 
	    else 
	       f b rp modified,subspecies 

	| L((agent_id,agent_type,site),(agent_id',agent_type',site')) -> 
	    modified,
	    (if bool 
	    then add_bond_to_subspecies 
	    else 
	      release_bond_from_subspecies)
	      subspecies 
	      ((build_empty_path agent_id),site)
	      ((build_empty_path agent_id'),site')
	     
	| _ -> modified,subspecies)
      (update,species) 
      blist 
  in 
   (* deal with side_effects due to agent removal *)
   let update,subspecies = 
     List.fold_left 
       (fun (update,subspecies) ((rp,a,s),(rp',s')) ->
	 try 
	   let rep = get rp' update in 
	     match rep 
	     with Some (agent_type,v) -> 
	       let v' = BMap.add (B(agent_type,agent_type,s')) false v in 
	       let v' = BMap.add (AL((agent_type,agent_type,s'),(a,s))) false v' in 
		 RPathMap.add 
		   rp' 
		   (agent_type,v') 
		   update,
	       subspecies
	       | None -> update,subspecies
	 with 
	   Not_found  -> update,subspecies)
       (update,subspecies) free_sites 
   in 
   let stringblist (x,bmap) = 
    let hashkey  = 
      (x,List.sort compare (BMap.fold (fun b bool l -> (b,bool)::l) bmap [])) in
    try 
      StringBListMap.find hashkey data_structure.blist_to_template 
    with
      Not_found -> 
	let _ = print_string x in
	let _ = print_newline () in 
	let _ = 
	  BMap.iter
	    (fun b bool -> 
	      print_b b;
	      if bool 
	      then 
		print_string "T\n"
	      else 
		print_string "F\n")
	    bmap in 
	    
	       error 1343 (Some  "Try to hash unknown view")
  in
  let species = 
    {subspecies 
    with subspecies_views = 
      RPathMap.fold
	(fun rp x map ->
	RPathMap.add 
	    rp
	    (stringblist x)
	    map)
	update 
	subspecies.subspecies_views
    }
  in
  let _ = 
    if apply_blist_debug 
    then
      begin
	print_string "APPLY BLIST RESULT \n";
	print_string " Species : \n";
	print_species species 
      end
  in species


let plug_views_in_subspecies agent_id view_id sp = 
  {sp 
  with 
    subspecies_views = 
    RPathMap.add (build_empty_path agent_id) view_id sp.subspecies_views}


let rec scan_list f l = 
  match l 
    with 
      [] -> None
    | t::q -> 
	begin
	  match f t 
	  with 
	    None -> scan_list f q
	  | x -> x
	end

let scan_views f fragment = 
  scan_list f fragment.views 


let scan_views_in_species f sp = 
  let views = RPathMap.fold (fun _ t q -> t::q) sp.subspecies_views [] in
  scan_list f views 

let fold_views f fragment a = 
  List.fold_left 
    (fun sol x -> f x sol)
    a 
    fragment.views 
  
let is_empty_species sp = RPathMap.is_empty sp.subspecies_views 

let get_views_from_agent_id view_map agent_id agent_type sp = 
  try 
    let view_id = 
      RPathMap.find (build_empty_path agent_id) sp.subspecies_views 
    in
    Some (view_id,view_map view_id)
  with
    Not_found -> 
      None 
let canonical_form = canonical_fragment_of_subspecies 



let build_species agent_of_views view_map extension = 
  let species = empty_species in
  let species = 
    StringMap.fold
      (fun agent_id view_id species -> 
	add_view_to_subspecies species (build_empty_path agent_id) view_id)
      view_map 
      species 
  in
  let species = 
    List.fold_left
      (fun species ((a,b),view_id) -> 
	add_view_to_subspecies species (build_rpath a b) view_id)
      species
      extension 
  in
  species 

let get_neighbour species (agent_id,site) agent_type' = 
  try 
    let (rp',s') = 
      SitetypeMap.find 
	site 
	(RPathMap.find 
	   (build_empty_path agent_id)
	   species.bonds_map
	   )
    in
    if rp'.path = [] 
    then rp'.root 
    else error 1405 None
  with 
    Not_found -> 
      error 1069 None 
    


let add_bond_to_subspecies sp (a,s) (a',s') = 
  add_bond_to_subspecies sp (build_empty_path a,s) (build_empty_path a',s') 

module FragMap = Map2.Make (struct type t = fragment let compare = compare end) 
module RootedFragMap = Map2.Make (struct type t = (rooted_path * view_id) * fragment let compare = compare end) 

module BondSet = Set.Make (struct type t = (int*string) * (int*string) let compare = compare end) 

let compute_edges fragment view_data_structure= 
  let stack = [] in 
  let views = fragment.views in
  let back_bonds = fragment.back_bonds in
  let fadd ((i,s),(i',s')) map = 
    let aux (i,s) (i',s') map = 
      let old = 
	try IntMap.find i map 
	with 
	  Not_found -> StringMap.empty
      in
      IntMap.add i (StringMap.add s (i',s') old) map in
    aux (i,s) (i',s') (aux (i',s') (i,s) map)
  in
  let occupied,bonds = 
    List.fold_left
      (fun (set,map) x -> BondSet.add x set,fadd x map)
      (BondSet.empty,IntMap.empty)
      back_bonds in
  let counter,stack,bonds = 
    List.fold_left
      (fun (i,stack,bonds) view -> 
	let view = view_of_tp_i view view_data_structure.interface_map  in 
	let target = view.Views.target in 
	let check,last,stack = 
	  match stack with 
	    [] -> None,None,[]
	  | (t,i)::q -> Some t,Some i,q in
	let bonds,stack = 
	  String2Map.fold
	    (fun (a,b) (c,d) (bonds,stack) -> 
	      if check = Some ((c,d),(a,b)) 
	      then 
		match last with 
		  None -> error 1451 None
		| Some i' -> 
		  begin 
		    fadd ((i',d),(i,b)) bonds,
		    stack
		  end
	      else
		if 
		  begin
		    try
		      let _ = 
			StringMap.find b (IntMap.find i bonds)
		      in true
		    with 
		      Not_found -> false
		  end
		then
		  bonds,stack
		else
		  bonds,(((a,b),(c,d)),i)::stack)
	    target
	    (bonds,stack)
	in
	(i+1,stack,bonds))
      (0,stack,bonds) 
      views in
  bonds 

let remove_agent_in_species ode_handler data_structure keep_link rule_id  (species,free_sites)  agent  =
  let rp = (build_empty_path agent) in 
  let get_dual_binding = 
    let map  = 
      try 
	RPathMap.find rp species.bonds_map 
      with 
	Not_found -> SitetypeMap.empty
    in 
    SitetypeMap.fold 
      (fun s (rp2,s2) bindings -> ((rp,agent,s),(rp2,s2))::bindings)
      map [] 
  in 
  let frem (a,s) map = 
    let old = try RPathMap.find a map with Not_found -> SitetypeMap.empty in 
    let new' = SitetypeMap.remove s old in 
    if SitetypeMap.is_empty new' then 
      RPathMap.remove a map 
    else
      RPathMap.add a new' map in 
  let frem ((a,_,s),(a',s')) map = 
    frem (a,s) (frem (a',s') map) in 
  let bonds_map = 
    List.fold_left 
      (fun bonds_map x -> frem x bonds_map)
      (species.bonds_map)
      get_dual_binding 
  in 
  let free_sites = 
    List.fold_left 
      (fun l x -> x::l)
      free_sites
      get_dual_binding 
  in
  let subspecies_views = RPathMap.remove rp species.subspecies_views in 
   {bonds_map = bonds_map;
    subspecies_views = subspecies_views},free_sites 

let pretty_print 
    stdprint 
    fragment 
    handler 
    ode_handler 
    views_data_structure 
    keep_this_link 
    empty 
    bool 
    = 
  let bonds = compute_edges fragment views_data_structure in 
  let fadd_free site  site_map = 
    let tuple  = 
      try (StringMap.find site site_map) 
      with Not_found -> 
	tuple_bot 
    in 
    let tuple' = {tuple with is_bound = Init false} in 
    StringMap.add site tuple'  site_map in
  let fadd_sign site s site_map = 
    let tuple = 
      try 
	StringMap.find site site_map 
      with 
	Not_found -> 
	  tuple_bot in 
    let tuple' = {tuple with mark = Init s} in 
    StringMap.add site tuple' site_map  in 
  let aux n (i,s) tuple_map = 
      let (ag,old) = 
	try
	  IntMap.find i tuple_map 
	with 
	  Not_found -> error 1511 None in 
      let tuple = 
	try 
	  StringMap.find s old
	with 
	  Not_found -> tuple_bot 
      in
      let tuple' = {tuple 
		   with link = Init (bound_of_number n) 
		   } 
      in
      IntMap.add i (ag,StringMap.add s tuple' old) tuple_map in
  let add_link (i,s) (i',s') (n,tuple_map) = 
    let tuple_map = 
      aux n (i,s) (aux n (i',s')  tuple_map)
    in
    (n+1,tuple_map)
  in
  let counter,tuple_map,stack = 
    List.fold_left 
      (fun (counter,map,stack) view_id -> 
	let view = view_of_tp_i view_id views_data_structure.interface_map  in 
	let agent = agent_of_view view in 
	let sigma = valuation_of_view view in
	let tuple,stack = 
	  List.fold_left 
	    (fun (tuple,stack) (b,bool) -> 
	      match ode_handler.b_of_var b,bool with 
		B(_,_,s),false -> fadd_free s tuple,stack
	      | M((_,_,s),mark),true -> fadd_sign s mark tuple,stack
	      |	AL((_,a,s),(b,s')),true when 
		  not (keep_this_link (a,s) (b,s')) -> 
		    tuple,(counter,s,b,s')::stack
	      | _ -> tuple,stack)
	    (StringMap.empty,stack)
	    sigma 
	in
	((counter+1),
	 IntMap.add counter (agent,tuple) map,
	 stack))
      (0,IntMap.empty,[])
      fragment.views
  in 
  let (n,tuple_map) =
    IntMap.fold
      (fun i site_map -> 
	StringMap.fold 
	  (fun s (i',s') (tuple_map) -> 
	    if compare (i,s) (i',s')<= 0 
	    then add_link (i,s) (i',s') tuple_map 
	    else 
	      tuple_map)
	  site_map)
      bonds 
      (1,tuple_map)
  in
  let (n,tuple_map,counter) = 
    List.fold_left
      (fun (n,tuple_map,counter) (i,s,b,s') -> 
	if not bool 
	then 
	let tuple = tuple_bot in 
	let tuple_map = 
	  IntMap.add 
	    counter 
	    (b,StringMap.add s' tuple StringMap.empty) 
	    tuple_map in
	let n,tuple_map = 
	  add_link 
	    (i,s) 
	    (counter,s') 
	    (n,tuple_map) in 
	n,tuple_map,counter+1
	else 
	  let ag,sitemap = 
	    try 
	      IntMap.find i tuple_map 
	    with 
	      Not_found -> 
		error 1611 None
	  in
	  let tuple = 
	    try 
	      StringMap.find s sitemap
	    with 
	      Not_found -> tuple_bot 
	  in
	  let tuple' = {tuple with link = Init(b,s')} in
	  let tuple_map = 
	    IntMap.add i (ag,StringMap.add s tuple' sitemap) tuple_map  in 
	  (n,tuple_map,counter)) 
      (n,tuple_map,counter)
      stack in
	  
  let _ = 
    IntMap.fold 
      (fun _ (ag,tuple) bool -> 
	let _ = 
	  if bool then () in 
	let pretty = StringMap.add ag  tuple StringMap.empty in 
	let l = 
	  print_pretty 
	    handler 
	    ag
	    (fun x->true)
	    ((pretty,pretty),0)
	    tuple_known
	    empty
	    (if bool then handler.agent_separator () else "")
	    (fun x->x) 
	    (fun x->x) 
	    None 
	    None in 
	let _ = 
	  List.iter 
	    (pprint_string stdprint)
	    (List.rev 
	       ((fun (_,a,_) -> a) 
	       l)) in
	true
	  )
      tuple_map false in
  () 
      
let root_of_species x = 
  match 
    RPathMap.fold
      (fun rp i sol ->
	match sol with None -> Some (rp,i) 
	| Some (rp',_) when compare rp rp'>0 -> Some (rp,i) 
	| _ -> sol)
      x.subspecies_views
      None 
  with 
    None -> error 1660 None 
  | Some i -> i

end:Fragments)
