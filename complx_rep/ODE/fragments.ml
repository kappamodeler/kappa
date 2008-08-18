(** Implementation of fragments for ODE *)

open Tools 
open Data_structures
open Views 
open Pb_sig
open Rooted_path 

(** Set this boolean to true to dump more debugging information *)
let trace = false 

let error i s = 
  unsafe_frozen None (Some "fragments.ml") s (Some ("line  "^(string_of_int i))) (fun () -> raise Exit)




(** former definition of fragments, a fragment was a list of views *)
type fragment = int list 

(** former definition of subspecies, a subspecies was a list of views *)
type subspecies = int list 

(** new definition of fragments, a fragments is an ordered list of views and a list of back bonds: in the list of views, the head is the view that has the smallest id, then the list give the depth-first exploration of the graph; back_bonds denote the edges that are not visited during the depth-first exploration integer denotes positions in the list starting from 0. 
In the case when there is two views with minimal indices, we chose the one that give the smallest final result, with respect to the polymorphic function compare *)
type canonical_fragment = 
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
	let _ = print_string ":" in
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
type new_subspecies = 
    {
    bonds:((rooted_path*site_type)*(rooted_path*site_type)) list;
    bonds_map: (rooted_path*site_type) SitetypeMap.t RPathMap.t;
    subspecies_views:view_id RPathMap.t;
  } 

(** empty species *)
let empty_species = 
  {
  bonds=[];
  bonds_map=RPathMap.empty;
  subspecies_views=RPathMap.empty
}

(** to add a bond within a subspecies *)
let add_bond_to_subspecies subspecies (rp,s) (rp',s') = 
  let bonds = ((rp,s),(rp',s'))::((rp',s'),(rp,s))::subspecies.bonds in
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
  with bonds = bonds;
    bonds_map = bonds_map} 

(** to add a view to a subspecies *)
let add_view_to_subspecies subspecies rp view = 
  {subspecies 
  with subspecies_views = RPathMap.add rp view subspecies.subspecies_views}

(** pretty print*)  
let print_path p = 
  let _ = 
    List.fold_left 
      (fun bool ((a,s),(a',s')) -> 
	let _ = if bool then print_string "/" in
	let _ = print_string a in
	let _ = print_string "." in
	let _ = print_string s in
	let _ = print_string "-" in
	let _ = print_string s' in
	let _ = print_string "." in
	let _ = print_string a' in true)
      false p in () 
    
let print_rpath p = 
  print_path p.path;
  print_string ".";
  print_string p.root

let print_new_subspecies a = 
  let _ = print_string "Template_pieces: \n "in 
  let _ = 
    RPathMap.iter 
      (fun path s  -> 
	print_int s;
	print_string ":";
	print_rpath path;
	print_newline ())
      a.subspecies_views in 
  let _ = print_string "Bonds:\n" in 
  let _ = 
    List.iter 
      (fun 
	((p,s),(p',s')) -> 
	print_rpath p;
	print_string s;
	  print_string "----";
	print_string s';
	  print_string ".";
	print_rpath p';
	  print_newline ())
      a.bonds in
  ()

(** compute the cannonical fragment associated with a subspecies *)
let canonical_fragment_of_subspecies graph  = 
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
		let edges = RPathMap.find t graph.bonds_map in
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
		      RPathMap.find t graph.subspecies_views 
		    with 
		      Not_found -> error 77 None )::sol'.views} in 
		vide working_list' (n+1) black_list sol'
	in
	let sol = vide [None,path] 0 RPathMap.empty empty_fragment in 
	let sol = {back_bonds = List.rev sol.back_bonds;
                   views = List.rev sol.views} in 
	sol)
      path in 
  let rec aux a b = 
    match a with 
      (t:canonical_fragment)::q -> 
	trace_print_fragment t;
	if compare t b <0 
	then aux q t 
	else aux q b
    | [] -> b in
  match candidate with t::q -> (trace_print_fragment t;aux q t)
  | [] -> error 105 None 
	

(* TEST *)
	
let inv (a,b) = (b,a) 
let g b = {path = b;root= ""} 
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
    bonds = 
    [(g [],"a"),(g [("2","a"),("1","a")],"a") ;
     (g [],"b"),(g [("3","a"),("1","b")],"a") ;
     (g [("2","a"),("1","a")],"b"),(g [("1","a"),("2","b");("2","a"),("1","a")],"b"); 
     (g [("3","a"),("1","b")],"b"),(g [("1","a"),("2","b");("2","a"),("1","a")],"a");
      inv ((g [],"a"),(g [("2","a"),("1","a")],"a")) ;
     inv ((g [],"b"),(g [("3","a"),("1","b")],"a")) ;
     inv ((g [("2","a"),("1","a")],"b"),(g [("1","a"),("2","b");("2","a"),("1","a")],"b")); 
     inv ((g [("3","a"),("1","b")],"b"),(g [("1","a"),("2","b");("2","a"),("1","a")],"a"))
	
    ] ;
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
	       RPathMap.empty))) }
      

let _ = print_new_subspecies a 
let b = canonical_fragment_of_subspecies a 
let _ = print_fragment b

(* END OF TEST*)  
 


(** new emptyness test for fragments *)
let new_is_empty_fragment x = x.views=[]

(** old emptyness test for fragments *) 
let is_empty_fragment x = x=[]


let concat a b = if b = [] then a else a@b


(** the following map shift the roots of a subspecies:
      a function defined the new address (as a rooted path) of some former root,
      the address of child of this root is also updates *)
let shift_subspecies subspecies shift =
  let shift rp = 
    try 
      StringMap.find rp.root shift
    with
	Not_found -> rp in 
  let bonds,modified = 
    List.fold_left
      (fun (list,modified) ((rp,s),(rp',s')) -> 
	let srp  = shift rp in 
	let srp' = shift rp' in 
	if srp = rp && srp' = rp' 
	then ((rp,s),(rp',s'))::list,modified 
	else ((srp,s),(srp',s'))::list,((rp,srp,s),(rp',srp',s'))::modified 
								     )
      ([],[]) subspecies.bonds in
  {bonds=bonds;
   bonds_map = 
    List.fold_left 
      (fun map ((rp,srp,s),(rp',srp',s')) -> 
	let modif (rp,srp,s) image map = 
	  let map = RPathMap.remove rp map in 
	  let map =  (*add new*) 
	    let old = 
	      try 
		RPathMap.find srp map 
	      with 
		Not_found -> SitetypeMap.empty in
	    RPathMap.add srp (SitetypeMap.add s image old) map 
	  in 
	  map in
	modif (rp,srp,s) (srp',s') (modif (rp',srp',s') (srp,s) map))
      subspecies.bonds_map modified;
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

let merge_subspecies sp1 sp2 = 
  {
    bonds = sp1.bonds@sp2.bonds ;
    bonds_map = 
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
    sp1.bonds_map sp2.bonds_map ;
  subspecies_views = 
  RPathMap.map2 
    (fun _ b -> b)
    (fun _ b -> b)
    (fun _ b c -> if b=c then b else error 395 None)
    sp1.subspecies_views sp2.subspecies_views }

      

(** old definition 
If the boolean is true then this function associates a maximal list of compatible fragments to a bond 
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
		  List.fold_left 
		    (fun comp tp_i -> 
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
		    compatibility stack 
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
		      (b',black',n_tp::stack)::q'
		    else q')
		  q tp in
	      aux q' compatibility sol in
	let rep = aux [[x],StringSet.empty,[]] StringMap.empty [] in
		    (Hashtbl.add hash x rep;
		     rep)
  in f 


let new_get_denum bool (agent_to_int_to_nlist,view_of_tp_i,ode_handler) = 
  (** If the boolean is true then this function associates a maximal list of compatible fragments to a bond *)
  (** If the boolean is false then this function associated a maximal list of fragments to a bond *)
  (** This function is hash consed *)
  let hash = Hashtbl.create 21 in
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
	  | ([],_,subspecies)::q -> (*a subspecies is now complete *)
	      let compatibility' = (* here we update compatibility map *)
		if not bool then compatibility 
		else 
		  RPathMap.fold 
		    (fun rpath tp_i comp -> 
		      let view = view_of_tp_i tp_i in 
		      let agent = agent_of_view view in 
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
	  | (((a,s,a',s'),rpath)::b,black,subspecies)::q ->(* a species to be extend *)
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
	      let tp = 
		List.filter (*filter out the one that are ot compatible*)
                            (* TO DO improve by computing directly the list when compatibility relation is already known *)
		  (fun tp -> 
		    let view = view_of_tp_i tp in 
		    let interface = interface_of_view view  in 
		    let agent = agent_of_view view in
		    try 
		      (not bool) 
			or RPathMap.find rpath compatibility = interface 
		    with Not_found -> true )
		  tp in 
	      let q' =
		List.fold_left
		  (fun q' n_tp -> 
		    let view = view_of_tp_i n_tp in 
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
		      let b' = 
			String4Set.fold 
			  (fun ((ag1,s1),(ag2,s2)) b' -> 
			    if ag2 = a' && s'=s2  then b'
			    else ((ag2,s2,ag1,s1),rpath')::b')
			  (pending_edges view) b 
		      in
		      (b',black',
		       add_bond_to_subspecies 
			 (add_view_to_subspecies subspecies rpath' n_tp)
			 (rpath',s) (rpath,s'))::q'
		    else q')
		  q tp in
	      aux q' compatibility sol in
	let rep = 
	  aux 
	    [[x],StringSet.empty,empty_species] 
	    RPathMap.empty 
	    [] 
	in
	(Hashtbl.add hash x rep;
	 rep)
  in f 




let complete_subspecies (pending_edges,view_of_tp_i,keep_this_link,get_denum) subspecies = 
  (** This function takes a subspecies and build the list of the fragments that extend it *)
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
		String2Set.add y2 target,
		String2Map.add y2 (rp,y1,get_denum (fst y2,snd y2,fst y1,snd y1)) map))
	   pending_edges 
	   (target,map))
       subspecies.subspecies_views 
       (String2Set.empty,String2Map.empty) in 
   let sol = 
     String2Map.fold 
       (fun y2 (rp,y1,extension_list) sol_list -> 
	 let rp' = {rp with path = (y2,y1)::rp.path} in 
	 List.fold_left 
	   (fun pre_sol_list extension -> 
	     List.fold_left 
	       (fun extended_sol_list pre_sol -> 
                 let extended_sol = 
		   add_bond_to_subspecies  
		     (merge_subspecies 
			pre_sol 
			(shift_subspecies extension (StringMap.add "" rp' StringMap.empty)))
		        (rp,snd y1) (rp',snd y2) in
		 extended_sol::extended_sol_list		
				 )
	       [] pre_sol_list
	       )
	   sol_list extension_list 
	   
	   ) 
       map [subspecies] in 
   ()
   

  let complete_tp_list (pending_edges,view_of_tp_i,keep_this_link,get_denum) tp = 
    (** This function takes a subfragment and build the list of the fragments that contains it *)
    let root,target,map = 
      List.fold_left
	(fun (root,target,map) tp -> 
	  let pending_edges = pending_edges (view_of_tp_i tp) in
	  String4Set.fold 
	    (fun (y1,y2) (root,target,map) ->
	      (String2Set.add y1 root,
	       String2Set.add y2 target,
	       String2Map.add y2 y1 map))
	    pending_edges (root,target,map))
	(String2Set.empty,String2Set.empty,String2Map.empty) tp in
    let target = String2Set.diff target root in
    let (target2:int list list list) = 
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
      let rec aux (l:int list list list) 
	  (sol:int list list) = 
	match l with [] -> sol
	| (t:int list list)::q -> 
	    aux q 
	      (List.fold_left 
		 (fun liste (elt_t:int list) -> 
		   List.fold_left  
				   (fun sol b -> 
				     (elt_t@b)::liste)
		     liste sol)
		 [] 
		 (t:int list list)) in
      aux 
	target2 
	[tp] in 
    sol 

(** this primitive split a species into a list of maximal connected subspecies *)
let split_subspecies subspecies = 
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
	      (RPathSet.remove rpath available_rpaths) 
	      list'
	end
    | rpath::q when RPathSet.mem rpath available_rpaths ->
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
	  (fun hash rp -> RPathMap.add rp id hash)
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
	      error 829 None
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
    List.fold_left
      (fun bonds_map ((a,b),(c,d))  -> 
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
      species_map_without_bonds 
      subspecies.bonds 
  in
  IntMap.fold 
    (fun _ a l -> a::l) 
    species_map_with_bonds 
    [] 
	
 	
