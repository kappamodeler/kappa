open Tools 
open Data_structures
open Pb_sig
open Error_handler 


let error i s = 
  unsafe_frozen None (Some "refinements.ml") s (Some ("line  "^(string_of_int i))) (fun () -> raise Exit)

let compare_guard a b =
  try 
    let _ = 
      BMap.iter2i 
	(fun a b -> () )
	(fun a b -> raise Exit)
	(fun a b c -> if not b=c then raise Exit)
	a b 
    in true
  with 
    Exit -> false 

let get_id r = r.Pb_sig.r_simplx.Rule.id 

let add_id_set g set = 
  List.fold_left 
    (fun set a -> IntSet.add (get_id a) set)
    set 
    (g.labels)

let elts set = IntSet.fold (fun a l -> a::l) set [] 

let fadd x y map = 
  let old = 
    try 
      IntMap.find x map 
    with 
      Not_found -> IntSet.empty 
  in
  IntMap.add x (IntSet.add y old) map 

let compute_refinement boolean_encoding = 
  List.map 
    (fun rule_class -> 
      let rule_class = 
	List.map 
	  (fun rule -> 
	    rule,
	    List.fold_left 
	      (fun sol (a,b) -> BMap.add a b sol) 
	      BMap.empty 
	      rule.injective_guard)
	  rule_class.rules in 
      let presolution = 
	List.fold_left 
	  (fun sol (rule,g) -> 
	    let list_succ_star = 
	      List.fold_left 
		(fun sol (rule2,g2) -> 
		  if compare_guard g g2 
		  then add_id_set rule2 sol
		  else sol)
		IntSet.empty
		rule_class 
	    in 
	    List.fold_left 
	      (fun sol idr -> 
		IntSet.fold 
		  (fun idg sol -> fadd idr idg sol)
		  list_succ_star sol)
	      sol 
	      (List.map get_id rule.labels)
	      )
	  IntMap.empty 
	  rule_class in 
      presolution) 
    boolean_encoding.system 


let compute_maximal_relation liste =  
  List.map 
    (fun presolution -> 
      let maxima = 
	IntMap.fold 
	  (fun t image set  -> 
	    match elts image with 
	      [] -> error 79 None 
	    | [q] when t=q -> IntSet.add t set
	    | [q] -> error 81 None 
	    | _ -> set)
	  presolution 
	  IntSet.empty in
      let (solution:IntSet.t IntMap.t) = 
	IntMap.map 
	  (fun b -> IntSet.inter maxima b)
	  presolution in 
      solution)
    liste 


let compute_dag_relation liste = 
  List.map 
    (fun presolution -> 
      let fadd i j map = 
	let old,old' = 
	  try 
	    IntMap.find i map 
	  with 
	    Not_found -> [],IntSet.empty  in
	IntMap.add i (j::old,IntSet.add j old') map
      in
      let reverse_map = 
	IntMap.fold 
	  (fun i s -> 
	    IntSet.fold 
	      (fun j -> fadd j i) 
	      s)
	  presolution IntMap.empty in
      let maxima = 
	IntMap.fold 
	  (fun t image set  -> 
	    match elts image with 
	      [] -> error 79 None 
	    | [q] when t=q -> t::set
	    | [q] -> error 81 None 
	    | _ -> set)
	  presolution [] in 
      let rec aux wl prof remaining = 
	match wl 
	with [] -> prof 
	| t::q -> 
	    begin
	      let depth,prof = 
		try 
		  IntMap.find t prof,prof
		with 
		  Not_found -> 1,IntMap.add t 1 prof  in
	      let remaining = 
		IntMap.mapi 
		  (fun i j -> IntSet.remove i j) 
		  remaining in 
	      let wl,prof,remaining = 
		List.fold_left
		  (fun (wl,prof,remaining) j ->
		    if t = j 
		    then  
		      (wl,prof,remaining)
		    else 
		      let wl',remaining' = 
			let old = 
			  try 
			    IntMap.find j remaining 
			with Not_found -> IntSet.empty in
			let new' = 
			  IntSet.remove t old in
			let remaining' = IntMap.add j new' remaining in
			if IntSet.is_empty new' 
			then 
			  j::wl,remaining'
			else
			  wl,remaining' in
		      let prof' = 
			let old = 
			  try 
			    IntMap.find j prof 
			  with 
			    Not_found -> 1 in
			IntMap.add j (max (depth+1) old) prof in
		      (wl',prof',remaining'))
		  (q,prof,remaining) 
		  (try fst (IntMap.find t reverse_map)
		  with Not_found -> [])
		  in
	      aux wl prof remaining  
      	    end
      in 
      let prof = 
	aux 
	  maxima 
	  IntMap.empty 
	  presolution 
      in
      let add_edge i j map = 
	let old = 
	  try 
	    IntMap.find i map 
	  with 
	    Not_found -> 
	      IntSet.empty in
	IntMap.add i (IntSet.add j old) map in
      let remove_edge i j map = 
	let set = 
	  try 
	    IntMap.find i presolution 
	  with 
	    Not_found -> 
	      IntSet.empty in
	let set = IntSet.remove i set in 
	IntSet.fold 
          (fun s map -> 
	    if s = i then map 
	    else 
	      let old = 
		try 
		  IntMap.find s map 
	      with 
		  Not_found -> IntSet.empty in
	      IntMap.add s (IntSet.diff old set) map)
	  (try snd (IntMap.find i reverse_map)
	  with Not_found -> IntSet.empty) 
	  map
      in 
      let rec aux k edges potential_edges = 
	let bool,edges,potential_edges = 
	  IntMap.fold 
	    (fun i succ (bool,edges,potential_edges) -> 
	      let profi = 
		try 
		  IntMap.find i prof
		with
		  Not_found -> 0 in
	      IntSet.fold
		(fun j (bool,edges,potential_edges) -> 
		  let profj = 
		    try IntMap.find j prof
		    with 
		      Not_found -> 0 in
		  if profj = profi-k
		  then 
		    true,
		    add_edge i j edges,
		    remove_edge i j potential_edges 
		  else
		    bool,edges,potential_edges)
		succ (bool,edges,potential_edges))
	    potential_edges 
	    (false,edges,potential_edges)
	in 
	if bool 
	then 
	  aux (k+1) edges potential_edges 
	else edges in
      aux 1 IntMap.empty presolution)
    liste 

let dump_refinement file file2 rules rel = 
  let rule_map = 
    List.fold_left 
      (fun map a -> 
	let id = a.Rule.id in
	IntMap.add id a map)
      IntMap.empty rules 
  in 
  if file = "" then () 
  else
    begin
      let channel = open_out file in
      let print_string s = Printf.fprintf channel "%s" s in
      let _ = print_string "digraph G{\n" in
      let string_of_rule s = 
	(match s.Rule.flag 
	      with None -> s.Rule.input 
	      |	Some a -> a)
      in
      let print_rule s = 
	let _ = print_string "\"" in
	let _ = print_string (string_of_rule s) in
	let _ = print_string "\"" in () 
      in
      let print_rule_id a = 
	try 
	  let s = 
	  try 
	    IntMap.find a rule_map
	  with 
	    Not_found -> raise Exit in
	print_rule s 
	with _ -> error 232  None  
      in
      let _ = 
	IntMap.iter 
	  (fun _ a -> 
	    print_rule a;
	    print_string "\n")
	  rule_map in
      let is_diag (x,y) = x=y in 
      let _ = 
	List.fold_left
	  (fun edges x -> 
	    IntMap.fold 
	      (fun i s edges -> 
		let rulei = IntMap.find i rule_map in
		IntSet.fold 
		  (fun j edges -> 
		    let rulej = IntMap.find j rule_map in 
		    let pair = string_of_rule rulei,string_of_rule rulej in
		    if String2Set.mem pair edges or is_diag pair 
		    then edges 
		    else 
		      let _ = print_rule rulei in
		      let _ = print_string "->" in
		      let _ = print_rule_id j in
		      let _ = print_string "\n" in
		      String2Set.add pair edges)
		  s edges)
	      x edges)
	  String2Set.empty rel
      in 
      let _ = print_string "}\n" in 
      let _ = close_out channel in 
      let _ = 
	if file2 <> "" 
	then 
	  let _ = Sys.command ("dot -Tjpg "^file^" -o "^file2) in () in 
      ()
    end

let dump_refinement_in_XML title channel rules rel = 
  let rule_map = 
    List.fold_left 
      (fun map a -> 
	let id = a.Rule.id in
	IntMap.add id a map)
      IntMap.empty rules 
  in 
  begin
    let print_string s = Printf.fprintf channel "%s" s in
    let _ = print_string "<Refinement Name=\"" in
    let _ = print_string title in
    let _ = print_string "\">" in
    let string_of_rule s = string_of_int s.Rule.id in 
    let print_rule s = 
      let _ = print_string "\"" in
      let _ = print_string (string_of_int s.Rule.id) in
      let _ = print_string "\"" in () 
    in
    let is_diag (x,y) = x=y in 
    let _ = 
      List.fold_left
	(fun edges x -> 
	  IntMap.fold 
	    (fun i s edges -> 
	      let rulei = IntMap.find i rule_map in
	      IntSet.fold 
		(fun j edges -> 
		  let rulej = IntMap.find j rule_map in 
		  let pair = string_of_rule rulei,string_of_rule rulej in
		  if String2Set.mem pair edges or is_diag pair 
		  then edges 
		  else 
		    let _ = print_string "<Association FromRule=" in
		    let _ = print_rule rulei in 
		    let _ = print_string " ToRule=" in
		    let _ = print_rule rulej in
		    let _ = print_string "/>\n" in
		    String2Set.add pair edges)
		s edges)
	    x edges)
	String2Set.empty rel
    in 
    let _ = print_string "</Refinement>\n" in 
      ()
    end
