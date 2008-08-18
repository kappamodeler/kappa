(* 21/11/2006 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Automatic generation of dependence packs *)
(* pack.ml *)

open Pb_sig 
open Data_structures 
open Tools 


let error (*i*) x (*t*) y = 
    unsafe
      (Some x) 
      (Some "packing.ml") 
      None (*(Some t)*) 
      None (*(Some i)*) 
      y

let error_frozen (*i*) x (*t*) y = 
    unsafe_frozen
      (Some x) 
      (Some "packing.ml") 
      None (*(Some t)*) 
      None (*(Some i)*) 
      y

let compute_pack pb = 
  let cpb = 
    match pb.intermediate_encoding with 
      None -> 
	(match pb.gathered_intermediate_encoding with 
	  None -> error_frozen  "PACKING 13" (fun () -> raise Exit)
	| Some a -> a)
    | Some a -> a in 
  let system = 
    match pb.boolean_encoding with 
      None -> 
	(match pb.gathered_boolean_encoding with 
	  None -> error_frozen  "PACKING 20" (fun () -> raise Exit)
	| Some a -> a.system) 
    | Some a -> a.system in 
  let map = StringMap.empty in
  let fadd a b map = 
    if fst a = fst b then 
      let oldmap = 
	try (StringMap.find (fst a) map)
	with Not_found -> String2Map.empty in 
      let old = 
	try (String2Map.find a oldmap) with 
	  Not_found -> String2Set.empty in 
      let new' = String2Set.add b old in 
      StringMap.add (fst a) (String2Map.add a new'  oldmap) map
  else map in 
  let map = 
    List.fold_left 
      (fun map (a,b,c) -> 
	List.fold_left (fun map s -> fadd (a,s) (a,s) map) 
	  (List.fold_left (fun map s -> fadd (a,s) (a,s) map) map c) b)
      map cpb.cpb_interface in 
  
  let var_of_g r spec = 
    List.fold_left 
      (fun set rule  -> let b = rule.injective_guard in 
	List.fold_left 
	  (fun set (a,b) -> 
	    match site_of_b a with 
	      Some (a,s) -> String2Set.add (spec a,s) set
	    | None -> set) set b)
      String2Set.empty 
      r.rules in 
  let var_of_c r spec = 
    List.fold_left 
      (fun set (a,b) ->  match site_of_b a with 
	   Some (a,s) -> String2Set.add (spec a,s) set
	 | None -> set)
      String2Set.empty r.control.context_update in 

  let dep = 
    List.fold_left 
      (fun map r -> 
	let spec_of_id = 
	  List.fold_left 
	    (fun map (a,b) -> StringMap.add a b map)
	    StringMap.empty r.id in 
	let spec x = StringMap.find x spec_of_id in 
	String2Set.fold 
	  (fun c map  -> 
	    String2Set.fold (fadd c) (var_of_g r spec) (fadd c c map))
	  (var_of_c r spec) map)
      map system in 
  let close_dep = 
    StringMap.map 
      (fun map -> 
	let rec aux to_visit visited sol = 
	  match to_visit with 
	    t::q when String2Set.mem t visited -> aux q visited sol 
	  | t::q -> 
	      let to_visit' = 
		String2Set.fold 
		  (fun a l -> a::l)
		  (String2Map.find t map) to_visit in 
	      let visited' = String2Set.add t visited in 
	      let sol' = t::sol in 
	      aux to_visit' visited' sol'
	  | [] -> sol
	in 
	String2Map.fold 
	  (fun k b1 s-> 
	    StringListSet.add 
	      (List.sort compare 
		 (List.rev_map 
		    snd 
		    (aux   [k] String2Set.empty [])))
	      s
	      )
	  map StringListSet.empty)
     
      dep in 
 
  StringMap.map
    (fun b -> 
      StringListSet.fold (fun l sol -> l::sol) b [])
    close_dep 

  
      
	  
