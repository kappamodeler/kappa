(* 21/02/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* To compute the causality map  *)
(* causality_map.ml *)

open Tools
open Data_structures 
open Pb_sig


type causality_map = string*string 

let print_short = false 


module Hashb = QMap

let compute_influence_map (pb:'a Pb_sig.cpb) = 
    let bt_to_control = Hashb.empty  in 
    let bt_to_guard = Hashb.empty  in 
    let bf_to_control = Hashb.empty  in 
    let system = pb.cpb_rules  in 
    let feed idlist liste hash = 
      	 list_fold
           (fun q hash ->
              let old = try Hashb.find q hash with Not_found -> [] in 
              let new' = list_fold (fun a b -> a::b) idlist old in 
                Hashb.add q new' hash)
	   liste hash in
    let bt_to_control,bf_to_control,bt_to_guard = 
        list_fold
            (fun rc (a,b,c) ->
	       let quarks = 
		 match rc.cpb_quarks with 
		     Some a -> a
		   | _ -> frozen_unsafe (Some "Quarks are missing") (Some "influence_map.ml") (Some "compute_influence_map") (Some "line 37")  (fun () -> raise Exit) in 
               let idlist = 
		 List.fold_left 
		   (fun sol (a,_,_) -> 
		      List.fold_left (fun sol a -> a::sol)
			sol a)
		   [] rc.cpb_guard in
		 feed idlist quarks.mod_pos a,
	         feed idlist quarks.mod_neg b,
	         feed idlist quarks.test_pos c)
	   system
	  (bt_to_control,bf_to_control,bt_to_guard) in
    let _ = trace_print "CAUSALITY OTHER WAY" in 
    let _ = if !Config_complx.trace then print_newline () else () in 
    let fadd (a,b) sol = 
      let new' = try (IntMap.find a sol) with Not_found -> IntSet.empty  in 
      IntMap.add a (IntSet.add b new') sol in 
    let g bool a b sol = 
      Hashb.fold2
	(fun _ _ sol -> sol)
	(fun _ _ sol -> sol)
	(fun _ rset rset' sol -> 
	   List.fold_left 
             (fun sol a -> 
                List.fold_left 
                  (fun sol b  -> 
		    if a.r_simplx.Rule.id=b.r_simplx.Rule.id & not bool then sol 
		    else 
		    fadd 
		       (a.r_simplx.Rule.id,b.r_simplx.Rule.id) 
		       sol)
                  sol rset')
             sol rset)
	a b sol 
    in 
  
    

      (if !Config_complx.wake_up 
	  then g true
	   bt_to_control 
	   bt_to_guard 
	  IntMap.empty else IntMap.empty),
	(if !Config_complx.inhibition
	then 
	 g false
	    bf_to_control
	    bt_to_guard
	      IntMap.empty 
	else IntMap.empty)



