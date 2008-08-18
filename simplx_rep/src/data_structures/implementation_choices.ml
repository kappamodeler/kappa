(* This module contains several implementation choices for each data-structure *)

(* Rule_of_int: to select rules at random *)

module Rule_of_int_functional_style = Val_map.Make
module Rule_of_int_in_place = Val_map.Make_in_place 

module Rule_of_int = Rule_of_int_in_place 

(* Do we use the same array to compute mod_ids in simulation *)

module Shared = 
  struct 
    let clean_solution a = 
      let t = a in 
      let n = Solution.AA.size a in 
      let rec aux k t  = 
	if k<0 
	then t
    else 
	  (aux (k-1) (Solution.AA.remove_index k t))
      in 
      let _ = aux (n-1) t 
      in a
    let alloc_solution = clean_solution
  end

module Allocated = 
  struct 
    let clean_solution a = ()
    let alloc_solution a = Solution.AA.create 5 
  end

module Clean_solution = Allocated


(* To compute string before outputing them on a channel  *)

module StringList = Stringlist.StringList
module StringElt = Stringlist.SingleString

module Stringstream = StringList

