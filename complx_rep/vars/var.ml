(* 31/10/2006 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Id and Vars *)
(* var.ml *)
open Pb_sig

module type Id = 
   sig
     type id 
     val string_of_id: id -> string
     val id_of_string: string -> id
     val print_id: id -> unit
   end
module Id = 
  struct 
    type id = string 
    let string_of_id x = x
    let id_of_string x = x
    let print_id = print_string 
  end

module type Keys = 
    sig
      type t
      val key_of_b: b -> t
      val b_of_key: t -> b
      val compare:t->t->int
      val concat: 'a list -> 'a  list -> 'a  list
      val print:t-> unit
      val to_string: t -> string
      val pref:t->string -> t
    end

module type Var = 
    sig 
      module K:Keys
      type var=K.t
      type varset
      type 'a varmap 
      val pref: var -> string -> var
      val varset_add: var -> varset -> varset
      val varset_empty: varset 
      val varset_is_empty: varset -> bool
      val varset_mem: var -> varset -> bool 
      val varset_iter: (var -> unit) -> varset -> unit
      val varset_remove: var -> varset -> varset
      val varset_minus: varset -> varset -> varset
      val varset_singleton: var -> varset 
      val varset_union: varset -> varset -> varset
      val varset_cap: varset -> varset -> varset
      val fold_vars: (var -> 'a -> 'a) -> varset -> 'a -> 'a
      val print_var: var -> unit
      val var_of_b: b -> var
      val b_of_var: var -> b
      val string_of_var: var -> string
      val varmap_empty: 'a varmap
      val varmap_find: var -> 'a varmap -> 'a
      val varmap_add: var -> 'a -> 'a varmap -> 'a varmap
      val varmap_fold: (var -> 'a -> 'b -> 'b) -> 'a varmap -> 'b -> 'b
      val varmap_iter: (var -> 'a  -> unit) -> 'a varmap -> unit
      val close: (var -> var list) -> var -> varset
    end



module RuleBool = 
  (struct 
    type t = b
    let compare a b = (-(compare_b a b)) 
    let b_of_b i = 
      match i with 
	L(a,b) -> l(a,b)
      |	_ -> i 
    let key_of_b i = b_of_b i
    let b_of_key i = i 
  
    let pref a b = 
      match a with 
     	B(x,y,z) -> B(b,y,z)
      | H(a,c) -> H(b,c) 
      |	AL((x,y,z),t) -> AL((b,y,z),t)
      |	_ -> (print_string "pref";raise Exit) 
   
    let to_string = string_of_b 

    let print i = print_string (to_string i)
    let concat i j = j@i
  end)

module Var = 
  (functor (Keys:Keys) -> 
    (struct 
      module K=Keys
      type var = K.t
	    
      module KSet = Set.Make(K)
      module KMap = Map.Make(K)
	  
      type varset = KSet.t
      type 'a varmap = 'a KMap.t
	    
     
      let print_var x = (K.print x)
      let var_of_b  = K.key_of_b 
      let pref = K.pref 
      let b_of_var = K.b_of_key 
      let varset_add = KSet.add
      let varset_is_empty = KSet.is_empty
      let varset_empty = KSet.empty
      let varset_mem = KSet.mem
      let fold_vars = KSet.fold 
      let string_of_var = K.to_string
      let varset_remove = KSet.remove
      let varset_iter = KSet.iter
      let varset_union = KSet.union
      let varset_cap = KSet.inter
      let varmap_add = KMap.add
      let varmap_empty = KMap.empty
      let varmap_fold = KMap.fold
      let varmap_find = KMap.find
      let varmap_iter = KMap.iter
      let varset_minus = KSet.diff
      let varset_singleton = KSet.singleton 
      let close g a = 
	let rec aux to_visit visited  = 
	  match to_visit with [] -> visited
	  | t::q when varset_mem t visited -> aux q visited 
	  | t::q -> aux ((g t)@to_visit) (varset_add t visited) 
	in aux [a] varset_empty    

	  end:Var))
