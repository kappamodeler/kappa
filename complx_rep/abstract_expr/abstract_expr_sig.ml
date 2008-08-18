(* 11/05/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Signature of Abstract expressions *)
(* abstract_expr_sig.ml *)
open Config_complx
open Pb_sig
open Var
open Expr
open Kleenean_expr
open Tools
open Data_structures
open Concretization


module type AbstractExprBool =
sig 

  (** type for sets of boolean valuations that may be partionized into carthesian product*)
  type abstract_expr 

  (** type for (not partitionized) sets of boolean valuations *)
  type reachable_states

  (** module for three-values (i.e. Kleenean) logic formula *)	
  module K:Kleenean_Expr

  (** empty set predicate *)
  val is_bot: abstract_expr -> bool

  (** whole set *)
  val ae_true: abstract_expr

  (** whole set predicate *)
  val is_ae_true: abstract_expr -> bool 

  (** whole set predicate (not partitionized)*)
  val r_ae_true: reachable_states

  (** empty set predicate (not partitionized)*)
  val r_is_bot: reachable_states -> bool 

  (** may_be_true expr var_set returns the subset of the variables in var_set that can takes a true value in the set of valuation expr *)  
  val may_be_true: abstract_expr -> K.E.V.varset -> K.E.V.varset

  (** atom_pos v denotes the set of the valuations sigma so that sigma(v)=T *)    
  val atom_pos: K.E.V.var -> abstract_expr

  (** atom_neg v denotes the set of the valuations sigma so that sigma(v)=F *)
  val atom_neg: K.E.V.var -> abstract_expr

  (** r_atom v denotes the not partitionized set of valuations sigma such that sigma(v)=T *)
  val r_atom: K.E.V.var -> reachable_states

  (** imply a b contains all valuations not in a, and valuations that are both in a and in b*)
  val imply: abstract_expr -> abstract_expr -> abstract_expr

  (** r_imply a b contains all valuations not in a, and valuations that are both in a and in b, it applies with not_paritionized sets *)
  val r_imply: reachable_states -> reachable_states -> reachable_states

  (** r_union is the union operator between not partitionized sets of valuations *)
  val r_union: reachable_states -> reachable_states -> reachable_states 

  (** r_neg is the complement operator for not partitionized set of valuation *)
  val r_neg: reachable_states -> reachable_states

  (** r_conj is the meet operator for not partitionized sets of valuation *)    
  val r_conj:reachable_states -> reachable_states -> reachable_states 

  (** conj is the meet operator for sets of valuations  *)   
  val conj: abstract_expr -> abstract_expr -> abstract_expr
  
  (** union is the union operator for sets of valuations *)
  val union: abstract_expr -> abstract_expr -> abstract_expr

  (** forget a map contains all the valuations sigma such that there is a valuation sigma' in a that coincides for the variables v such that map(v)=F *)
  val forget: abstract_expr -> (K.E.V.var -> bool) -> abstract_expr

  (** project a map applies the restriction operator (that keeps only the variables for which map(v)=T) with all the elements of a *)
  val project: abstract_expr -> (K.E.V.var -> bool) -> abstract_expr

  (** if sigma is a valuation in a, then sigma': v -> not sigma(v) when v is a member of set, v -> sigma(v) otherwise is a valuation in switch a set *)
  val switch: abstract_expr -> K.E.V.varset -> abstract_expr

  (** if sigma is a valuation in a, then sigma': v -> map(v) if map(v) is defined, v-> sigma(v) otherwise is a valuation of set a map *)
  val set: abstract_expr -> bool K.E.V.varmap  -> abstract_expr
  
  (** if sigma is a valuation in a, then both sigma and sigma': v -> map(v) if map(v) is defined, v-> sigma(v) otherwise are a valuation of set a map *)
 val weak_set: abstract_expr -> bool K.E.V.varmap -> abstract_expr

  (** set inclusion between set of valuations *)
  val is_included: abstract_expr -> abstract_expr -> bool


  val rename: abstract_expr -> (K.E.V.var -> K.E.V.var) -> abstract_expr
  val increasing_renaming: abstract_expr -> (string -> string) -> abstract_expr
  val test: abstract_expr -> K.E.V.var -> bool -> abstract_expr 

  val reachable_states_of_expr: K.E.expr -> reachable_states
  val reachable_states_of_abstract_expr: abstract_expr -> reachable_states
  val summarize_abstract_expr: abstract_expr -> (string*reachable_states) list
  val reachable_states_of_valuation_list: K.E.boolean_valuation list -> K.E.V.varset -> reachable_states
  val print_reachable_states: reachable_states -> (string ->string)  -> reachable_states  pb -> string option ->   out_channel option -> string list list
  val print_reachable_states2: reachable_states -> (string ->string)  -> reachable_states  pb -> string option -> (((string * string) * (string*string)),int) Hashtbl.t * int ref ->   out_channel option -> string list list

  val var_of_expr: abstract_expr -> K.E.V.varset
  val print: abstract_expr ->  string   -> reachable_states pb  ->  pretty_fun ->  string option  ->  out_channel option  -> (string list * string  list list ) list
  val print_bdd: abstract_expr -> unit
  val fnd_of_bdd: abstract_expr -> (K.E.V.var*bool) list list 
  val expr_of_abstract_expr: abstract_expr -> K.E.expr
  val guard: K.kleenean_rule_system -> abstract_expr
  val abstract_system: compression_mode -> K.kleenean_rule_system -> reachable_states -> K.E.V.varset 
->  (K.E.V.var -> K.E.V.var list) -> string list -> K.kleenean_rule_system * (string list)

  val f:b->bool
  val all_false: b list -> abstract_expr
  val list_conj: (b*bool) list -> abstract_expr
  val atmost_once: b list -> abstract_expr
  val exactly_once: b list -> abstract_expr

  val r_all_false: b list -> reachable_states
  val r_list_conj: (b*bool) list -> reachable_states
  val r_atmost_once: b list -> reachable_states
  val r_exactly_once: b list -> reachable_states

(*with sorted list*)
  val r_all_false_s: b list -> reachable_states 
  val r_atmost_once_s: b list -> reachable_states
  val r_exactly_once_s: b list -> reachable_states


      
  val export_ae:  abstract_expr ->  string -> reachable_states pb -> pretty_fun -> (string*(bool * pretty  StringMap.t) *pretty_fun) list

  val compute_abstract_lens: reachable_states StringMap.t -> reachable_states 
  val compute_subviews: abstract_expr -> reachable_states KeyMap.t
  val restore_subviews: reachable_states KeyMap.t -> abstract_expr
end

