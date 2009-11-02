
type dep = LESSER_TIME of float | RULE_FLAGS of (string list) | GREATER_TIME of float

(*test = fun fake_rules_indices -> bool*)
(*modif = [(flg_1,mult_1);...;(flg_n;mult_n)]*)
type perturbation = {dep_list: dep list; 
		     test_list: ((int Mods2.StringMap.t) * Rule.Rule_of_int.t -> bool) list;  (*rule_of_name,rules*) 
		     modif: Mods2.IntSet.t * Mods2.IntSet.t * (int Mods2.StringMap.t) * Rule.Rule_of_int.t  -> Mods2.IntSet.t * Mods2.IntSet.t * Rule.Rule_of_int.t  ; 
		     test_str: string ; 
		     modif_str:string} 

type t = {
  fresh_pert:int;
  name_dep: Mods2.IntSet.t Mods2.StringMap.t ; (*pert_dep: [obs_name -> {pert_indices}] *)
  time_on: float Mods2.IntMap.t ; (*activate time_on: pert_id -> time*) 
  time_off: float Mods2.IntMap.t ; (*remove time_of: pert_id -> time*) 
  perturbations: perturbation Mods2.IntMap.t ; (*[pert_indice -> perturbation]*) 
}

val empty: t

type ast = Mult of ast * ast | Add of ast * ast | Div of ast * ast 
	   | Val_float of float | Val_sol of string | Val_kin of string | Val_infinity

val string_of_perturbation: perturbation -> string
val print: t -> unit 
val string_of_ast: ast -> string

type ord = VAL of float | INF


val add: perturbation -> t -> t
val eval: float -> ast -> Rule.Rule_of_int.key Mods2.StringMap.t -> Rule.Rule_of_int.t -> ord

val greater: ord -> ord -> bool
val extract_dep: ast -> string list 
val smaller: ord -> ord -> bool
