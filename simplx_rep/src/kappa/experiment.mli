type dep = LESSER_TIME of float | RULE_FLAGS of (string list) | GREATER_TIME of float


type ast = Mult of ast * ast | Add of ast * ast | Div of ast * ast 
	   | Val_float of float | Val_sol of string | Val_kin of string | Val_infinity

type test = Comp of ast*ast | Timeg of float | Timel of float 


type ord = VAL of float | INF

(*test = fun fake_rules_indices -> bool*)
(*modif = [(flg_1,mult_1);...;(flg_n;mult_n)]*)
type perturbation = {dep_list: dep list; 
                     test_unfun_list: test list;
		     test_list: ((int Mods2.StringMap.t) * Rule.Rule_of_int.t -> bool) list;  (*rule_of_name,rules*) 
		     modif: Mods2.IntSet.t * Mods2.IntSet.t * (int Mods2.StringMap.t) * Rule.Rule_of_int.t  -> Mods2.IntSet.t * Mods2.IntSet.t * Rule.Rule_of_int.t  ; 
		     modif_unfun:string*ast;
                     test_str: string ; 
		     modif_str:string} 

type perturbation_unfun = 
   {dep_list_unfun: dep list; 
    test_unfun_list_unfun: test list;
    modif_unfun_unfun: string*ast;
    test_str_unfun: string ; 
    modif_str_unfun:string}  

type t = {
  fresh_pert:int;
  name_dep: Mods2.IntSet.t Mods2.StringMap.t ; (*pert_dep: [obs_name -> {pert_indices}] *)
  time_on: float Mods2.IntMap.t ; (*activate time_on: pert_id -> time*) 
  time_off: float Mods2.IntMap.t ; (*remove time_of: pert_id -> time*) 
  perturbations: perturbation Mods2.IntMap.t ; (*[pert_indice -> perturbation]*) 
}

type t_unfun = {
  fresh_pert_unfun:int;
  name_dep_unfun: Mods2.IntSet.t Mods2.StringMap.t ; (*pert_dep: [obs_name -> {pert_indices}] *)
  time_on_unfun: float Mods2.IntMap.t ; (*activate time_on: pert_id -> time*) 
  time_off_unfun: float Mods2.IntMap.t ; (*remove time_of: pert_id -> time*) 
  perturbations_unfun: perturbation_unfun Mods2.IntMap.t ; (*[pert_indice -> perturbation]*) 
}

val unfun: t -> t_unfun

val empty: t


val string_of_perturbation: perturbation -> string
val print: t -> unit 
val string_of_ast: ast -> string




val add: perturbation -> t -> t
val eval: float -> ast -> Rule.Rule_of_int.key Mods2.StringMap.t -> Rule.Rule_of_int.t -> ord

val greater: ord -> ord -> bool
val extract_dep: ast -> string list 
val smaller: ord -> ord -> bool
