open Mods2
open Error
open Rule

type dep = CURR_TIME of float | RULE_FLAGS of (string list)

(*test = fun fake_rules_indices -> bool*)
(*modif = [(flg_1,mult_1);...;(flg_n;mult_n)]*)
type perturbation = {dep: dep ; 
		     test: (int StringMap.t) * Rule_of_int.t -> bool ; 
		     modif: IntSet.t * IntSet.t * (int StringMap.t) * Rule_of_int.t  -> IntSet.t * IntSet.t * Rule_of_int.t  ; 
		     test_str: string ; modif_str:string} 

type t = {
  fresh_pert:int;
  name_dep: IntSet.t StringMap.t ; (*pert_dep: [rule_name -> {pert_indices}] *)
  time_dep: (float*int) list ; (*time_stack: sorted list (t_0,pert_0)::...::(t_n,pert_n)::[]*) 
  perturbations: perturbation IntMap.t ; (*[pert_indice -> perturbation]*) 
}

let empty = {fresh_pert = 0 ; 
	     name_dep = StringMap.empty ; 
	     time_dep = [] ;
	     perturbations = IntMap.empty
	    }

type ast = Mult of ast * ast | Add of ast * ast | Div of ast * ast 
	   | Val_float of float | Val_sol of string | Val_kin of string | Val_infinity

let string_of_perturbation pert =
  Printf.sprintf "-Whenever %s do %s\n" pert.test_str pert.modif_str 

let print exp =
  Printf.printf "PERTURBATIONS:\n" ;
  IntMap.iter (fun _ pert -> 
		 Printf.printf "%s\n" (string_of_perturbation pert)
	      ) exp.perturbations
    

let rec string_of_ast ast = 
  match ast with
      Mult(ast1,ast2) ->
	let str1 = string_of_ast ast1 
	and str2 = string_of_ast ast2
	in
	  Printf.sprintf "(%s*%s)" str1 str2
    | Add(ast1,ast2) -> 
	let str1 = string_of_ast ast1 
	and str2 = string_of_ast ast2
	in
	  Printf.sprintf "(%s+%s)" str1 str2
    | Div(ast1,ast2) ->
	let str1 = string_of_ast ast1 
	and str2 = string_of_ast ast2
	in
	  Printf.sprintf "(%s/%s)" str1 str2
    | Val_float f -> string_of_float f
    | Val_sol flag -> flag 
    | Val_kin flag -> "kin("^flag^")"
    | Val_infinity -> "inf"

type ord = VAL of float | INF

let greater ord1 ord2 = 
  match (ord1,ord2) with
      (INF,INF) -> false
    | (INF,_) -> true
    | (_,INF) -> false
    | (VAL x,VAL y) -> (x>y)

let smaller ord1 ord2 = 
  match (ord1,ord2) with
      (INF,INF) -> false
    | (INF,_) -> false
    | (_,INF) -> true
    | (VAL x,VAL y) -> (x<y)

let rec eval rescale ast rule_of_name rules =
  let op f val1 val2 = 
    match (val1,val2) with
	(VAL v1,VAL v2) -> VAL (f v1 v2)
      | _ -> runtime "Experiment.eval"
  in
  match ast with
      Mult(ast1,ast2) ->
	let v1 = eval rescale ast1 rule_of_name rules 
	and v2 = eval rescale ast2 rule_of_name rules
	in
	  if (v1=INF) or (v2=INF) then INF else op (fun x y -> x*.y) v1 v2
    | Add(ast1,ast2) -> 
	let v1 = eval rescale ast1 rule_of_name rules 
	and v2 = eval rescale ast2 rule_of_name rules
	in
	  if (v1=INF) or (v2=INF) then INF else op (fun x y -> x+.y) v1 v2
    | Div(ast1,ast2) ->
	let v1 = eval rescale ast1 rule_of_name rules 
	and v2 = eval rescale ast2 rule_of_name rules
	in
	  if (v1=INF) then
	    if (v2=INF) then Error.runtime "Experiment.eval: undetermined form for rate inf/inf"
	    else INF
	  else 
	    if v2=INF then VAL 0.0
	    else op (fun x y -> x/.y) v1 v2
    | Val_float f -> VAL f
    | Val_infinity -> INF
    | Val_sol flag -> (
	try
	  let i = StringMap.find flag rule_of_name in
	  let _,inst_i = Rule_of_int.find i rules in VAL (inst_i/.rescale)
	with Not_found -> Error.runtime (flag^" is not a defined rule")
      )
    | Val_kin flag -> (
	try
	  let i = StringMap.find flag rule_of_name in
	  let r_i,_ = Rule_of_int.find i rules in
	    VAL r_i.kinetics
	with Not_found -> Error.runtime (flag^" is not a defined rule")
      )

let rec extract_dep ast = 
  match ast with
      Mult(ast1,ast2) -> (extract_dep ast1)@(extract_dep ast2)
    | Add(ast1,ast2) -> (extract_dep ast1)@(extract_dep ast2)
    | Div(ast1,ast2) -> (extract_dep ast1)@(extract_dep ast2)
    | Val_float _ -> []
    | Val_sol flag -> [flag]
    | Val_kin _ -> []
    | Val_infinity -> []

let add pert experiment =
  let dep = pert.dep in
  let fresh = experiment.fresh_pert in
  let name_dep,time_dep =
    match dep with
	CURR_TIME t -> (experiment.name_dep,sorted_insert (t,fresh) experiment.time_dep)
      | RULE_FLAGS l -> 
	  let name_dep = 
	    List.fold_right (fun flg map -> 
			       let set = try StringMap.find flg map with Not_found -> IntSet.empty in
				 StringMap.add flg (IntSet.add fresh set) map
			    ) l experiment.name_dep
	  in
	    (name_dep,experiment.time_dep)
  in
    {fresh_pert = fresh+1 ; 
     name_dep = name_dep ; 
     time_dep = time_dep ; 
     perturbations = IntMap.add fresh pert experiment.perturbations
    }
