open Mods2
open Error
open Rule

type dep = LESSER_TIME of float | RULE_FLAGS of (string list) | GREATER_TIME of float

type ast = Mult of ast * ast | Add of ast * ast | Div of ast * ast 
	   | Val_float of float | Val_sol of string | Val_kin of string | Val_infinity


type ord = VAL of float | INF


type test = Comp of ast*ast | Timeg of float | Timel of float
(*test = fun fake_rules_indices -> bool*)
(*modif = [(flg_1,mult_1);...;(flg_n;mult_n)]*)

type perturbation = {dep_list: dep list; 
                     test_unfun_list: test list;
		     test_list: ((int StringMap.t) * Rule_of_int.t -> bool) list;  (*rule_of_name,rules*) 
		     modif: IntSet.t * IntSet.t * (int StringMap.t) * Rule_of_int.t  -> IntSet.t * IntSet.t * Rule_of_int.t  ; 
                     modif_unfun: string*ast;
		     test_str: string ; 
		     modif_str:string} 

type t = {
  fresh_pert:int;
  name_dep: IntSet.t StringMap.t ; (*pert_dep: [obs_name -> {pert_indices}] *)
  time_on: float IntMap.t ; (*activate time_on: pert_id -> time*) 
  time_off: float IntMap.t ; (*remove time_of: pert_id -> time*) 
  perturbations: perturbation IntMap.t ; (*[pert_indice -> perturbation]*) 
}


type perturbation_unfun = 
   {dep_list_unfun: dep list; 
    test_unfun_list_unfun: test list;
    modif_unfun_unfun: string*ast;
    test_str_unfun: string ; 
    modif_str_unfun:string}  

type t_unfun = {
  fresh_pert_unfun:int;
  name_dep_unfun: Mods2.IntSet.t Mods2.StringMap.t ; (*pert_dep: [obs_name -> {pert_indices}] *)
  time_on_unfun: float Mods2.IntMap.t ; (*activate time_on: pert_id -> time*) 
  time_off_unfun: float Mods2.IntMap.t ; (*remove time_of: pert_id -> time*) 
  perturbations_unfun: perturbation_unfun Mods2.IntMap.t ; (*[pert_indice -> perturbation]*) 
}

let unfun t = 
  {
    fresh_pert_unfun=t.fresh_pert;
    name_dep_unfun= t.name_dep;
    time_on_unfun= t.time_on;
    time_off_unfun= t.time_off;
    perturbations_unfun=
      IntMap.map 
        (fun p -> 
           {dep_list_unfun= p.dep_list;
            test_unfun_list_unfun= p.test_unfun_list;
            test_str_unfun=p.test_str;
            modif_unfun_unfun=p.modif_unfun;
            modif_str_unfun=p.modif_str})
        t.perturbations}
    
let empty = {fresh_pert = 0 ; 
	     name_dep = StringMap.empty ; 
	     time_on = IntMap.empty ;
	     time_off = IntMap.empty ;
	     perturbations = IntMap.empty
	    }


let string_of_perturbation pert =
  Printf.sprintf "-Whenever %s do %s\n" pert.test_str pert.modif_str 

let print exp =
  Printf.printf "PERTURBATIONS:\n" ;
  IntMap.iter (fun i pert -> 
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
      | _ -> 
	  let s = "Experiment.eval" in
	  runtime 
	    (Some "experiment.ml",
	     Some 86,
	     Some s)
	    s
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
	    if (v2=INF) then 
	      let s = "Experiment.eval: undetermined form for rate inf/inf" in
	      runtime
		(Some "experiment.ml",
		 Some 110,
		 Some s)
		s

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
	with Not_found -> 
	  let s = (flag^" is not a defined rule") in
	  runtime
	    (Some "experiment.ml",
	     Some 128,
	     Some s)
	    s
      )
    | Val_kin flag -> (
	try
	  let i = StringMap.find flag rule_of_name in
	  let r_i,_ = Rule_of_int.find i rules in
	    VAL r_i.kinetics
	with Not_found -> 
	  let s = (flag^" is not a defined rule") in
	  runtime 
	    (Some "experiment.ml",
	     Some 141,
	     Some s)
	    s
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
  let dep_list = pert.dep_list in
  let fresh = experiment.fresh_pert in
  let name_dep,settime =
    List.fold_left (fun (name_dep,settime) dep -> 
		      match dep with
			| LESSER_TIME t -> 
			    begin
			      match settime with
				  None -> (experiment.name_dep, Some (-1.,t)) (*-1 for initial time*) 
				| Some (t0,t1) -> 
				    if t<t0 then (
				      Printf.fprintf stderr "*Warning* precondition of modification \"%s\" is never satisfied!\n" (string_of_perturbation pert) ;
				      (experiment.name_dep,None)
				    )
				    else
				      let t1' = if t1 < 0.0 then t else min t t1 in
					(experiment.name_dep,Some (t0,t1')) 
			    end
			| GREATER_TIME t -> 
			    begin
			      match settime with
				  None -> (experiment.name_dep, Some (t,-1.))  (*-1 for infinity*)
				| Some (t0,t1) -> 
				    if (t>t1) && (t1>0.0) then (
				      Printf.fprintf stderr "*Warning* precondition of modification \"%s\" is never satisfied!\n" (string_of_perturbation pert) ;
				      (experiment.name_dep,None)
				    )
				    else
				      let t0' = max t0 t in 
					(experiment.name_dep,Some (t0',t1)) 
			    end
			| RULE_FLAGS l -> 
			    match settime with 
				Some _ -> (experiment.name_dep,settime) (*if perturbation has a time dep, then no obs dependency*)
			      | None ->
				  let name_dep = 
				    List.fold_right (fun flg map -> 
						       let set = try StringMap.find flg map with Not_found -> IntSet.empty in
							 StringMap.add flg (IntSet.add fresh set) map
						    ) l name_dep
				  in
				    (name_dep,settime)
		   ) (experiment.name_dep,None) dep_list 
      
  in
  let time_on,time_off = match settime with 
      None -> (experiment.time_on,experiment.time_off) 
    | Some (t0,t1) -> 
	(IntMap.add fresh t0 experiment.time_on,IntMap.add fresh t1 experiment.time_off)
  in
    {fresh_pert = fresh+1 ; 
     name_dep = name_dep ; 
     time_on = time_on ; 
     time_off = time_off ;
     perturbations = IntMap.add fresh pert experiment.perturbations
    }
