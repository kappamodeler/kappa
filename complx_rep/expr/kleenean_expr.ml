(* 13/04/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Signature of Abstract expressions *)
(* kleenean_expr.ml *)


open Tools
open Config_complx
open Var
open Expr
open Pb_sig
open Data_structures 
  
module type Kleenean_Expr =
  sig
    module E:ExprBool
    module Id:Id
	
    type kleenean_valuation = kleenean_token E.V.varmap
    type boolean_rule_system = (rule_id list  * control * E.boolean_valuation) list 
    type kleenean_rule_system = (rule_id list * control  * kleenean_valuation) list 


	  
    val kfold:(E.V.var -> kleenean_token -> 'a -> 'a) -> kleenean_valuation -> 'a -> 'a
    val ksfold:((rule_id list * control  * kleenean_valuation) -> 'a) -> ('a -> 'a -> 'a) -> kleenean_rule_system -> 'a -> 'a
    val ksmap: (kleenean_valuation -> kleenean_valuation) -> kleenean_rule_system -> kleenean_rule_system 
    val expr_of_kleenean_valuation: kleenean_valuation -> E.expr
    val expr_of_kleenean_rule_system: kleenean_rule_system -> E.expr
    val kleenean_valuation_of_boolean_valuation: E.boolean_valuation -> E.V.varset -> kleenean_valuation 
    val kleenean_rule_system_of_boolean_rule_system: boolean_rule_system -> E.V.varset -> kleenean_rule_system 
    val build_boolean_rule_system: (rule_id list * control  * E.V.var list) list  -> E.V.varset -> boolean_rule_system   
    val build_kleenean_rule_system: (rule_id list * control  * (E.V.var *bool) list) list  -> E.V.varset -> kleenean_rule_system 
    val extract_kleenean_rule_system: kleenean_rule_system -> kleenean_rule_system  
    val list_of_kleenean_valuation: kleenean_valuation -> (E.V.var * bool) list 
    val abstract:  kleenean_valuation -> E.V.var -> kleenean_valuation
    val print_kleenean_system:  (rule_id -> bool) -> (rule_id  -> string) -> (string -> int) -> (IntSet.t) -> kleenean_rule_system -> string option -> (string->string) -> (string -> string) -> bool  ->  out_channel option -> (rule_id list * string list) list
    val empty_kleenean_rule_system: kleenean_rule_system 
  end
      
module Kleenean_expr = 
  (functor (Expr:ExprBool) -> 
    (functor (Id:Id) -> 
      (struct
	module E = Expr
	module Id = Id
	    
	type kleenean_valuation = kleenean_token E.V.varmap
	type boolean_rule_system = (rule_id list  * control * E.boolean_valuation) list 
	type kleenean_rule_system = (rule_id list * control  * kleenean_valuation) list 
	      
	let list_of_kleenean_valuation m = 
	  E.V.varmap_fold 
	    (fun a b m -> 
	      match b with 
		TRUE -> (a,true)::m
	      |	FALSE -> (a,false)::m
	      |	ANY -> m)
	    m [] 
	let empty_kleenean_rule_system = []
	let ksfold f g (a:kleenean_rule_system) b = 
	  match a with [] -> b 
	  | t::q -> list_fold (fun a sol -> g (f a) sol) q (f t)
	   
	let kfold = E.V.varmap_fold 
	let ksmap f s = List.rev_map (fun (a,b,c) -> (a,b,f c)) s
	    
	let expr_of_kleenean_valuation v = 
	  kfold 
	    (fun x b expr -> 
	      match b with 
		TRUE -> E.expr_and expr (E.expr_atom x)
	      | FALSE -> E.expr_and expr (E.expr_not (E.expr_atom x))
	      | ANY -> expr)
	    v E.expr_true
	    
	let expr_of_kleenean_rule_system s = 
          ksfold 
	    (fun (_,_,x) -> expr_of_kleenean_valuation x)
	    E.expr_or 
	    s (E.expr_not(E.expr_true))
	    
	let build_boolean_rule_system l vset  = 
	  List.rev_map (fun (s,c,a) -> (s,c,E.boolean_valuation_of_list a)) l
	    
	let build_kleenean_rule_system l vset = 
          let init = 
            E.V.fold_vars 
	      (fun x sol -> E.V.varmap_add x ANY sol)
	      vset E.V.varmap_empty in                           
	  ((List.rev_map 
	      (fun (s,c,a) -> 
		(s,c,list_fold (fun (l,a) sol -> 
		  let f k sol = 
		    E.V.varmap_add l (if a then TRUE else FALSE)  sol in
		  match E.V.b_of_var l with 
		    L((a1,a2,a3),(b1,b2,b3)) -> 
		      f (E.V.var_of_b (AL((a1,a2,a3),(b2,b3)))) 
			(f (E.V.var_of_b (AL((b1,b2,b3),(a1,a2))))
			   (f l sol))
		  | _ -> f l sol) a init)) l))
	    
	    
	let abstract rule a = E.V.varmap_add a ANY rule
	let kleenean_valuation_of_boolean_valuation rule varset = 
	  let rule = E.varset_of_boolean_valuation rule in 
	  E.V.fold_vars 
	    (fun x rho -> 
	      if E.V.varset_mem x rule 
	      then E.V.varmap_add x TRUE rho
	      else E.V.varmap_add x FALSE rho)
	    varset E.V.varmap_empty
	    
	let kleenean_rule_system_of_boolean_rule_system s varset = List.rev_map (fun (a,b,x) -> (a,b,kleenean_valuation_of_boolean_valuation x varset)) s
	let extract_kleenean_rule_system l = 
	  let a = List.rev_map 
	      (fun (s,c,a) -> ((s,c,a),
			     E.V.varmap_fold (fun x a l -> 
			       match a with ANY -> l | _ -> 
				 E.V.K.concat l [(E.V.string_of_var x)^":"^(match a with TRUE -> "T," | FALSE -> "F," | ANY  -> "?,")]) a []))
	      l in 
	  let m = list_fold (fun ((s,c,b),a) sol -> 
	    let old,c',b' = 
	      try (StringListMap.find a sol) 
	      with Not_found -> [],(empty_control:control),E.V.varmap_empty in 
	    StringListMap.add a (s@old,c,b) sol)
	      a StringListMap.empty in
	  StringListMap.fold (fun s a sol -> (a::sol)) m []

	let apply_context_control b e = 
	  list_fold 
	    (fun (b,bool) e ->
	      if (match b with B(a,a',a'') -> 
		(try ((E.V.varmap_find (E.V.var_of_b b) e)=TRUE) with _ -> false) or (try ((E.V.varmap_find (E.V.var_of_b (H(a,a'))) e)=TRUE) with _ -> false) or true 
	      |	 _ -> true)
	      then 
		E.V.varmap_add (E.V.var_of_b b) (if bool then TRUE else FALSE) e else e)
	    b.context_update  e
	    
        let print_kleenean_system  is_access (f:'a -> string)  g set  ss print_any sigma sigma2 ret log = 
	  
	  let g x = try (g x) with Not_found -> (print_string "WARN g";-1) in 
	  if ss = empty_kleenean_rule_system or 
	    List.for_all (fun (r,c,e) -> 
	      List.for_all (fun rid -> not (is_access rid))
		r) ss
	  then (
	    print_option empty_prefix log "#Cannot be applied \n\n";
	    List.fold_left 
	      (fun sol (r,c,e) -> 
	        if List.for_all (fun r -> false(*r.r_clone*)) r then sol else
		let id,b = list_fold 
		    (fun rid  (l,bool)  -> 
		      if false (*rid.r_clone*) then l,bool 
		      else 
			begin
			  (rid)::l,true
			end )
		    r ([],false) in 
		(id,["Cannot be applied"])::sol)
	      [] ss)
	  else 
	    let def_init = tuple_bot in 
	    List.fold_left 
	      (fun sol (r,c,e) -> 
		if List.for_all (fun r -> false (*r.r_clone*)) r then sol 
		else
		  if List.for_all (fun r -> not (is_access r)) r 
		  then  
		    (print_option empty_prefix 
		      log "#Cannot be applied \n\n";
		    if List.for_all (fun r -> false (*r.r_clone*)) r then sol else
			let id,b = 
			  list_fold 
			    (fun rid  (l,bool)  -> 
			      if false (*rid.r_clone*) then l,bool 
			      else 
				begin
				  (rid)::l,true
				end )
			    r ([],false) in 
			(id,["Cannot be applied"])::sol)
		      
		  else
		    let id,real_id,b = 
		      list_fold 
			(fun rid  (l,l2,bool)  -> 
			  if false (*rid.r_clone*) then l,l2,bool 
			  else 
			    begin
			      (if bool then 
				print_option empty_prefix log  ",");
		    	    print_option empty_prefix log ("'"^(f rid)^"'");
			      (f rid)::l,rid::l2,true
			    end )
			r ([],[],false) in 
		    let _ = if r<>[] then  print_option empty_prefix log " " else ()in 
		    let fadd_bound (a,b) bool (sol,n) = 
		      let old_sp = try (StringMap.find a sol) with Not_found -> 
			StringMap.empty in 
		      let tuple  = 
			try (StringMap.find b old_sp) with Not_found -> 
			  def_init in 
		      let tuple' = {tuple with is_bound = 
				     if bool = TRUE then Init true 
				     else if bool = FALSE then Init false
				     else Any } in 
		      let new_sp = StringMap.add b tuple'  old_sp in 
		      StringMap.add a new_sp sol,n in 
		    let fadd_link a b bool (sol,n) = 
		      if bool <> TRUE then (sol,n) 
		      else 
			let fadd (a,_,b)  sol = 
			  let old_sp = try (StringMap.find a sol) with Not_found -> 
			StringMap.empty in 
			  let tuple = 
			    try (StringMap.find b old_sp) with Not_found -> 
			      def_init in 
			  let tuple' = {tuple with link = Init (bound_of_number n)} in 
			  let new_sp = StringMap.add b tuple' old_sp in 
			  StringMap.add a new_sp sol in 
			fadd a  (fadd b  sol),n+1 in
		    let fadd_sign (a,b) s bool (sol,n) = 
		      match bool with 
			ANY -> sol,n 
		      | TRUE -> 
			  let old_sp = 
			    try (StringMap.find a sol) 
			    with Not_found -> 
			      StringMap.empty in 
			  let tuple = try (StringMap.find b old_sp) with Not_found -> 
			    def_init in 
			  let tuple' = {tuple with mark = Init s} in 
			  let new_sp = StringMap.add b tuple' old_sp in 
		      StringMap.add a new_sp sol,n
			
		  | FALSE -> 
		      let old_sp = 
			try (StringMap.find a sol) 
			with Not_found -> 
			  StringMap.empty in 
		      let tuple = try (StringMap.find b old_sp) with Not_found -> 
			def_init in 
		      let tuple' = {tuple with impossible_marks = 
				     match tuple.impossible_marks 
				     with Init l -> Init (s::l)
					 
				     | _ -> Init [s]} in 
		      let new_sp = StringMap.add b tuple' old_sp in 
		      StringMap.add a new_sp sol,n
		in 
		let fadd_here a bool (sol,(sol',n),cons,cons') = 
		  if (bool=TRUE) then 
		    (StringSet.add a sol,(
		     let old_sp = 
		       try (StringMap.find a sol') 
		       with Not_found -> 
			 StringMap.empty in 
		     StringMap.add a old_sp sol',n),cons,cons')
		  else (sol,(sol',n),cons,cons')  in
		let fadd a bool (hb,sol,cons,cons') = 
		  match E.V.b_of_var a with 
		    B(a,_,b) -> hb,fadd_bound (a,b) bool sol,cons,cons'
		  | H (a,_) -> (fadd_here a bool (hb,sol,cons,cons'))
		  | L(a,b) -> hb,fadd_link a b bool sol,cons,cons'
		  | M((a,_,b),s) -> hb,fadd_sign (a,b) s bool sol,cons,cons'
		  | Dis(a,b) -> hb,sol,((a,b)::cons),cons'
		  | Forb(a,l) -> hb,sol,cons,(a,l)::cons'
                  |_ -> hb,sol,cons,cons'  in 
     		let b1,(pretty,n),cons,consb = 
		  E.V.varmap_fold
		    fadd
		    e 
		    (StringSet.empty,
		     (StringMap.empty,1),
		     [],[]) in
		let b',(pretty',n'),cons',consb' = 
		  E.V.varmap_fold 
	            fadd 
		    (apply_context_control c e)
		    (StringSet.empty,(StringMap.empty,1),[],[]) in 
		let c',(pretty'',n''),cons'',consb'' = b',(pretty',n'),cons',consb' in 
	      let l =
		StringMap.fold
		  (fun a b sol -> (a,b)::sol) pretty [] in 
     	      let l = List.sort (fun (a,b) (c,d) -> compare (g a) (g c)) l in 
	      let _,b,n,s,compt,map = 
		list_fold 
		  (fun (a,b) 
		      (k,bool,n,olds,compt,map) -> 
			let b,string,n = 
			  print_pretty 
			    a 
			    (fun x -> StringSet.mem x b1  &&
				(not (List.exists (fun x' -> x=x')
				  c.remove)))
			    (pretty,n) tuple_data print_any
			    (if bool then 
			      (if (IntSet.mem (g k) set) then 
				(!Config_complx.solution_complex) 
			      else !Config_complx.solution_separator)
			    else "") 
			    sigma sigma2 
			    None log
			in a,b or bool,
			n,
			List.fold_left (fun s a -> a::s) olds (List.rev string),
			(if b then compt+1 else compt) ,
			if b then StringMap.add a compt map else map)
		  l ("",false,n,[],1,StringMap.empty)  in
	     
	      let s = 
		if b && c.remove <> [] 
		then (print_option empty_prefix log  (!Config_complx.solution_separator);
		      (!Config_complx.solution_separator)::s) else s in           
	      let _,b,n,s,compt,map = 
		list_fold 
		  (fun (a,b) 
		      (k,bool,n,olds,compt,map) -> 
			let b,string,n = 
			  print_pretty 
			    a 
			    (fun x -> StringSet.mem x b1  &&
				((List.exists (fun x' -> x=x')
				  c.remove)))
			    (pretty,n) tuple_data print_any
			    (if bool then 
			      (if (IntSet.mem (g k) set)
  then 
				(!Config_complx.solution_complex) 
			      else !Config_complx.solution_separator)
			    else "") 
			    sigma sigma2 
			    None log
			in a,b or bool,
			n,
			List.fold_left (fun s a -> a::s) olds (List.rev string),
			(if b then compt+1 else compt),
			if b then StringMap.add a compt map else map)
		  l ("",false,n,s,compt,map)  in
	       let s = 
		if cons = [] && consb = [] then s else 
		(let _ = print_option empty_prefix log "}" in
		"}")::
		(fst (List.fold_left 
			(fun (string,bool) ((a,_),l) -> 
			  List.fold_left
			    (fun (string,bool) l -> 
			      let s = (if bool then "," else "")^"$"^(string_of_int (try StringMap.find a map with Not_found -> 0))^"//"^l in 
			      let _ = print_option empty_prefix log s in 
			      s::string,true)
			(string,bool) l)
			(List.fold_left 
			   (fun (string,bool) ((a,_),(b,_)) 
			     -> (
			       let s = (if bool then "," else "")^"$"^(string_of_int (try StringMap.find a map with Not_found -> 0))
				 ^"<>$"^(string_of_int (try StringMap.find b map with Not_found -> 0))^(if bool then "," else "") in
			       let _ = print_option empty_prefix log s in
			       s::string,true))
			   (let _ = print_option empty_prefix log " {" in " {"::s,false) 
			   cons)
			consb))
	      in 
	      let _ = print_option empty_prefix log " -> "  in
	      let l = 
		StringMap.fold (fun a b sol -> (a,b)::sol) pretty' [] in 
	      let l = List.sort (fun (a,b) (c,d) -> compare (g a) (g c)) l in 
	      let k,b,n',string  = 
		list_fold
		  (fun (a,b) (k,bool,n,olds) ->
		    let b,string,n = 
		      print_pretty 
			a 
			(fun x -> StringSet.mem x c' &&
			  not ((List.exists (fun x' -> x=x')
				  c.remove) or List.exists (fun x' -> x=x') c.add)  )
			(pretty',n) 
			tuple_data 
			print_any 
			(if bool then 
			  (if (IntSet.mem (g k) set) then 
			    (!Config_complx.solution_complex) 
			  else !Config_complx.solution_separator)  else "") 
			sigma sigma2 None log in
		    a,b or bool,n,List.fold_left (fun s a -> a::s) olds (List.rev string)) l ("",false,n'," -> "::s) in 
	      let string = 
		if b && c.add <> [] 
		then (print_option empty_prefix log  (!Config_complx.solution_separator);
		      (!Config_complx.solution_separator)::string) else string in     
	      
	      let k,b,n,string = 
		list_fold
		  (fun (a,b) (k,bool,n,olds) ->
		    let b,string,n = 
		      print_pretty a (fun x -> StringSet.mem x c' &&
		      List.exists (fun x' -> x=x') c.add)
			(pretty',n) tuple_data print_any 
			(if bool 
			then 
			  (if (IntSet.mem (g k) set) then 
			    (!Config_complx.solution_complex) 
			  else !Config_complx.solution_separator)  
			else "") 
			sigma sigma2 None log in 
		    a,b or bool,n,List.fold_left (fun s a -> a::s) olds (List.rev string)) l ("",false,n',string) in 
	     
			    
			     
	let f x = () in 
	let _ = f k in 
	let _ = f b in 
	let _ = f n in 
	let _ = f n'' in
	let _ = f pretty'' in 
	let _ = f c' in 
	let _ = if ret then print_option empty_prefix log  "\n" else () in 
	((let l = 
	  List.sort compare real_id in
	let vide l = 
	  let rec aux residue last rep = 
	    match residue with 
	      t::q when t=last -> aux q last rep
	    | t::q -> aux q t (t::rep)
	    | [] -> List.rev rep
	  in match l with 
	    t::q -> aux q t [t] 
	  | [] -> [] in
	vide real_id,string)::sol))
	      [] ss

	    
	    end:Kleenean_Expr)))
