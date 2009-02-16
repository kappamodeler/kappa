%{  
  open Mods2
  open Agent
  open Rule
  open Data

  let error error i s = 
    error 
      (Some "kappa_parse.mly",
       Some i,
       Some s)
      s

  let error_runtime = error Error.runtime  
  let error_found = error Error.found 
    
  let hsh_add hsh (sol,k) = 
    let key = Solution.kappa_of_solution sol in
    let (sol',n) = try Hashtbl.find hsh key with Not_found -> (sol,0) in
      Hashtbl.replace hsh key (sol',n+k)

  let sol_of_hsh hsh = 
    Hashtbl.fold (fun _ (sol,n) init -> 
		    let init = (sol,n)::init
		    in
		      init
		 ) hsh !init
    
  (*Hashtable to control the type of the labels*)
  let (flag_env:(string,int) Hashtbl.t) = Hashtbl.create 100 (** flag->0 (type rule) 1 (type obs)*)

  let make_rule (lhs:Solution.t) (rhs:Solution.t) (beta,u_opt) (constraints:Rule.constraints list) = 
    let cc_map = Solution.split lhs in
    let n_cc = IntMap.size cc_map in
    let lhs,cc_map,empty_lhs = 
      if n_cc = 0 then 
	let sol_empty = Solution.add Agent.empty (Solution.empty()) in 
	  (sol_empty,IntMap.add 0 sol_empty cc_map,true)
      else
	(lhs,cc_map,false)
    in
    let rhs = if empty_lhs then Solution.insert_empty_agent rhs else rhs in
    let (diffs,add,rate,corr) = Solution.diff lhs rhs in
    let arrow = 
      match rate with
	  0 -> "~>" 
	| 1 -> "->" 
	| _ -> "=>"
    in
    let input_str = (Solution.kappa_of_solution lhs)^arrow^(Solution.kappa_of_solution rhs) in
    let _ = if empty_lhs then Error.warning (Printf.sprintf "Rule %s has no left hand side" input_str) else ()
    and _ = if n_cc > 2 then
      Error.warning 
	(Printf.sprintf "Rule %s has more than 2 possibly disconnected component in the left hand side" input_str)
    and _ = 
      match u_opt with 
	  None -> () 
	| Some _ -> 
	    if n_cc < 2 then 
	      Error.warning (Printf.sprintf "Unary exception rate defined for rule %s that is already unary" input_str) 
	    else ()
    in
    let precompil = 
      IntMap.fold (fun i cc_i map -> 
		     IntMap.add i (Solution.recognitions_of_cc cc_i) map
		  ) cc_map IntMap.empty 
    in
      {lhs = cc_map ;
       rhs = rhs;
       precompil = precompil;
       add = add ;
       actions = diffs ; 
       corr_ag = corr ;
       rate = rate ; 
       input = input_str;
       flag = None;
       constraints= constraints;
       kinetics = (let k = if beta<(-(1.0)) then 1.0 else beta in k *. (!rescale ** (1.0 -. (float_of_int n_cc)))) ;
       boost = (let k = if beta<(-(1.0)) then 1.0 else beta in k *. (!rescale ** (1.0 -. (float_of_int n_cc)))) ;
       automorphisms = None ;	
       n_cc = n_cc ;
       id = (incr rule_id ; !rule_id) ;
       infinite = (beta<(-(1.0))) ;
       abstraction = None;
       intra = u_opt 
      }

  let check_flag_obs flag = 
    let flg_type = 
      try Hashtbl.find flag_env flag 
      with Not_found -> error_found 80 ("observation "^flag^" is undefined")
    in
      if flg_type = 0 then error_found 82 (flag^" is a rule name, expecting an observation name") else ()
	
  let check_flag_rule flag = 
    let flg_type = 
      try Hashtbl.find flag_env flag 
      with Not_found -> error_found 87 ("rule "^flag^" is undefined")
    in
      if flg_type > 0 then 
	error_found 90 (flag^" is an observation name, expecting a rule name")
%}
%token INIT_LINE  OBS_LINE  STORY_LINE NEWLINE MODIF_LINE EOF
%token MULT DIVIDE PLUS MINUS COMMA SEMICOLON GREATER SMALLER SET INFINITY SEP
%token DO AT TIME
%token KAPPA_LNK KAPPA_WLD KAPPA_SEMI KAPPA_LRAR KAPPA_RAR
%token OP_PAR CL_PAR OP_CONC CL_CONC OP_ACC CL_ACC
%token <int> INT REF
%token <float> FLOAT 
%token <string> ID KAPPA_MRK LABEL
%left PLUS MINUS
%left COMMA
%left MULT DIVIDE

%start line
%type <unit> line 

%% /*Grammar rules*/

  line: 
| INIT_LINE init_expr {hsh_add !env $2}
| OBS_LINE obs_expr {obs_l := $2::(!obs_l)}
| STORY_LINE story_expr {obs_l := $2::(!obs_l)}
| MODIF_LINE modif_expr {exp := Experiment.add $2 (!exp)}
| NEWLINE {()}
| named_rule_expr {rules := (List.fold_left (fun rules r -> r::rules) (!rules) $1)} 
| EOF {let sol = sol_of_hsh !env in
         init := sol ;
	 raise End_of_file}
| error {raise (error_found 119 "syntax error")}
  ;

  modif_expr:
| concentration_ineq DO assignement NEWLINE {let dep,test,str1 = $1 and modif,str2 = $3 in {Experiment.dep=dep;
											    Experiment.test=test;
											    Experiment.modif=modif;
											    Experiment.test_str=str1;
											    Experiment.modif_str=str2
											   }
					    } 
| time_ineq DO assignement NEWLINE {let dep,test,str1 = $1 and modif,str2 = $3 in {Experiment.dep=dep;
										   Experiment.test=test;
										   Experiment.modif=modif ;
										   Experiment.test_str=str1;
										   Experiment.modif_str=str2
										  }
				   }
| error {error_found 137 "invalid modification"}
  ;

  assignement: 
| LABEL SET assign_expr {let assgn = $3 
			 and flag = $1 (*check here if flag corresponds to a real rule*)
			 in
			   check_flag_rule flag ;
			   let modif (oo,inf_list,rule_of_name,rules) = 
			     let ord = Experiment.eval (!rescale) assgn rule_of_name rules in
			     let i = StringMap.find flag rule_of_name in
			     let (r_i,inst_i) = Rule.Rule_of_int.find i rules in
			       match ord with
				   Experiment.INF -> 
				     let oo = IntSet.add i oo
				     and inf_list = if inst_i>0.1 then IntSet.add i inf_list else inf_list
				     in
				       (oo,inf_list,rules)
				 | Experiment.VAL kin' -> 
				     let oo = IntSet.remove i oo
				     and inf_list = IntSet.remove i inf_list
				     in
				       (oo,inf_list,Rule.Rule_of_int.add i ({r_i with Rule.kinetics = kin' ; Rule.boost = kin'}, inst_i) rules)
			   in 
			   let str = "kin("^flag^"):="^(Experiment.string_of_ast assgn) in 
			     (modif,str)
			}

| LABEL SET error {error_found 165 "invalid assignement"}
  ;

  assign_expr:
| OP_PAR assign_expr CL_PAR {$2}
| assign_val MULT assign_expr {Experiment.Mult($1,$3)}
| assign_val PLUS assign_expr {Experiment.Add($1,$3)}
| assign_val DIVIDE assign_expr {Experiment.Div($1,$3)}
| assign_val {$1}
  ;

  assign_val:
| FLOAT {Experiment.Val_float $1}
| INT {Experiment.Val_float (float_of_int $1)}
| INFINITY {Experiment.Val_infinity}
| LABEL {let flag = $1 in check_flag_rule flag ; Experiment.Val_kin flag }
  ;
  
  concentration_ineq:
| conc_expr GREATER conc_expr {let c1 = $1 and c2 = $3 in
			       let test (rule_of_name,rules) = 
				 let inst1 = (Experiment.eval (!rescale) c1 rule_of_name rules)  
				 and inst2 = (Experiment.eval (!rescale) c2 rule_of_name rules) 
				 in
				   Experiment.greater inst1 inst2 
			       and rule_flags = (Experiment.extract_dep c1)@(Experiment.extract_dep c2)
			       in
				 (*check here if rule flags corresponds to defined obervations --fake rules--*)
				 List.iter (fun flag -> check_flag_obs flag) rule_flags ;
				 let dep = Experiment.RULE_FLAGS rule_flags in
				 let str = (Experiment.string_of_ast c1)^">"^(Experiment.string_of_ast c2) in
				   (dep,test,str)
			      }

| conc_expr SMALLER conc_expr {let c1 = $1 and c2 = $3 in
			       let test (rule_of_name,rules) = 
				 let inst1 = Experiment.eval (!rescale) c1 rule_of_name rules (*/. (!rescale)*)
				 and inst2 = Experiment.eval (!rescale) c2 rule_of_name rules (*/. (!rescale)*)
				 in
				   Experiment.smaller inst1 inst2
			       and rule_flags = (Experiment.extract_dep c1)@(Experiment.extract_dep c2)
			       in
				 (*check here if rule flags corresponds to defined obervations --fake rules--*)
				 List.iter (fun flag -> check_flag_obs flag) rule_flags ;
				 let dep = Experiment.RULE_FLAGS rule_flags 
				 in
				   (*check here if rule flags corresponds to defined obervations --fake rules--*)
				 let str = (Experiment.string_of_ast c1)^"<"^(Experiment.string_of_ast c2) in
				   (dep,test,str)
			      }
  ;

  conc_expr:
| OP_PAR conc_expr CL_PAR {$2}
| conc_val MULT conc_expr {Experiment.Mult($1,$3)}
| conc_val DIVIDE conc_expr {Experiment.Div ($1,$3)}
| conc_val PLUS conc_expr {Experiment.Add($1,$3)}
| conc_val {$1}
  ;

  conc_val:
| FLOAT {Experiment.Val_float $1}
| INT {Experiment.Val_float (float_of_int $1)}
| OP_CONC LABEL CL_CONC {Experiment.Val_sol ("["^$2^"]")}
| OP_CONC error {error_found 229 "invalid concentration expression"}
  ;

  time_ineq:
| TIME GREATER FLOAT {let t0 = $3 in
		      let test _ = true 
		      and dep = Experiment.CURR_TIME t0
		      in
		      let str = "current time > "^(string_of_float t0) in
			(dep,test,str)
		     }
| TIME GREATER INT {let t0 = float_of_int $3 in
		    let test _ = true 
		    and dep = Experiment.CURR_TIME t0
		    in
		    let str = "current time > "^(string_of_float t0) in
		      (dep,test,str)
		   }
| TIME error {error_found 247 "invalid precondition"}
  ;

  init_expr:
| NEWLINE {(Solution.empty(),0)} 
| mult_sol_expr NEWLINE {$1}
| mult_sol_expr EOF {error_found 253 "missing end of line"}
  ;


  mult_sol_expr:
| INT MULT ne_sol_expr {let semi_bounds,_,sol = $3 in 
                        let coef = if !parse_coef or $1 = 0 then 
			  (int_of_float (float_of_int $1 *. (!rescale)))
			else 1 in
			  if not (IntMap.is_empty semi_bounds) then 
			    let str = 
			      String.concat "," (IntMap.fold (fun n _ cont -> string_of_int n::cont) semi_bounds []) 
			    in
			      error_found 266 (Printf.sprintf "dangling bound(s): {%s}." str)
			  else 
			    (*Solution.multiply sol coef*)
			    (sol,coef)
		       }
| FLOAT MULT ne_sol_expr {let semi_bounds,_,sol = $3 in 
			    if not (IntMap.is_empty semi_bounds) then 
			      let str = 
				String.concat "," (IntMap.fold (fun n _ cont -> string_of_int n::cont) semi_bounds []) 
			      in
				error_found 276 (Printf.sprintf "dangling bound(s): {%s}." str)
			    else
			      (*Solution.multiply sol (int_of_float ($1 *. (!rescale)))*)
			      (sol,int_of_float ($1 *. (!rescale)))
			 }
| ne_sol_expr {let semi_bounds,_,sol = $1 in 
		 if not (IntMap.is_empty semi_bounds) then 
		   let str = 
		     String.concat "," (IntMap.fold (fun n _ cont -> string_of_int n::cont) semi_bounds []) 
		   in
		     error_found 286 (Printf.sprintf "dangling bound(s): {%s}." str)
		 else 
		   (*Solution.multiply sol (int_of_float (!rescale))*)
		   (sol,int_of_float (!rescale))
	      }
      
  ;

  ne_sol_expr: /*non empty solution*/
| OP_PAR ne_sol_expr CL_PAR {$2}
| agent_expr {let semi_bounds_ag,ag = $1 in 
	      let semi_bounds_sol = IntMap.fold (fun n s map -> IntMap.add n (0,s) map) semi_bounds_ag IntMap.empty in
		(semi_bounds_sol,
		 IntSet.empty,
		 {Solution.agents = Solution.AA.add 0 ag (Solution.AA.create 1) ;
		  Solution.links = Solution.PA.create 1 ;
		  Solution.fresh_id = 1;
		 }
		)
	     }

| ne_sol_expr COMMA agent_expr {let (semi_bounds_ag,ag) = $3 and (semi_bounds_sol,used,sol) = $1 in
				let semi_bounds_sol,used,links =
				  IntMap.fold 
				    (fun n s (semi_bounds_sol,used,links) -> 
				       if IntSet.mem n used then 
					 error_found 312 
					   (Printf.sprintf "link %d is defined multiple times" n)
				       else
					 let i = sol.Solution.fresh_id in
					   try
					     let (i',s') = IntMap.find n semi_bounds_sol in
					     let semi_bounds_sol = IntMap.remove n semi_bounds_sol in
					     let links = 
					       Solution.PA.add (i,s) (i',s') 
						 (Solution.PA.add (i',s') (i,s) links) 
					     in
					     let used = IntSet.add n used 
					     in
					       (semi_bounds_sol,used,links)
					   with 
					       Not_found -> (IntMap.add n (i,s) semi_bounds_sol,used,links)
				    ) semi_bounds_ag (semi_bounds_sol,used,sol.Solution.links)
				in
				  (semi_bounds_sol,
				   used,
				   {Solution.agents = Solution.AA.add sol.Solution.fresh_id ag sol.Solution.agents ;
				    Solution.links = links ;
				    Solution.fresh_id = sol.Solution.fresh_id + 1;
				   }
				  )
			       }
  ;

  agent_expr:
| ID OP_PAR interface_expr CL_PAR {let semi_bound,state_of_site = $3 in 
				   let interface = 
				     Agent.fold_environment 
				       (fun site _ intf -> StringSet.add site intf) state_of_site 
				       (StringSet.add "_" (StringSet.empty)) 
				   in
				   let state_of_site = Agent.add_environment "_" (Agent.Wildcard,Agent.Free) state_of_site in
				   let ag = Agent.make $1 interface state_of_site in
				     (semi_bound,ag)
				  }
| ID OP_PAR interface_expr error {error_found 351 "mismatch parenthesis"}
  ;

  interface_expr: /*empty*/ {IntMap.empty,Agent.empty_environment}
| ID state_expr link_expr {let inf,lnk,semi_bounds =  
			     match ($2,$3) with
				 (inf,(lnk,Some n)) -> (inf,lnk,IntMap.add n $1 IntMap.empty)
			       | (inf,(lnk,None)) -> (inf,lnk,IntMap.empty)
			   in
			     (semi_bounds,Agent.add_environment  $1 (inf,lnk) Agent.empty_environment)
			  }
| ID state_expr link_expr COMMA interface_expr {let semi_bounds,smap = $5 in 
						  if Agent.mem_environment  $1 smap 
						  then error_found 364 
						    (Printf.sprintf "site %s is defined multiple times" $1)
						  else
						    let inf,lnk,semi_bounds =  
						      match ($2,$3) with
							  (inf,(lnk,Some n)) -> 
							    if IntMap.mem n semi_bounds 
							    then error_found 371 
							      (Printf.sprintf "link %d is defined multiple times" n)
							    else (inf,lnk,IntMap.add n $1 semi_bounds)
							| (inf,(lnk,None)) -> (inf,lnk,semi_bounds)
						    in
						      (semi_bounds,Agent.add_environment  $1 (inf,lnk) smap)
					       }
  ;

  state_expr: /*empty*/ {Agent.Wildcard}
| KAPPA_MRK {let mrk = $1 in Agent.Marked (String.sub mrk 1 ((String.length mrk)-1)) }
  ;

  link_expr: /*empty*/ {(Agent.Free,None)}
| KAPPA_LNK INT {(Agent.Bound,Some $2)}
| KAPPA_LNK KAPPA_SEMI {(Agent.Bound, None)}
| KAPPA_LNK error {error_found 387 "invalid link identifier"}
| KAPPA_WLD {(Agent.Wildcard,None)}
  ;

  
  obs_expr: 
| LABEL NEWLINE {let flag = $1 in (check_flag_rule flag ; Solution.Occurrence flag)} 
| ne_sol_expr NEWLINE {let (semi_bounds,_,sol) = $1 in 
			 if IntMap.is_empty semi_bounds 
			 then Solution.Concentration ("["^(Solution.kappa_of_solution sol)^"]",sol)
			 else 
			   let str =  
			     String.concat "," 
			       (IntMap.fold (fun n _ cont -> string_of_int n::cont) semi_bounds []) 
			   in
			     error_found 402 (Printf.sprintf "dangling bound(s): {%s}." str)
		      }
| LABEL ne_sol_expr NEWLINE {Hashtbl.replace flag_env ("["^$1^"]") 1 ;
			     let (semi_bounds,_,sol) = $2 in 
			       if IntMap.is_empty semi_bounds then Solution.Concentration ("["^$1^"]",sol)
			       else 
				 let str =  
				   String.concat "," 
				     (IntMap.fold (fun n _ cont -> string_of_int n::cont) semi_bounds []) 
				 in
				   error_found 412 (Printf.sprintf "dangling bound(s): {%s}." str)
			    } 
| LABEL EOF {error_found 414 "missing end of line"}
| ne_sol_expr EOF {error_found 415 "missing end of line"}
  ;

  story_expr: 
| LABEL NEWLINE {let flag = $1 in (check_flag_rule flag ; Solution.Story flag)} 
| LABEL EOF {error_found 420 "missing end of line"}
  ;
  
  named_rule_expr:
| LABEL rule_expr NEWLINE { match $2 with
				[r';r] -> (
				  Hashtbl.replace flag_env $1 0 ;
				  Hashtbl.replace flag_env ($1^"_op") 0 ;
				  [{r with flag = Some $1};{r' with flag = Some ($1^"_op")}]
				)
			      | [r] -> (Hashtbl.replace flag_env $1 0 ; [{r with flag = Some $1}])
			      | _ -> error_runtime 431 "Parser.named_rule_expr failure" 
			  }
| rule_expr NEWLINE {$1}
| LABEL rule_expr EOF {error_found 434 "missing end of line"}
| rule_expr EOF {error_found 435 "missing end of line"}
  ;


  sol_expr:  /*empty*/ {Solution.empty()}
| ne_sol_expr {let semi_bounds,_,sol = $1 in 
		 if not (IntMap.is_empty semi_bounds) then 
		   let str = 
		     String.concat "," (IntMap.fold (fun n _ cont -> string_of_int n::cont) semi_bounds []) 
		   in
		     error_found 445 (Printf.sprintf "dangling bound(s): {%s}." str)
		 else 
		   sol
	      }
  ;


  rule_expr:
| sol_expr KAPPA_RAR sol_expr kin_expr1 constraint_expr
      {let lhs = $1 and rhs = $3 and constraints = $5 and (k,u) = $4
       in 
	 [make_rule lhs rhs (k,u) constraints]
      }
| sol_expr KAPPA_LRAR sol_expr kin_expr2 constraint_expr
	  {let lhs = $1 and rhs = $3 and (k,u,k') = $4 and constraints = $5
	   in 
	     [make_rule rhs lhs (k',None) [] ; make_rule lhs rhs (k,u) constraints]
	  }
  ;

  constraint_expr: /*empty*/ {[]}
| OP_CONC cstr_list CL_CONC {$2}
  ;
  cstr_list: /*empty*/ {[]}
| ID {match $1 with "NO_HELIX" -> [Rule.NO_HELIX] | "NO_POLY" -> [Rule.NO_POLY] | s -> error_found 469 (Printf.sprintf "Unkown constraint %s" s)}
| ID SEMICOLON cstr_list {let hd=
			    match $1 with 
				"NO_HELIX" -> Rule.NO_HELIX 
			      | "NO_POLY" -> Rule.NO_POLY 
			      | s -> error_found 474 (Printf.sprintf "Unkown constraint %s" s)
			  in hd::$3 
			 }
  ;

  kin_expr1: /*empty*/ {(1.0,None) (*default kinetics*)}
| AT var_kin intra_rate {($2,$3)}
| AT error {error_found 481 "invalid kinetics rate"}
  ;

  kin_expr2: /*empty*/ {(1.0,None,1.0) (*default kinetics*)}
| AT var_kin intra_rate COMMA var_kin {($2,$3,$5)}
| AT error {error_found 486 "invalid kinetics rate"}
  ;

  var_kin:
| KAPPA_WLD {1.0}
| FLOAT {$1}
| INFINITY {-(10.0)}
| INT {float_of_int $1}
  ;

  intra_rate: /*empty*/ {None}
| OP_PAR FLOAT CL_PAR {Some $2}
| OP_PAR INT CL_PAR {Some (float_of_int $2)}
| OP_PAR INFINITY CL_PAR {error_found 499 "infinite rate for implicit unary rule is not allowed"}

%%
