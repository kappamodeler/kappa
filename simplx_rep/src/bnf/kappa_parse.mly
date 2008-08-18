%{  
  open Mods2
  open Agent
  open Rule
  open Data
  
  let env = Hashtbl.create 100

  let hsh_add hsh (sol,k) = 
    let key = Solution.kappa_of_solution sol in
    let (sol',n) = try Hashtbl.find hsh key with Not_found -> (sol,0) in
      Hashtbl.replace hsh key (sol',n+k)

  let sol_of_hsh hsh = 
    Hashtbl.fold (fun _ (sol,n) (init,pairs) -> 
		    let init =
		      if !compile_mode then init
		      else
			Solution.compose (Solution.multiply sol n) init
		    and pairs = (sol,n)::pairs
		    in
		      (init,pairs)
		 ) hsh (!init,!pairs) 
    
  (*Hashtable to control the type of the labels*)
  let (flag_env:(string,int) Hashtbl.t) = Hashtbl.create 100 (** flag->0 (type rule) 1 (type obs)*)

  let make_rule (lhs:Solution.t) (rhs:Solution.t) k (constraints:Solution.constraints list) is_infinite = 
    let (diffs,add,rate,corr) = Solution.diff lhs rhs in
    let arrow = 
      match rate with
	  0 -> "~>" 
	| 1 -> "->" 
	| _ -> "=>"
    in
    let input_str = (Solution.kappa_of_solution lhs)^arrow^(Solution.kappa_of_solution rhs) in
    let cc_map = Solution.split lhs in
    let n_cc = IntMap.size cc_map in
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
       kinetics = k *. (!rescale ** (1.0 -. (float_of_int n_cc))) ;
       automorphisms = None ;	
       n_cc = n_cc ;
       id = (incr rule_id ; !rule_id) ;
       infinite = is_infinite ;
       abstraction = None;
      }

  let check_flag_obs flag = 
    let flg_type = 
      try Hashtbl.find flag_env flag 
      with Not_found -> Error.before ("observation "^flag^" is undefined")
    in
      if flg_type = 0 then Error.after (flag^" is a rule name, expecting an observation name") else ()
	
  let check_flag_rule flag = 
    let flg_type = 
      try Hashtbl.find flag_env flag 
      with Not_found -> Error.after ("rule "^flag^" is undefined")
    in
      if flg_type > 0 then 
	Error.before (flag^" is an observation name, expecting a rule name")

  type atom_constraint = JOINT of int * int | DISJOINT of int * int | SEPARATE of int * string

  let rec make_constraints hsh = function
      [] -> Hashtbl.fold (fun i (joints,disjoints,sep) cont -> Solution.CC(i,joints,disjoints,sep)::cont) hsh []
    | cons::tl -> 
	begin
	  match cons with
	      JOINT(i,j) -> 
		let joints,disjoints,sep = try Hashtbl.find hsh i with Not_found -> (IntSet.empty,IntSet.empty,StringSet.empty)
		in
		  Hashtbl.replace hsh i (IntSet.add j joints,disjoints,sep) ;
		  make_constraints hsh tl
	    | DISJOINT(i,j) ->
		let joints,disjoints,sep = try Hashtbl.find hsh i with Not_found -> (IntSet.empty,IntSet.empty,StringSet.empty)
		in
		  Hashtbl.replace hsh i (joints,IntSet.add j disjoints,sep) ;
		  make_constraints hsh tl
	    | SEPARATE(i,name) ->
		let joints,disjoints,sep = try Hashtbl.find hsh i with Not_found -> (IntSet.empty,IntSet.empty,StringSet.empty)
		in
		  Hashtbl.replace hsh i (joints,disjoints,StringSet.add name sep) ;
		  make_constraints hsh tl
	end

%}

%token INIT_LINE  OBS_LINE  STORY_LINE NEWLINE MODIF_LINE EOF
%token MULT DIVIDE PLUS MINUS COMMA GREATER SMALLER DIFF EQUIV SET INFINITY SEP
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
| INIT_LINE init_expr {hsh_add env $2}
| OBS_LINE obs_expr {obs_l := $2::(!obs_l)}
| STORY_LINE story_expr {obs_l := $2::(!obs_l)}
| MODIF_LINE modif_expr {exp := Experiment.add $2 (!exp)}
| NEWLINE {()}
| named_rule_expr {rules := (List.fold_left (fun rules r -> r::rules) (!rules) $1)} 
| EOF {let sol,assoc = sol_of_hsh env in
         init := sol ; pairs := assoc ;
	 raise End_of_file}
| error {raise (Error.before "syntax error")}
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
| error {Error.after "invalid modification"}
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
				       (oo,inf_list,Rule.Rule_of_int.add i ({r_i with Rule.kinetics = kin'}, inst_i) rules)
			   in 
			   let str = "kin("^flag^"):="^(Experiment.string_of_ast assgn) in 
			     (modif,str)
			}

| LABEL SET error {Error.before "invalid assignement"}
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
| OP_CONC error {Error.before "invalid concentration expression"}
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
| TIME error {Error.before "invalid precondition"}
  ;

  init_expr:
| NEWLINE {(Solution.empty(),0)} 
| mult_sol_expr NEWLINE {$1}
| mult_sol_expr EOF {Error.before "missing end of line"}
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
			      Error.after (Printf.sprintf "dangling bound(s): {%s}." str)
			  else 
			    (*Solution.multiply sol coef*)
			    (sol,coef)
		       }
| FLOAT MULT ne_sol_expr {let semi_bounds,_,sol = $3 in 
			    if not (IntMap.is_empty semi_bounds) then 
			      let str = 
				String.concat "," (IntMap.fold (fun n _ cont -> string_of_int n::cont) semi_bounds []) 
			      in
				Error.after (Printf.sprintf "dangling bound(s): {%s}." str)
			    else
			      (*Solution.multiply sol (int_of_float ($1 *. (!rescale)))*)
			      (sol,int_of_float ($1 *. (!rescale)))
			 }
| ne_sol_expr {let semi_bounds,_,sol = $1 in 
		 if not (IntMap.is_empty semi_bounds) then 
		   let str = 
		     String.concat "," (IntMap.fold (fun n _ cont -> string_of_int n::cont) semi_bounds []) 
		   in
		     Error.after (Printf.sprintf "dangling bound(s): {%s}." str)
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
					 Error.before 
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
| ID OP_PAR interface_expr error {Error.before "mismatch parenthesis"}
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
						  then Error.before 
						    (Printf.sprintf "site %s is defined multiple times" $1)
						  else
						    let inf,lnk,semi_bounds =  
						      match ($2,$3) with
							  (inf,(lnk,Some n)) -> 
							    if IntMap.mem n semi_bounds 
							    then Error.before 
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
| KAPPA_LNK error {Error.before "invalid link identifier"}
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
			     Error.after (Printf.sprintf "dangling bound(s): {%s}." str)
		      }
| LABEL ne_sol_expr NEWLINE {Hashtbl.replace flag_env ("["^$1^"]") 1 ;
			     let (semi_bounds,_,sol) = $2 in 
			       if IntMap.is_empty semi_bounds then Solution.Concentration ("["^$1^"]",sol)
			       else 
				 let str =  
				   String.concat "," 
				     (IntMap.fold (fun n _ cont -> string_of_int n::cont) semi_bounds []) 
				 in
				   Error.after (Printf.sprintf "dangling bound(s): {%s}." str)
			    } 
| LABEL EOF {Error.before "missing end of line"}
| ne_sol_expr EOF {Error.before "missing end of line"}
  ;

  story_expr: 
| LABEL NEWLINE {let flag = $1 in (check_flag_rule flag ; Solution.Story flag)} 
| LABEL EOF {Error.before "missing end of line"}
  ;
  
  named_rule_expr:
| LABEL rule_expr NEWLINE { match $2 with
				[r';r] -> (
				  Hashtbl.replace flag_env $1 0 ;
				  Hashtbl.replace flag_env ($1^"_op") 0 ;
				  [{r with flag = Some $1};{r' with flag = Some ($1^"_op")}]
				)
			      | [r] -> (Hashtbl.replace flag_env $1 0 ; [{r with flag = Some $1}])
			      | _ -> Error.runtime "Parser.named_rule_expr failure" 
			  }
| rule_expr NEWLINE {$1}
| LABEL rule_expr EOF {Error.before "missing end of line"}
| rule_expr EOF {Error.before "missing end of line"}
  ;


  lhs_rhs_expr:  /*empty*/ {Solution.empty()}
| ne_sol_expr {let semi_bounds,_,sol = $1 in 
		 if not (IntMap.is_empty semi_bounds) then 
		   let str = 
		     String.concat "," (IntMap.fold (fun n _ cont -> string_of_int n::cont) semi_bounds []) 
		   in
		     Error.after (Printf.sprintf "dangling bound(s): {%s}." str)
		 else 
		   sol
	      }
  ;


  rule_expr:
| lhs_rhs_expr constraints KAPPA_RAR lhs_rhs_expr kin_expr1 
      {let lhs = $1 and rhs = $4 and constraints = $2 and (k,inf) = if $5<0.0 then (1.0,true) else ($5,false)
       in 
	 [make_rule lhs rhs k constraints inf]
      }
| lhs_rhs_expr constraints KAPPA_LRAR lhs_rhs_expr constraints kin_expr2 
	  {let lhs = $1 and rhs = $4 and constraints1 = $2 and constraints2 = $5 
	   in 
	   let k,k' = $6 in 
	     if !forward then 
	       let k,inf = if k<0.0 then (1.0,true) else (k,false) in
		 [make_rule lhs rhs k constraints1 inf] 
	     else
	       let k,inf = if k<0.0 then (1.0,true) else (k,false)
	       and k',inf'= if k'<0.0 then (1.0,true) else (k',false)
	       in
		 [make_rule rhs lhs k' constraints2 inf'; make_rule lhs rhs k constraints1 inf]}
  ;

  constraints: /*empty*/ {[]}
| OP_ACC constraint_expr CL_ACC {make_constraints (Hashtbl.create 20) $2}
| OP_ACC error {Error.before (Printf.sprintf "malformed constraint expression")}
  ;

  constraint_expr: 
| cstr {[$1]}
| cstr COMMA constraint_expr {$1::$3}
  ;

  cstr:
| REF EQUIV REF  {JOINT($1-1,$3-1)}
| REF DIFF REF {DISJOINT($1-1,$3-1)}
| REF SEP ID {SEPARATE($1-1,$3)}
  ;

  kin_expr1: /*empty*/ {1.0 (*default kinetics*)}
| AT FLOAT {$2}
| AT INT {float_of_int $2}
| AT INFINITY {-(1.0)}
| AT error {Error.after "invalid kinetics rate"}
  ;

  kin_expr2: /*empty*/ {(1.0,1.0)}
| AT var_kin COMMA var_kin {($2,$4)}
| AT error {Error.after "invalid pair of kinetics rates"}
  ;

  var_kin:
| KAPPA_WLD {1.0}
| FLOAT {$1}
| INFINITY {-(1.0)}
| INT {float_of_int $1}
  ;

%%
