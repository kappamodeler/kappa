%{  
  open Mods2
  open Agent
  open Rule
  open Data
  open Data_structures_metaplx

  let error error i s = 
    error 
      (Some "kappa_parse.mly",
       Some i,
       Some s)
      s

let line = ref 0
    
let error_runtime = error Error.runtime  
let error_found x y = 
    let y = "Line: "^(string_of_int ((!line)+1))^": "^y in 
    error Error.found x y 

      

  
  let sol_of_hsh hsh = 
    Hashtbl.fold (fun _ (sol,n) init -> 
		    let init = (sol,n)::init
		    in
		      init
		 ) hsh !init
    
  (*Hashtable to control the type of the labels*)
  let (flag_env:(string,int) Hashtbl.t) = Hashtbl.create 100 (** flag->0 (type rule) 1 (type obs)*)


%}
%token INIT_LINE  OBS_LINE  STORY_LINE NEWLINE MODIF_LINE GEN_LINE CONC_LINE EOF
%token MULT DIVIDE PLUS MINUS COMMA SEMICOLON GREATER SMALLER SET EQUAL INFINITY SEP
%token DO AT TIME
%token KAPPA_LNK KAPPA_WLD KAPPA_SEMI KAPPA_LRAR KAPPA_RAR
%token OP_PAR CL_PAR OP_CONC CL_CONC OP_ACC CL_ACC
%token <int> INT REF
%token <float> FLOAT 
%token <string> ID KAPPA_MRK LABEL 
%token <string> COMMENT
%left PLUS MINUS
%left COMMA
%left MULT DIVIDE

%start main
%type <Data_structures_metaplx.parse list> main

%% /*Grammar rules*/
main: 
  line main {$1::$2}
| line {[$1]}
| EOF {[]}
  
  line: 
| INIT_LINE init_expr {let _ = line:=(!line)+1 in 
                       let a,b = $2 in INIT_L(a,"%init:"^b,!line)}
| OBS_LINE obs_expr   {let _ = line:=(!line)+1 in $2}
| STORY_LINE story_expr {let _ = line:=(!line)+1 in $2}
| MODIF_LINE modif_expr {let _ = line:=(!line)+1 in DONT_CARE_L("%mod"^$2,!line)}
| GEN_LINE gen_expr {let _ = line:=(!line)+1 in 
                     let (a,b,c,d,e) = $2 in 
		     GEN_L((a,b,c,d,"#%gen: "^e),!line)}
| CONC_LINE gen_expr {let _ = line:=(!line)+1 in 
                      let (a,b,c,d,e) = $2 in 
		      CONC_L((a,b,c,d,"#%conc: "^e),!line)}
| newline2 {let _ = line:=(!line)+1 in DONT_CARE_L($1,!line)}
| named_rule_expr {let _ = line:=(!line)+1 in RULE_L($1,!line)}
| error {raise (error_found 119 "syntax error")}
  ;

  modif_expr:
| concentration_ineq DO assignement newline {$1^" do "^$3^$4}
| time_ineq DO assignement newline {$1^" do "^$3^$4}
| error {error_found 137 "invalid modification"}
  ;

  assignement: 
| LABEL SET assign_expr {"'"^$1^"' := "^$3}
| LABEL SET error {error_found 165 "invalid assignement"}
  ;

  assign_expr:
| OP_PAR assign_expr CL_PAR {"("^$2^")"}
| assign_val MULT assign_expr {$1^"*"^$3}
| assign_val PLUS assign_expr {$1^"+"^$3}
| assign_val DIVIDE assign_expr {$1^"/"^$3}
| assign_val {$1}
  ;

  assign_val:
| FLOAT {string_of_float $1}
| INT {string_of_int $1}
| INFINITY {"$INF"}

| LABEL {"'$1' "}
  ;
  
  concentration_ineq:
| conc_expr GREATER conc_expr {$1^">"^$3}
| conc_expr SMALLER conc_expr {$1^"<"^$3}
  ;

  conc_expr:
| OP_PAR conc_expr CL_PAR {"("^$2^")"}
| conc_val MULT conc_expr {$1^"*"^$3}
| conc_val DIVIDE conc_expr {$1^"/"^$3}
| conc_val PLUS conc_expr {$1^"+"^$3}
| conc_val {$1}
  ;

  conc_val:
| FLOAT {string_of_float $1}
| INT {string_of_int $1}
| OP_CONC LABEL CL_CONC {"["^$2^"]"}
| OP_CONC error {error_found 229 "invalid concentration expression"}
  ;

  time_ineq:
| TIME GREATER FLOAT {"$T >"^(string_of_float $3)}
| TIME GREATER INT {"$T <"^(string_of_int  $3)}
| TIME error {error_found 247 "invalid precondition"}
  ;

  init_expr:
| newline {[],$1}
| mult_sol_expr newline {let a,b = $1 in (a,b^($2))}
  ;


  mult_sol_expr:
| INT MULT ne_sol_expr {let a,b = $3 in (a,(string_of_int $1)^"*"^b)}
| FLOAT MULT ne_sol_expr {let a,b = $3 in (a,(string_of_float $1)^"*"^b)}
| ne_sol_expr {$1}
  ;

  ne_sol_expr: /*non empty solution*/
| OP_PAR ne_sol_expr CL_PAR {let a,b=$2 in a,"("^b^")"}
| agent_expr {let a,b=$1 in [a],b}
| ne_sol_expr COMMA agent_expr {let (a,b),(c,d) = $1,$3 in 
                                   c::a,b^","^d}
  ;

  agent_expr:
| ID OP_PAR interface_expr CL_PAR {let a,b = $3 in ($1,a),$1^"("^b^")"}
| ID OP_PAR interface_expr error {error_found 351 "mismatch parenthesis"}
  ;

  interface_expr: /*empty*/ {[],""}
| ID state_expr link_expr {[$1,($2^$3)],$1^$2^$3}
| ID state_expr link_expr COMMA interface_expr {let a,b=$5 in 
                                                ($1,($2^$3))::a,$1^$2^$3^","^b}
  ;

  state_expr: /*empty*/ {""}
| KAPPA_MRK {$1}
  ;

  link_expr: /*empty*/ {""}
| KAPPA_LNK INT {"!"^(string_of_int $2)}
| KAPPA_LNK KAPPA_SEMI {"!_"}
| KAPPA_LNK error {error_found 387 "invalid link identifier"}
| KAPPA_WLD {"?"}
  ;

  
  obs_expr: 
| LABEL newline {OBS_L($1,"%obs: '"^$1^"'"^$2,!line)}
| ne_sol_expr newline {DONT_CARE_L("%obs: "^(snd $1)^$2,!line)}
| LABEL ne_sol_expr newline {DONT_CARE_L("%obs: '"^$1^"' "^(snd $2)^$3,!line)}
  ;

  story_expr: 
| LABEL newline {STORY_L($1,"%story: '"^$1^"'"^$2,!line)}
  ;
  
  named_rule_expr:
| LABEL rule_expr newline {$1,let (a,b,c,d,e)=$2 in (a,b,c,d,e^$3)}
| rule_expr newline {"Auto"^(string_of_int (!line)) ,let (a,b,c,d,e)=$1 in (a,b,c,d,e^$2)}
  ;


  sol_expr:  /*empty*/ {[],""}
| ne_sol_expr {$1}
  ;


  rule_expr:
| sol_expr KAPPA_RAR sol_expr kin_expr1 constraint_expr
      {$1,"->",$3,$4,$5}
| sol_expr KAPPA_LRAR sol_expr kin_expr2 constraint_expr
    {$1,"<->",$3,$4,$5}
  ;

  constraint_expr: /*empty*/ {""}
| OP_CONC cstr_list CL_CONC {"["^$2^"]"}
  ;
  cstr_list: /*empty*/ {""}
| ID {$1}
| ID SEMICOLON cstr_list {$1^";"^$3}
  ;

  kin_expr1: /*empty*/ {"" (*default kinetics*)}
| AT var_kin intra_rate {"@"^$2^" "^$3}
| AT error {error_found 481 "invalid kinetics rate"}
  ;

  kin_expr2: /*empty*/ {"" (*default kinetics*)}
| AT var_kin intra_rate COMMA var_kin {"@"^$2^" "^$3^","^$5}
| AT error {error_found 486 "invalid kinetics rate"}
  ;

  var_kin:
| KAPPA_WLD {"?"}
| FLOAT {string_of_float $1}
| INFINITY {"$INF"}
| INT {string_of_int $1}
  ;

  intra_rate: /*empty*/ {""}
| OP_PAR FLOAT CL_PAR {"("^(string_of_float $2)^")"}
| OP_PAR INT CL_PAR {"("^(string_of_int $2)^")"}
| OP_PAR INFINITY CL_PAR {error_found 499 "infinite rate for implicit unary rule is not allowed"}

  gen_expr : 
    agent_expr newline {Some $1,None,None,[],(snd $1)^$2}
|   ID EQUAL ID newline {None,Some $1,Some $3,[],($1^" = "^$3^$4)}
|   ID EQUAL ID OP_CONC instruction_list CL_CONC newline 
    {None,Some $1,Some $3,(fst $5),($1^" = "^$3^"["^(snd $5)^"]"^$7)}

  instruction_list: 
    /*empty*/ {[],""}
| instruction_ne_list {$1}


 instruction_ne_list: 
    instruction {[fst $1],snd $1}
| instruction instruction_ne_list {(fst $1)::(fst $2),(snd $1)^" "^(snd $2)}

  instruction:
| PLUS ID {Data_structures_metaplx.Add_site $2,"+ "^$2}
| MINUS ID {Data_structures_metaplx.Delete_site $2,"- "^$2}
| ID DIVIDE OP_ACC id_list CL_ACC {Data_structures_metaplx.Rename ($1,fst $4),$1^"\\{"^(snd $4)^"\\}"}
| ID SET ID {Data_structures_metaplx.Mutate_site($1,$3),$1^" := "^$3}

  id_list: 
    /*empty*/ {[],""}
| id_non_empty_list {$1} 

id_non_empty_list:
   ID {[$1],$1}
| ID id_non_empty_list {$1::(fst $2),$1^" "^(snd $2)} 


newline: 
  NEWLINE {"\n"}
| COMMENT {$1}
| EOF {"\n"}

newline2: 
    NEWLINE {"\n"}
| COMMENT {$1}
