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
%token INIT_LINE  OBS_LINE  STORY_LINE NEWLINE MODIF_LINE GEN_LINE CONC_LINE BEGIN_MAC_LINE END_MAC_LINE EXPAND_MAC_LINE EOF
%token MULT DIVIDE PLUS MINUS COMMA SEMICOLON GREATER SMALLER SET EQUAL INFINITY SEP
%token INSTANCE DO AT TIME
%token KAPPA_LNK KAPPA_WLD KAPPA_SEMI KAPPA_LRAR KAPPA_RAR
%token OP_PAR CL_PAR OP_CONC CL_CONC OP_ACC CL_ACC
%token <int> INT REF
%token <float> FLOAT 
%token <string> ID 
%token KAPPA_MRK
%token <string> LABEL 
%token <string> COMMENT
%left PLUS MINUS
%left COMMA
%left MULT DIVIDE

%start main
%type <Data_structures_metaplx.pp_parse list> main

%% /*Grammar rules*/
main: 
  line main {$1::$2}
| line {[$1]}
| EOF {[]}
  
  line: 
| INIT_LINE init_expr 
    {let _ = line:=(!line)+1 in 
    PP_INIT_L((fun f -> 
      let a,b = $2 f in a,"%init:"^b),!line)}
| OBS_LINE obs_expr   {let _ = line:=(!line)+1 in $2}
| STORY_LINE story_expr {let _ = line:=(!line)+1 in $2}
| MODIF_LINE modif_expr {let _ = line:=(!line)+1 in PP_DONT_CARE_L((fun f -> "%mod"^($2 f)),!line)}
| GEN_LINE gen_expr {let _ = line:=(!line)+1 in 
                     let (a,b,c,d,e) = $2 in 
		     PP_GEN_L((fun f -> a f,b f,c f,d f,"#%gen: "^(e f)),!line)}
| CONC_LINE gen_expr {let _ = line:=(!line)+1 in 
                      let (a,b,c,d,e) = $2 in 
		      PP_CONC_L((fun f -> a f,b f,c f,d f,"#%conc: "^(e f)),!line)}
| BEGIN_MAC_LINE macro_def {let _ = line:=(!line)+1 in 
                            let a,b,c = $2 in 
			    PP_BMAC_L((fun f -> a f, b f,"#%begin_macro: "^(c f)),!line)}
| END_MAC_LINE newline {let _ = line:=(!line)+1 in PP_EMAC_L((fun f -> "#%end_macro: "^($2 f)),!line)}
    
| EXPAND_MAC_LINE macro_call {let _ = line:=(!line)+1 in 
                                let a,b,c = $2 in 
			    PP_CMAC_L((fun f -> a f, b f,"#%expand: "^(c f)),!line)}
| newline2 {let _ = line:=(!line)+1 in PP_DONT_CARE_L($1,!line)}
| named_rule_expr {let _ = line:=(!line)+1 in PP_RULE_L($1,!line)}
| error {raise (error_found 119 "syntax error")}
  ;

  modif_expr:
| concentration_ineq DO assignement newline {(fun f -> ($1 f)^" do "^($3 f)^($4 f))}
| time_ineq DO assignement newline {fun f -> ($1 f)^" do "^($3 f)^($4 f)}
| error {error_found 137 "invalid modification"}
  ;

  assignement: 
| LABEL SET assign_expr {fun f -> "'"^$1^"' := "^($3 f)}
| LABEL SET error {error_found 165 "invalid assignement"}
  ;

  assign_expr:
| OP_PAR assign_expr CL_PAR {fun f -> "("^($2 f)^")"}
| assign_val MULT assign_expr {fun f -> ($1 f)^"*"^($3 f)}
| assign_val PLUS assign_expr {fun f -> ($1 f)^"+"^($3 f)}
| assign_val DIVIDE assign_expr {fun f -> ($1 f)^"/"^($3 f)}
| assign_val {$1}
  ;

  assign_val:
| FLOAT {fun f-> string_of_float $1}
| INT {fun f -> string_of_int $1}
| INFINITY {fun f -> "$INF"}

| LABEL {fun f -> "'"^(f ($1,""))^"'"}
| id    {$1}
  ;
  
  concentration_ineq:
| conc_expr GREATER conc_expr {fun f -> ($1 f)^">"^($3 f)}
| conc_expr SMALLER conc_expr {fun f -> ($1 f)^"<"^($3 f)}
  ;

  conc_expr:
| OP_PAR conc_expr CL_PAR {fun f -> "("^($2 f)^")"}
| conc_val MULT conc_expr {fun f -> ($1 f)^"*"^($3 f)}
| conc_val DIVIDE conc_expr {fun f -> ($1 f)^"/"^($3 f)}
| conc_val PLUS conc_expr {fun f -> ($1 f)^"+"^($3 f)}
| conc_val {$1}
  ;

  conc_val:
| FLOAT {fun f -> string_of_float $1}
| INT {fun f -> string_of_int $1}
| OP_CONC LABEL CL_CONC {fun f -> "["^(f ($2,""))^"]"}
| OP_CONC error {error_found 229 "invalid concentration expression"}
  ;

  time_ineq:
| TIME GREATER FLOAT {fun f -> "$T >"^(string_of_float $3)}
| TIME GREATER INT {fun f -> "$T <"^(string_of_int  $3)}
| TIME error {error_found 247 "invalid precondition"}
  ;

  init_expr:
| newline {fun f -> [],$1 f}
| mult_sol_expr newline {fun f -> let a,b = ($1 f) in (a,b^($2 f))}
  ;


  mult_sol_expr:
| INT MULT ne_sol_expr {fun f -> let a,b = $3 f in (a,(string_of_int $1)^"*"^b)}
| FLOAT MULT ne_sol_expr {fun f -> let a,b = $3 f in (a,(string_of_float $1)^"*"^b)}
| ne_sol_expr {$1}
  ;

  ne_sol_expr: /*non empty solution*/
| OP_PAR ne_sol_expr CL_PAR {fun f -> let a,b=$2 f in a,"("^b^")"}
| agent_expr {fun f -> let a,b=$1 f in [a],b}
| ne_sol_expr COMMA agent_expr {fun f -> let (a,b),(c,d) = $1 f ,$3 f in 
                                   c::a,b^","^d}
  ;

  agent_expr:
| id OP_PAR interface_expr CL_PAR {fun f -> let a,b = $3 f in ($1 f,a),($1 f)^"("^b^")"}
| id OP_PAR interface_expr error {error_found 351 "mismatch parenthesis"}
  ;

  interface_expr: /*empty*/ {fun f -> [],""}
| id state_expr link_expr {fun f-> [$1 f,(($2 f)^($3 f))],($1 f)^($2 f)^($3 f)}
| id state_expr link_expr COMMA interface_expr {fun f -> let a,b=$5 f in 
                                                ($1 f,(($2 f)^($3 f)))::a,($1 f)^($2 f)^($3 f)^","^b}
  ;

  state_expr: /*empty*/ {fun f -> ""}
| KAPPA_MRK id {fun f -> "~"^($2 f)}
  ;

  link_expr: /*empty*/ {fun f -> ""}
| KAPPA_LNK INT {fun f -> "!"^(string_of_int $2)}
| KAPPA_LNK KAPPA_SEMI {fun f -> "!_"}
| KAPPA_LNK error {error_found 387 "invalid link identifier"}
| KAPPA_WLD {fun f -> "?"}
  ;

  
  obs_expr: 
| LABEL newline {PP_OBS_L((fun f -> let a = f ($1,"") in a,"%obs: '"^a^"'"^($2 f)),!line)}
| ne_sol_expr newline {PP_DONT_CARE_L((fun f -> 
    let a,b = $1 f in 
    "%obs: "^(b)^($2 f)),!line)}
| LABEL ne_sol_expr newline {PP_DONT_CARE_L((fun f -> 
    let a,b =  $2 f in 
    "%obs: '"^(f ($1,""))^"' "^b^($3 f)),!line)}
  ;

  story_expr: 
| LABEL newline {PP_STORY_L((fun f -> let a = f ($1,"")in a,"%story: '"^a^"'"^($2 f)),!line)}
  ;
  
  named_rule_expr:
| LABEL rule_expr newline {fun f -> f ($1,""),let (a,b,c,d,e)=$2 f  in (a,b,c,d,e^($3 f))}
| rule_expr newline {let l = (!line)+1 in fun f -> f ("Auto"^(string_of_int l),"") ,let (a,b,c,d,e)=$1 f in (a,b,c,d,e^($2 f))}
  ;


  sol_expr:  /*empty*/ {fun f-> [],""}
| ne_sol_expr {$1}
  ;


  rule_expr:
| sol_expr KAPPA_RAR sol_expr kin_expr1 constraint_expr
      {fun f -> $1 f ,"->",$3 f,$4 f,$5 f}
| sol_expr KAPPA_LRAR sol_expr kin_expr2 constraint_expr
    {fun f -> $1 f,"<->",$3 f,$4 f,$5 f}
  ;

  constraint_expr: /*empty*/ {fun f -> ""}
| OP_CONC cstr_list CL_CONC {fun f -> "["^($2 f)^"]"}
  ;
  cstr_list: /*empty*/ {fun f -> ""}
| id {$1}
| id SEMICOLON cstr_list {fun f -> ($1 f)^";"^($3 f)}
  ;

  kin_expr1: /*empty*/ {fun f -> "" (*default kinetics*)}
| AT var_kin intra_rate {fun f -> "@"^($2 f)^" "^($3 f)}
| AT error {error_found 481 "invalid kinetics rate"}
  ;

  kin_expr2: /*empty*/ {fun f -> "" (*default kinetics*)}
| AT var_kin intra_rate COMMA var_kin {fun f-> "@"^($2 f)^" "^($3 f)^","^($5 f)}
| AT error {error_found 486 "invalid kinetics rate"}
  ;

  var_kin:
| KAPPA_WLD {fun f -> "?"}
| FLOAT {fun f -> string_of_float $1}
| INFINITY {fun f -> "$INF"}
| INT {fun f -> string_of_int $1}
  ;

  intra_rate: /*empty*/ {fun f -> ""}
| OP_PAR FLOAT CL_PAR {fun f -> "("^(string_of_float $2)^")"}
| OP_PAR INT CL_PAR {fun f -> "("^(string_of_int $2)^")"}
| OP_PAR INFINITY CL_PAR {error_found 499 "infinite rate for implicit unary rule is not allowed"}

  gen_expr : 
    agent_expr newline 
    {(fun f -> Some ($1 f)),
      (fun f -> None),
      (fun f -> None),
      (fun f -> []),
      fun f -> (snd ($1 f))^($2 f)}
|   id EQUAL id newline 
    {(fun f -> None),
      (fun f -> Some ($1 f)),
      (fun f -> Some ($3 f)),
      (fun f -> []),
      (fun f -> (($1 f)^" = "^($3 f)^($4 f)))}
|   id EQUAL id OP_CONC instruction_list CL_CONC newline 
    {(fun f -> None),
      (fun f -> Some ($1 f)),
      (fun f -> Some ($3 f)),
      (fun f -> (fst ($5 f))),
      fun f -> (($1 f)^" = "^($3 f)^"["^(snd ($5 f))^"]"^($7 f))}

  instruction_list: 
    /*empty*/ {fun f -> ([],"")}
| instruction_ne_list {$1}


 instruction_ne_list: 
    instruction {fun f -> [fst ($1 f)],snd ($1 f)}
| instruction instruction_ne_list 
    {fun f -> (fst ($1 f))::(fst ($2 f)),(snd ($1 f))^" "^(snd ($2 f))}

  instruction:
| PLUS id {fun f -> Data_structures_metaplx.Add_site ($2 f),"+"^($2 f)}
| MINUS id {fun f -> Data_structures_metaplx.Delete_site ($2 f),"-"^($2 f)}
| id DIVIDE OP_ACC id_list CL_ACC {fun f -> Data_structures_metaplx.Rename ($1 f,fst ($4 f)),($1 f)^"\\{"^(snd ($4 f))^"\\}"}
| id SET id {fun f -> Data_structures_metaplx.Mutate_site($1 f,$3 f),($1 f)^" := "^($3 f)}

  id_list: 
    /*empty*/ {fun f -> [],""}
| id_non_empty_list {$1} 

id_non_empty_list:
   id {fun f -> [$1 f],($1 f)}
| id id_non_empty_list {fun f-> ($1 f)::(fst ($2 f)),($1 f)^" "^(snd ($2 f))} 


newline: 
  NEWLINE {(fun f -> "\n")}
| COMMENT {(fun f -> $1)}
| EOF {fun f -> "\n"}

newline2: 
    NEWLINE {fun f -> "\n"}
| COMMENT {fun f -> $1}

id:
    ID {fun f -> f ($1,"")}
| ID INSTANCE INT {fun f -> f ($1,string_of_int $3)}

macro_def: 
    id OP_PAR idlistcomma CL_PAR newline {(fun f -> ($1 f)),
					   (fun f -> fst ($3 f)),
					   fun f -> ($1 f)^"("^(snd ($3 f))^")"}

macro_call: 
    id OP_PAR idlistlist CL_PAR newline  {(fun f -> ($1 f)),
					   (fun f -> fst ($3 f)),
					   (fun f -> ($1 f)^"("^(snd ($3 f))^")")}

idlistcomma: 
    /*empty*/ {fun f -> [],""}
|   ne_idlistcomma {$1} 

ne_idlistcomma:
    id {fun f -> [$1 f],$1 f}
| id COMMA ne_idlistcomma {fun f -> ($1 f)::(fst ($3 f)),($1 f)^" , "^(snd ($3 f))}


idlist: 
    /*empty*/ {fun f -> [],""}
|   ne_idlist {$1}

ne_idlist:
    id {fun f -> [$1 f],$1 f}
| id ne_idlist {fun f -> ($1 f)::(fst ($2 f)),($1 f)^(snd ($2 f))}




idlistlist:
    /*empty*/ {fun f -> [],""}
| ne_idlistlist {$1}

ne_idlistlist:
    ne_idlist {fun f -> [fst ($1 f)],snd ($1 f) }
| idlist COMMA idlistlist {fun f -> (fst ($1 f))::(fst ($3 f)),(snd ($1 f))^" , "^(snd ($3 f))}

