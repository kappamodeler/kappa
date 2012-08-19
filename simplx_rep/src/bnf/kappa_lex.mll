{
 open Lexing
 open Error
 open Kappa_parse
 open Data

 let reach_eof lexbuf = 
   lexbuf.lex_eof_reached <- true 

 let reset_eof lexbuf = 
   lexbuf.lex_eof_reached <- false 

 let incr_line lexbuf = 
   let pos = lexbuf.lex_curr_p in
     lexbuf.lex_curr_p <- {pos with pos_lnum = pos.pos_lnum+1 ; pos_bol = pos.pos_cnum}

 let return_error lexbuf msg = 
   let pos = lexbuf.lex_curr_p in
   let line = pos.pos_lnum in
   let full_msg =
     Printf.sprintf "in %s: %s" pos.pos_fname msg
   in
     Error.syntax 
       (Some "kappa_lex.mll",
	Some 19,
	Some (full_msg^" line "^(string_of_int line)))
     (full_msg,line)

 let return_error_before lexbuf msg = 
   let pos = lexbuf.lex_curr_p in
   let line = pos.pos_lnum - 1 in
   let full_msg =
     Printf.sprintf "in %s: %s" pos.pos_fname msg
   in
     Error.syntax 
       (Some "kappa_lex.mll",
	Some 37,
	Some (full_msg^" line "^(string_of_int line)))
     (full_msg,line)

}

let blank = [' ' '\t' '\r']
let integer = (['0'-'9']+)
let real = 
  (((['0'-'9'] | ['0'-'9']+ '.' ['0'-'9']*) | (['0'-'9']* '.' ['0'-'9']+)) ((['e' 'E'] ['+' '-'] ['0'-'9']+) | (['e' 'E'] ['0'-'9']+))) 
  | ((['0'-'9']+ '.' ['0'-'9']*) | (['0'-'9']* '.' ['0'-'9']+))   
let id = (['a'-'z' 'A'-'Z' '0'-'9'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '^' '-']*)
let internal_state = '~' (['0'-'9' 'a'-'z' 'A'-'Z']+)


  rule token = parse
    | "%init:" {INIT_LINE}
    | "%obs:"  {OBS_LINE}
    | "%story:" {STORY_LINE}
    | "%causal:" {STORY_LINE}
    | "%mod:" {MODIF_LINE}
    | "%gen:" {GEN_LINE}
    | "%conc:" {CONC_LINE}
    | "%var:" {VAR_LINE}
    | "do" {DO}
    | "\\\n" {incr_line lexbuf ; token lexbuf} 
    | '\n' {incr_line lexbuf ; NEWLINE}
    | '#' {comment lexbuf}
    | integer as n {INT(int_of_string n)}
    | real as f {FLOAT(float_of_string f)}
    | '\'' {let lab = read_label "" lexbuf in LABEL lab}
    | id as str {ID(str)}
    | '@' {AT}
    | ',' {COMMA}
    | '(' {OP_PAR}
    | ')' {CL_PAR}
    | '[' {OP_CONC}
    | ']' {CL_CONC}
    | '{' {OP_ACC}
    | '}' {CL_ACC}
    | '+' {PLUS}
    | '-' {MINUS}
    | '*' {MULT}
    | '/' {DIVIDE}
    | '!' {KAPPA_LNK}
    | internal_state as s {KAPPA_MRK s}
    | '?' {KAPPA_WLD}
    | '_' {KAPPA_SEMI}
    | "->" {KAPPA_RAR}
    | "<->" {KAPPA_LRAR}
    | '>' {GREATER}
    | '<' {SMALLER}
    | ":=" {SET}
    | "=" {EQUAL}
    | "=>" {IMPLY} 
    | '&' {AND}
    | '|' {OR}
    | "$INF" {INFINITY}
    | "$T" {TIME}
    | "$M" {MIXTURE}
    | "!PAUSE" {PAUSE}
    | "!STOP" {KILL}
    | "!DUMP" {DUMP}
    | blank  {token lexbuf}
    | eof {reach_eof lexbuf;EOF}
    | _ as c {return_error lexbuf (Printf.sprintf "invalid use of character %c" c)}

  and read_label acc = parse
    | '\n' {return_error lexbuf (Printf.sprintf "invalid rule label")}
    | eof {return_error lexbuf (Printf.sprintf "invalid rule label")}
    | "\\\n" {incr_line lexbuf ; read_label acc lexbuf} 
    | '\'' {acc}
    | _ as c {read_label (Printf.sprintf "%s%c" acc c) lexbuf}

  and comment = parse
    | '\n' {incr_line lexbuf ; NEWLINE}
    | "\\\n" {incr_line lexbuf ; comment lexbuf} 
    | eof {EOF}
    | _ {comment lexbuf}

{ 
  let init_val = 
    fun () -> 
      begin
	rule_id:=0;
	rules:=[]; 
	init:=[];
	obs_l:=[] ;
	exp:=Experiment.empty ;
	env := Hashtbl.create 100 
      end
	
  let sol_of_hsh hsh = 
    Hashtbl.fold (fun _ (sol,n) init -> 
		    let init = (sol,n)::init
		    in
		      init
		 ) hsh !init
    

  let test_eof lexbuf = 
    if lexbuf.lex_eof_reached 
    then 
      begin
        let sol = sol_of_hsh !env in
          init := sol ;
	  raise End_of_file
      end

  let compile fic =
    init_val() ;
    let d = open_in fic in
      try
	let lexbuf = Lexing.from_channel d in
	  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = fic} ;
	  while true do
            let _ = test_eof lexbuf in 
	      try
	        Kappa_parse.line token lexbuf 
	      with 
		  Error.Found msg -> return_error lexbuf msg 
		| Error.Not_valid_semantics msg -> return_error_before lexbuf msg
	  done ; 
	  let s = "Lexer.compile: unexpected end of loop" in
	    Error.runtime
	      (Some "kappa_lex.mll",
	       Some 112,
	       Some s)
	      s
      with End_of_file -> (close_in d ; 
			   let rules = 
			     if (!compilation_opt land _PARSE_RULES) = _PARSE_RULES then !rules (*serialized file contains true rules*)
			     else
			       if !load_sim_data then []
			       else
				 try
				   let d = open_in_bin (!serialized_rule_file) in
				   let f_rules = (Marshal.from_channel d:Rule.marshalized_t list)
				   in
				     print_string ("-Reading rule set from "^(!serialized_mixture_file)^"...") ;
				     close_in d;
				     List.map (fun f_r -> Rule.unmarshal f_r) f_rules
				 with
				     exn -> 
				       let s = (Printf.sprintf "Lexer.compile: could not load %s (returned %s)" !serialized_rule_file (Printexc.to_string exn)) in
					 Error.runtime (None,None,None) s
			   and init,init_l =
			     if (!compilation_opt land _PARSE_INIT) = _PARSE_INIT then !init,!init_l
			     else 
			       if !load_sim_data or !map_mode or !compile_mode then [],[]
			       else
				 if Sys.file_exists !serialized_mixture_file then
				   let d = open_in_bin !serialized_mixture_file in
				   let f_init = (Marshal.from_channel d:(Solution.marshalized_t * int) list) in
				     print_string ("-Reading initial mixture from "^(!serialized_mixture_file)^"...") ;
				     close_in d;
				     let a=List.map (fun (f_sol,n) -> (Solution.unmarshal f_sol,n)) f_init
                                     in a,a
				 else Error.runtime (None,None,None) ("Cannot find "^(!serialized_mixture_file))
			   and obs_l = if !load_sim_data then [] else !obs_l
			   in
			   let _ = 
			     if !save_rules then
			       let d = open_out_bin !serialized_rule_file in
				 print_string ("-Saving rule set in "^(!serialized_rule_file)^"...") ;
				 Marshal.to_channel d (List.map (fun r -> Rule.marshal r) rules) [];
				 close_out d
			   in
			     (rules,init,init_l,obs_l,!exp)
			  )
    
  let make_rule rule_str = 
    init_val() ;
    try
      let lexbuf = Lexing.from_string (rule_str^"\n") in
      let _ = reset_eof lexbuf in 
	while true do
          let _ = test_eof lexbuf in 
	  try
	    Kappa_parse.line token lexbuf
	  with 
	      Error.Found msg -> return_error lexbuf msg 
	done ; 
      let s = "Lexer.compile: unexpected end of loop" in 
      Error.runtime 
	(Some "kappa_lex.mll",
	 Some 131,
	 Some s)
	s
    with End_of_file -> (!rules)

  let make_init sol_str = 
    init_val() ;
    try
      let lexbuf = Lexing.from_string ("%init:"^sol_str^"\n") in
      let _ = reset_eof lexbuf in 
	while true do
          let _ = test_eof lexbuf in 
	  try
	    Kappa_parse.line token lexbuf
	  with 
	      Error.Found msg -> return_error lexbuf msg 
	done ; 
      let s = "Lexer.compile: unexpected end of loop" in
      Error.runtime 
	(Some "kappa_lex.mll",
	 Some 149,
	 Some s)
	s
    with End_of_file -> (!init)
}
