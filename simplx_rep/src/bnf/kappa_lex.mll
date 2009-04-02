{
 open Lexing
 open Error
 open Kappa_parse
 open Data

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
    | "%mod:" {MODIF_LINE}
    | "%gen:" {GEN_LINE}
    | "%conc:" {CONC_LINE}
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
    | "$T" {TIME}
    | ":=" {SET}
    | "=" {EQUAL} 
    | '$' (integer as i) {REF(int_of_string i)}
    | "$INF" {flush stdout ; INFINITY}
    | blank  {token lexbuf}
    | eof {EOF}
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
	env := Hashtbl.create 100 
      end
	
  let compile fic =
    init_val() ;
    let d = open_in fic in
      try
	let lexbuf = Lexing.from_channel d in
	  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = fic} ;
	  while true do
	    try
	      Kappa_parse.line token lexbuf 
	    with 
		Error.Found msg -> return_error lexbuf msg 
	  done ; 
	let s = "Lexer.compile: unexpected end of loop" in
	Error.runtime
	  (Some "kappa_lex.mll",
	   Some 112,
	   Some s)
	  s
      with End_of_file -> (close_in d ; (!rules,!init,!obs_l,!exp))
    
	
  let make_rule rule_str = 
    init_val() ;
    try
      let lexbuf = Lexing.from_string (rule_str^"\n") in
	while true do
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
	while true do
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
