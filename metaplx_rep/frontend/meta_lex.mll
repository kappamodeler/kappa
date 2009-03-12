{
 open Lexing
 open Error
 open Meta_parse
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
  (((['0'-'9']+ '.' ['0'-'9']*) | (['0'-'9']* '.' ['0'-'9']+)) ((['e' 'E'] ['+' '-'] ['0'-'9']+) | (['e' 'E'] ['0'-'9']+))) 
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
    | "%begin_macro:" {BEGIN_MAC_LINE}
    | "%end_macro:" {END_MAC_LINE}
    | "%expand:" {EXPAND_MAC_LINE}
    | "do" {DO}
    | "\\\n" {incr_line lexbuf ; token lexbuf} 
    | '\n' {incr_line lexbuf ; NEWLINE}
    | '#' {let comment = comment "#" lexbuf in COMMENT comment}
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
    | '~' {KAPPA_MRK}
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

  and comment acc = parse
    | '\n' {incr_line lexbuf ; (Printf.sprintf "%s%c" acc '\n')}
    | "\\\n" {incr_line lexbuf ; comment 
		 (Printf.sprintf 
		    "%s%s" 
		    acc 
		    "\\\n"
		    ) 
		 lexbuf}
    | eof {incr_line lexbuf ; (Printf.sprintf "%s%c" acc '\n')}
    | _ as c {comment (Printf.sprintf "%s%c" acc c) lexbuf}

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
	

}
