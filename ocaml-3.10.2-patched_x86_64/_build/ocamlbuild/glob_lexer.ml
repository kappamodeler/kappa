# 15 "ocamlbuild/glob_lexer.mll"
 
open Bool;;
open Glob_ast;;

type token =
| ATOM of pattern atom
| AND
| OR
| NOT
| LPAR
| RPAR
| TRUE
| FALSE
| EOF
;;

let sf = Printf.sprintf;;

let concat_patterns p1 p2 =
  match (p1,p2) with
  | (Epsilon,_) -> p2
  | (_,Epsilon) -> p1
  | (_,_)       -> Concat(p1,p2)
;;

let slash = Class(Atom('/','/'));;
let not_slash = Class(Not(Atom('/','/')));;
let any = Class True;;

# 32 "ocamlbuild/glob_lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\245\255\005\000\247\255\248\255\249\255\001\000\250\255\
    \000\000\251\255\001\000\001\000\252\255\000\000\001\000\253\255\
    \003\000\003\000\254\255\255\255\000\000\000\000\001\000\000\000\
    \001\000\000\000\000\000\004\000\002\000\083\000\244\255\000\000\
    \001\000\000\000\162\000\002\000\000\000\003\000\029\000\030\000\
    \001\000\048\000\000\000\006\000\001\000\002\000";
  Lexing.lex_backtrk = 
   "\255\255\255\255\009\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\008\000\
    \009\000\003\000\000\000\255\255\005\000\007\000\255\255\002\000\
    \003\000\255\255\003\000\003\000\000\000\255\255";
  Lexing.lex_default = 
   "\255\255\000\000\255\255\000\000\000\000\000\000\255\255\000\000\
    \255\255\000\000\255\255\255\255\000\000\255\255\255\255\000\000\
    \255\255\255\255\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\030\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\039\000\039\000\
    \255\255\042\000\255\255\255\255\255\255\015\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\002\000\002\000\000\000\002\000\002\000\002\000\002\000\
    \000\000\002\000\002\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\000\000\018\000\018\000\000\000\002\000\015\000\000\000\
    \004\000\003\000\037\000\035\000\036\000\045\000\045\000\009\000\
    \005\000\007\000\005\000\045\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\019\000\000\000\000\000\019\000\
    \255\255\016\000\000\000\000\000\015\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\010\000\013\000\
    \023\000\021\000\012\000\000\000\009\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\043\000\015\000\000\000\
    \000\000\017\000\026\000\018\000\015\000\007\000\006\000\005\000\
    \000\000\000\000\000\000\000\000\027\000\000\000\011\000\014\000\
    \022\000\020\000\024\000\012\000\008\000\009\000\025\000\028\000\
    \000\000\040\000\255\255\000\000\012\000\031\000\009\000\000\000\
    \034\000\034\000\032\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\044\000\000\000\000\000\
    \000\000\000\000\001\000\000\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\033\000\000\000\
    \000\000\000\000\034\000\000\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\018\000\034\000\
    \034\000\000\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\000\000\000\000\000\000\
    \001\000\034\000\255\255\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\000\000\000\000\002\000\002\000\
    \255\255\002\000\002\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\040\000\255\255\002\000\000\000\255\255\
    \000\000\000\000\031\000\032\000\035\000\042\000\044\000\036\000\
    \000\000\000\000\037\000\043\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\038\000\
    \039\000\000\000\255\255\255\255\021\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\000\000\
    \010\000\016\000\013\000\255\255\023\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\041\000\033\000\255\255\
    \255\255\000\000\006\000\043\000\020\000\025\000\000\000\028\000\
    \255\255\255\255\255\255\255\255\026\000\255\255\000\000\000\000\
    \011\000\017\000\008\000\014\000\000\000\022\000\024\000\027\000\
    \255\255\038\000\039\000\255\255\000\000\029\000\000\000\255\255\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\041\000\255\255\255\255\
    \255\255\255\255\029\000\255\255\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\255\255\
    \255\255\255\255\029\000\255\255\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\034\000\
    \034\000\255\255\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\255\255\255\255\255\255\
    \000\000\034\000\045\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\038\000\039\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \041\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\029\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 49 "ocamlbuild/glob_lexer.mll"
                  ( ATOM(Pattern(let (p,_) = parse_pattern ['>'] Epsilon lexbuf in p)) )
# 184 "ocamlbuild/glob_lexer.ml"

  | 1 ->
# 50 "ocamlbuild/glob_lexer.mll"
                  ( ATOM(Constant(parse_string (Buffer.create 32) lexbuf)) )
# 189 "ocamlbuild/glob_lexer.ml"

  | 2 ->
# 51 "ocamlbuild/glob_lexer.mll"
                  ( AND )
# 194 "ocamlbuild/glob_lexer.ml"

  | 3 ->
# 52 "ocamlbuild/glob_lexer.mll"
                  ( OR )
# 199 "ocamlbuild/glob_lexer.ml"

  | 4 ->
# 53 "ocamlbuild/glob_lexer.mll"
                  ( NOT )
# 204 "ocamlbuild/glob_lexer.ml"

  | 5 ->
# 54 "ocamlbuild/glob_lexer.mll"
                  ( TRUE )
# 209 "ocamlbuild/glob_lexer.ml"

  | 6 ->
# 55 "ocamlbuild/glob_lexer.mll"
                  ( FALSE )
# 214 "ocamlbuild/glob_lexer.ml"

  | 7 ->
# 56 "ocamlbuild/glob_lexer.mll"
                  ( LPAR )
# 219 "ocamlbuild/glob_lexer.ml"

  | 8 ->
# 57 "ocamlbuild/glob_lexer.mll"
                  ( RPAR )
# 224 "ocamlbuild/glob_lexer.ml"

  | 9 ->
# 58 "ocamlbuild/glob_lexer.mll"
                  ( token lexbuf )
# 229 "ocamlbuild/glob_lexer.ml"

  | 10 ->
# 59 "ocamlbuild/glob_lexer.mll"
                  ( EOF )
# 234 "ocamlbuild/glob_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and parse_pattern eof_chars p lexbuf =
    __ocaml_lex_parse_pattern_rec eof_chars p lexbuf 29
and __ocaml_lex_parse_pattern_rec eof_chars p lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 62 "ocamlbuild/glob_lexer.mll"
                     u
# 246 "ocamlbuild/glob_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 62 "ocamlbuild/glob_lexer.mll"
                        ( parse_pattern eof_chars (concat_patterns p (Word u)) lexbuf )
# 250 "ocamlbuild/glob_lexer.ml"

  | 1 ->
# 64 "ocamlbuild/glob_lexer.mll"
  (
    let rec loop pl =
      let (p',c) = parse_pattern ['}';','] Epsilon lexbuf in
      let pl = p' :: pl in
      if c = ',' then
        loop pl
      else
        parse_pattern eof_chars (concat_patterns p (Union pl)) lexbuf
    in
    loop []
  )
# 265 "ocamlbuild/glob_lexer.ml"

  | 2 ->
# 76 "ocamlbuild/glob_lexer.mll"
  (
    let cl = Not(Or(parse_class [] lexbuf)) in
    parse_pattern eof_chars (concat_patterns p (Class cl)) lexbuf
  )
# 273 "ocamlbuild/glob_lexer.ml"

  | 3 ->
# 81 "ocamlbuild/glob_lexer.mll"
  (
    let cl = Or(parse_class [] lexbuf) in
    parse_pattern eof_chars (concat_patterns p (Class cl)) lexbuf
  )
# 281 "ocamlbuild/glob_lexer.ml"

  | 4 ->
# 87 "ocamlbuild/glob_lexer.mll"
  ( let q = Union[slash; Concat(slash, Concat(Star any, slash)) ] in
    parse_pattern eof_chars (concat_patterns p q) lexbuf )
# 287 "ocamlbuild/glob_lexer.ml"

  | 5 ->
# 90 "ocamlbuild/glob_lexer.mll"
  ( let q = Union[Epsilon; Concat(slash, Star any)] in
    parse_pattern eof_chars (concat_patterns p q) lexbuf )
# 293 "ocamlbuild/glob_lexer.ml"

  | 6 ->
# 93 "ocamlbuild/glob_lexer.mll"
  ( let q = Union[Epsilon; Concat(Star any, slash)] in
    parse_pattern eof_chars (concat_patterns p q) lexbuf )
# 299 "ocamlbuild/glob_lexer.ml"

  | 7 ->
# 95 "ocamlbuild/glob_lexer.mll"
       ( raise (Parse_error("Ambiguous ** pattern not allowed unless surrounded by one or more slashes")) )
# 304 "ocamlbuild/glob_lexer.ml"

  | 8 ->
# 96 "ocamlbuild/glob_lexer.mll"
      ( parse_pattern eof_chars (concat_patterns p (Star not_slash)) lexbuf )
# 309 "ocamlbuild/glob_lexer.ml"

  | 9 ->
# 97 "ocamlbuild/glob_lexer.mll"
      ( parse_pattern eof_chars (concat_patterns p slash) lexbuf )
# 314 "ocamlbuild/glob_lexer.ml"

  | 10 ->
# 98 "ocamlbuild/glob_lexer.mll"
      ( parse_pattern eof_chars (concat_patterns p not_slash) lexbuf )
# 319 "ocamlbuild/glob_lexer.ml"

  | 11 ->
let
# 99 "ocamlbuild/glob_lexer.mll"
       c
# 325 "ocamlbuild/glob_lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 100 "ocamlbuild/glob_lexer.mll"
  ( if List.mem c eof_chars then 
      (p,c)
    else
      raise (Parse_error(sf "Unexpected character %C in glob pattern" c))
  )
# 333 "ocamlbuild/glob_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_parse_pattern_rec eof_chars p lexbuf __ocaml_lex_state

and parse_string b lexbuf =
    __ocaml_lex_parse_string_rec b lexbuf 38
and __ocaml_lex_parse_string_rec b lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 107 "ocamlbuild/glob_lexer.mll"
                        ( Buffer.contents b )
# 344 "ocamlbuild/glob_lexer.ml"

  | 1 ->
# 108 "ocamlbuild/glob_lexer.mll"
                        ( Buffer.add_char b '"'; parse_string b lexbuf )
# 349 "ocamlbuild/glob_lexer.ml"

  | 2 ->
let
# 109 "ocamlbuild/glob_lexer.mll"
                  u
# 355 "ocamlbuild/glob_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 109 "ocamlbuild/glob_lexer.mll"
                        ( Buffer.add_string b u; parse_string b lexbuf )
# 359 "ocamlbuild/glob_lexer.ml"

  | 3 ->
let
# 110 "ocamlbuild/glob_lexer.mll"
       c
# 365 "ocamlbuild/glob_lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 110 "ocamlbuild/glob_lexer.mll"
                        ( raise (Parse_error(sf "Unexpected character %C in string" c)) )
# 369 "ocamlbuild/glob_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_parse_string_rec b lexbuf __ocaml_lex_state

and parse_class cl lexbuf =
    __ocaml_lex_parse_class_rec cl lexbuf 41
and __ocaml_lex_parse_class_rec cl lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 113 "ocamlbuild/glob_lexer.mll"
                          ( cl )
# 380 "ocamlbuild/glob_lexer.ml"

  | 1 ->
# 114 "ocamlbuild/glob_lexer.mll"
                          ( ((Atom('-','-'))::cl) )
# 385 "ocamlbuild/glob_lexer.ml"

  | 2 ->
let
# 115 "ocamlbuild/glob_lexer.mll"
        c1
# 391 "ocamlbuild/glob_lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos
and
# 115 "ocamlbuild/glob_lexer.mll"
                      c2
# 396 "ocamlbuild/glob_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2) in
# 115 "ocamlbuild/glob_lexer.mll"
                          ( parse_class ((Atom(c1,c2))::cl) lexbuf )
# 400 "ocamlbuild/glob_lexer.ml"

  | 3 ->
let
# 116 "ocamlbuild/glob_lexer.mll"
       c
# 406 "ocamlbuild/glob_lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 116 "ocamlbuild/glob_lexer.mll"
                          ( parse_class ((Atom(c,c))::cl) lexbuf )
# 410 "ocamlbuild/glob_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_parse_class_rec cl lexbuf __ocaml_lex_state

;;

