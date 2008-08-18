type token =
  | ARGUMENT of (string)
  | LIDENT of (string)
  | UIDENT of (string)
  | OPERATOR of (string)
  | INTEGER of (int64)
  | STAR
  | MINUS
  | DOT
  | SHARP
  | AT
  | DOLLAR
  | BANG
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | EOL

open Parsing;;
# 17 "parser.mly"

open Int64ops
open Primitives
open Input_handling
open Longident
open Parser_aux

# 30 "parser.ml"
let yytransl_const = [|
  262 (* STAR *);
  263 (* MINUS *);
  264 (* DOT *);
  265 (* SHARP *);
  266 (* AT *);
  267 (* DOLLAR *);
  268 (* BANG *);
  269 (* LPAREN *);
  270 (* RPAREN *);
  271 (* LBRACKET *);
  272 (* RBRACKET *);
  273 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* ARGUMENT *);
  258 (* LIDENT *);
  259 (* UIDENT *);
  260 (* OPERATOR *);
  261 (* INTEGER *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\003\000\003\000\004\000\005\000\006\000\
\007\000\007\000\020\000\020\000\008\000\008\000\009\000\009\000\
\021\000\021\000\021\000\022\000\022\000\019\000\010\000\010\000\
\011\000\012\000\012\000\013\000\013\000\014\000\023\000\023\000\
\023\000\023\000\023\000\023\000\023\000\023\000\023\000\015\000\
\015\000\016\000\016\000\016\000\016\000\016\000\017\000\017\000\
\018\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\002\000\001\000\002\000\002\000\001\000\
\002\000\001\000\002\000\001\000\002\000\001\000\002\000\001\000\
\001\000\003\000\001\000\001\000\003\000\002\000\001\000\001\000\
\002\000\001\000\001\000\001\000\000\000\002\000\001\000\001\000\
\002\000\003\000\005\000\005\000\003\000\002\000\003\000\002\000\
\001\000\001\000\001\000\002\000\004\000\004\000\003\000\001\000\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\049\000\050\000\002\000\
\000\000\051\000\000\000\052\000\005\000\000\000\053\000\000\000\
\054\000\008\000\055\000\000\000\056\000\010\000\000\000\014\000\
\057\000\000\000\000\000\058\000\012\000\016\000\023\000\024\000\
\059\000\000\000\060\000\026\000\061\000\027\000\028\000\062\000\
\000\000\063\000\017\000\020\000\019\000\032\000\000\000\000\000\
\000\000\064\000\041\000\031\000\000\000\000\000\000\000\043\000\
\065\000\042\000\000\000\000\000\048\000\066\000\067\000\068\000\
\000\000\001\000\003\000\004\000\006\000\007\000\009\000\013\000\
\011\000\015\000\025\000\030\000\033\000\038\000\000\000\000\000\
\000\000\040\000\000\000\044\000\000\000\022\000\039\000\018\000\
\021\000\037\000\034\000\000\000\000\000\000\000\000\000\047\000\
\000\000\000\000\045\000\046\000\036\000\035\000"

let yydgoto = "\020\000\
\023\000\026\000\028\000\031\000\033\000\035\000\037\000\041\000\
\044\000\055\000\051\000\053\000\056\000\058\000\066\000\073\000\
\078\000\038\000\080\000\046\000\068\000\069\000\070\000"

let yysindex = "\102\000\
\004\255\024\255\010\255\031\255\054\255\063\255\040\255\013\255\
\027\255\009\255\009\255\006\255\009\255\009\255\077\255\036\255\
\009\255\253\254\096\255\000\000\004\255\000\000\000\000\000\000\
\253\254\000\000\010\255\000\000\000\000\253\254\000\000\253\254\
\000\000\000\000\000\000\253\254\000\000\000\000\031\255\000\000\
\000\000\253\254\054\255\000\000\000\000\000\000\000\000\000\000\
\000\000\253\254\000\000\000\000\000\000\000\000\000\000\000\000\
\253\254\000\000\000\000\000\000\000\000\000\000\065\255\120\255\
\120\255\000\000\000\000\000\000\067\255\061\255\009\255\000\000\
\000\000\000\000\044\255\045\255\000\000\000\000\000\000\000\000\
\253\254\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\021\255\083\255\
\082\255\000\000\125\255\000\000\040\255\000\000\000\000\000\000\
\000\000\000\000\000\000\086\255\087\255\040\255\031\255\000\000\
\111\255\085\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\136\000\121\255\000\000\000\000\
\049\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\130\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\116\000\000\000\113\000\241\255\098\000\066\000\248\255\000\000\
\000\000\117\000\000\000\000\000\245\255\126\000\074\000\000\000\
\000\000\001\000\000\000\000\000\127\000\000\000\247\255"

let yytablesize = 146
let yytable = "\040\000\
\072\000\024\000\057\000\029\000\021\000\076\000\075\000\047\000\
\048\000\045\000\047\000\048\000\054\000\022\000\027\000\067\000\
\074\000\036\000\079\000\039\000\022\000\024\000\022\000\088\000\
\025\000\083\000\022\000\029\000\097\000\022\000\085\000\042\000\
\086\000\043\000\103\000\030\000\087\000\059\000\060\000\061\000\
\030\000\062\000\089\000\022\000\036\000\071\000\063\000\064\000\
\065\000\034\000\091\000\097\000\022\000\029\000\094\000\095\000\
\022\000\092\000\032\000\099\000\022\000\022\000\059\000\060\000\
\061\000\029\000\062\000\034\000\097\000\093\000\067\000\063\000\
\064\000\065\000\096\000\100\000\092\000\022\000\059\000\060\000\
\061\000\102\000\062\000\106\000\104\000\105\000\107\000\063\000\
\064\000\065\000\113\000\114\000\112\000\022\000\108\000\116\000\
\109\000\059\000\060\000\061\000\118\000\115\000\001\000\002\000\
\003\000\004\000\005\000\006\000\007\000\008\000\009\000\010\000\
\011\000\012\000\013\000\014\000\015\000\016\000\017\000\018\000\
\019\000\059\000\060\000\061\000\117\000\062\000\049\000\050\000\
\052\000\110\000\063\000\064\000\065\000\111\000\029\000\029\000\
\082\000\029\000\029\000\084\000\090\000\101\000\077\000\098\000\
\000\000\081\000"

let yycheck = "\008\000\
\016\000\001\000\014\000\003\000\001\001\017\000\016\000\002\001\
\003\001\009\000\002\001\003\001\012\000\017\001\005\001\015\000\
\016\000\005\001\018\000\007\001\017\001\021\000\017\001\039\000\
\001\001\025\000\017\001\027\000\008\001\017\001\030\000\005\001\
\032\000\007\001\014\001\005\001\036\000\002\001\003\001\004\001\
\005\001\006\001\042\000\017\001\005\001\010\001\011\001\012\001\
\013\001\005\001\050\000\008\001\017\001\005\001\064\000\065\000\
\017\001\057\000\005\001\071\000\017\001\017\001\002\001\003\001\
\004\001\017\001\006\001\005\001\008\001\005\001\070\000\011\001\
\012\001\013\001\008\001\075\000\076\000\017\001\002\001\003\001\
\004\001\081\000\006\001\002\001\002\001\003\001\005\001\011\001\
\012\001\013\001\005\001\005\001\101\000\017\001\013\001\111\000\
\015\001\002\001\003\001\004\001\016\001\110\000\001\000\002\000\
\003\000\004\000\005\000\006\000\007\000\008\000\009\000\010\000\
\011\000\012\000\013\000\014\000\015\000\016\000\017\000\018\000\
\019\000\002\001\003\001\004\001\014\001\006\001\010\000\011\000\
\012\000\005\001\011\001\012\001\013\001\009\001\005\001\000\000\
\021\000\017\001\009\001\027\000\043\000\076\000\017\000\070\000\
\255\255\019\000"

let yynames_const = "\
  STAR\000\
  MINUS\000\
  DOT\000\
  SHARP\000\
  AT\000\
  DOLLAR\000\
  BANG\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  EOL\000\
  "

let yynames_block = "\
  ARGUMENT\000\
  LIDENT\000\
  UIDENT\000\
  OPERATOR\000\
  INTEGER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string list) in
    Obj.repr(
# 110 "parser.mly"
      ( _1::_2 )
# 211 "parser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 112 "parser.mly"
      ( [] )
# 218 "parser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 116 "parser.mly"
      ( _1 )
# 226 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int64) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int list) in
    Obj.repr(
# 122 "parser.mly"
      ( (to_int _1) :: _2 )
# 234 "parser.ml"
               : int list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 124 "parser.mly"
      ( [] )
# 241 "parser.ml"
               : int list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int64) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 128 "parser.mly"
      ( to_int _1 )
# 249 "parser.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int64) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 132 "parser.mly"
      ( _1 )
# 257 "parser.ml"
               : int64))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int64) in
    Obj.repr(
# 136 "parser.mly"
      ( to_int _1 )
# 264 "parser.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int64) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 140 "parser.mly"
      ( Some (to_int _1) )
# 272 "parser.ml"
               : int option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 142 "parser.mly"
      ( None )
# 279 "parser.ml"
               : int option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int64) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 146 "parser.mly"
      ( Some _1 )
# 287 "parser.ml"
               : 'opt_int64_eol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 148 "parser.mly"
      ( None )
# 294 "parser.ml"
               : 'opt_int64_eol))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 152 "parser.mly"
      ( Some (- _2) )
# 301 "parser.ml"
               : int option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int option) in
    Obj.repr(
# 154 "parser.mly"
      ( _1 )
# 308 "parser.ml"
               : int option))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int64) in
    Obj.repr(
# 158 "parser.mly"
      ( Some (Int64.neg _2) )
# 315 "parser.ml"
               : int64 option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'opt_int64_eol) in
    Obj.repr(
# 160 "parser.mly"
      ( _1 )
# 322 "parser.ml"
               : int64 option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 165 "parser.mly"
                                ( Lident _1 )
# 329 "parser.ml"
               : 'longident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'module_path) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 166 "parser.mly"
                                ( Ldot(_1, _3) )
# 337 "parser.ml"
               : 'longident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 167 "parser.mly"
                                ( Lident _1 )
# 344 "parser.ml"
               : 'longident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 171 "parser.mly"
                                ( Lident _1 )
# 351 "parser.ml"
               : 'module_path))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'module_path) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 172 "parser.mly"
                                ( Ldot(_1, _3) )
# 359 "parser.ml"
               : 'module_path))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'longident) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 176 "parser.mly"
                               ( _1 )
# 367 "parser.ml"
               : Longident.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 179 "parser.mly"
                                ( _1 )
# 374 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 180 "parser.mly"
                                ( _1 )
# 381 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 183 "parser.mly"
                                ( _1 )
# 389 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 186 "parser.mly"
                                ( Some _1 )
# 396 "parser.ml"
               : string option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 187 "parser.mly"
                                ( None )
# 403 "parser.ml"
               : string option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 190 "parser.mly"
                                ( Some _1 )
# 410 "parser.ml"
               : string option))
; (fun __caml_parser_env ->
    Obj.repr(
# 191 "parser.mly"
                                ( None )
# 416 "parser.ml"
               : string option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string option) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 194 "parser.mly"
                                ( _1 )
# 424 "parser.ml"
               : string option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'longident) in
    Obj.repr(
# 199 "parser.mly"
                                               ( E_ident _1 )
# 431 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 200 "parser.mly"
                                                ( E_result )
# 437 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int64) in
    Obj.repr(
# 201 "parser.mly"
                                                ( E_name (to_int _2) )
# 444 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int64) in
    Obj.repr(
# 202 "parser.mly"
                                                ( E_item(_1, (to_int _3)) )
# 452 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int64) in
    Obj.repr(
# 203 "parser.mly"
                                                ( E_item(_1, (to_int _4)) )
# 460 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int64) in
    Obj.repr(
# 204 "parser.mly"
                                                ( E_item(_1, (to_int _4)) )
# 468 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 205 "parser.mly"
                                                ( E_field(_1, _3) )
# 476 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 206 "parser.mly"
                                                ( E_field(_2, "contents") )
# 483 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 207 "parser.mly"
                                                ( _2 )
# 490 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Parser_aux.expression list) in
    Obj.repr(
# 213 "parser.mly"
                                                ( _1::_2 )
# 498 "parser.ml"
               : Parser_aux.expression list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 214 "parser.mly"
                                                ( [] )
# 505 "parser.ml"
               : Parser_aux.expression list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 220 "parser.mly"
                                                ( BA_none )
# 512 "parser.ml"
               : Parser_aux.break_arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 221 "parser.mly"
                                                ( BA_pc _1 )
# 519 "parser.ml"
               : Parser_aux.break_arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 222 "parser.mly"
                                                ( BA_function _1 )
# 527 "parser.ml"
               : Parser_aux.break_arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string option) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int64) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int option) in
    Obj.repr(
# 223 "parser.mly"
                                                ( BA_pos1 (_2, (to_int _3), _4))
# 536 "parser.ml"
               : Parser_aux.break_arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string option) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 224 "parser.mly"
                                                ( BA_pos2 (_2, _4) )
# 544 "parser.ml"
               : Parser_aux.break_arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string option) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int option) in
    Obj.repr(
# 231 "parser.mly"
      ( (_1, Some _2, _3) )
# 553 "parser.ml"
               : string option * int option * int option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string option) in
    Obj.repr(
# 233 "parser.mly"
      ( (_1, None, None) )
# 560 "parser.ml"
               : string option * int option * int option))
; (fun __caml_parser_env ->
    Obj.repr(
# 238 "parser.mly"
        ( stop_user_input () )
# 566 "parser.ml"
               : unit))
(* Entry argument_list_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry argument_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry integer_list_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry integer_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry int64_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry integer *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry opt_integer_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry opt_signed_integer_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry opt_signed_int64_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry identifier *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry identifier_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry identifier_or_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry opt_identifier *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry opt_identifier_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry expression_list_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry break_argument_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry list_arguments_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry end_of_line *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry longident_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let argument_list_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : string list)
let argument_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : string)
let integer_list_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : int list)
let integer_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 4 lexfun lexbuf : int)
let int64_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 5 lexfun lexbuf : int64)
let integer (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 6 lexfun lexbuf : int)
let opt_integer_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 7 lexfun lexbuf : int option)
let opt_signed_integer_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 8 lexfun lexbuf : int option)
let opt_signed_int64_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 9 lexfun lexbuf : int64 option)
let identifier (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 10 lexfun lexbuf : string)
let identifier_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 11 lexfun lexbuf : string)
let identifier_or_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 12 lexfun lexbuf : string option)
let opt_identifier (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 13 lexfun lexbuf : string option)
let opt_identifier_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 14 lexfun lexbuf : string option)
let expression_list_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 15 lexfun lexbuf : Parser_aux.expression list)
let break_argument_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 16 lexfun lexbuf : Parser_aux.break_arg)
let list_arguments_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 17 lexfun lexbuf : string option * int option * int option)
let end_of_line (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 18 lexfun lexbuf : unit)
let longident_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 19 lexfun lexbuf : Longident.t)
