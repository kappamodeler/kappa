type token =
  | ERROR
  | END
  | Title of (int * string option)
  | BOLD
  | EMP
  | CENTER
  | LEFT
  | RIGHT
  | ITALIC
  | CUSTOM of (string)
  | LIST
  | ENUM
  | ITEM
  | LINK
  | CODE
  | END_CODE
  | CODE_PRE
  | END_CODE_PRE
  | VERB
  | END_VERB
  | LATEX
  | END_LATEX
  | ELE_REF
  | VAL_REF
  | TYP_REF
  | EXC_REF
  | MOD_REF
  | MODT_REF
  | CLA_REF
  | CLT_REF
  | ATT_REF
  | MET_REF
  | SEC_REF
  | MOD_LIST_REF
  | INDEX_LIST
  | SUPERSCRIPT
  | SUBSCRIPT
  | BEGIN_SHORTCUT_LIST_ITEM
  | BEGIN_SHORTCUT_ENUM_ITEM
  | SHORTCUT_LIST_ITEM
  | SHORTCUT_ENUM_ITEM
  | END_SHORTCUT_LIST
  | BLANK_LINE
  | EOF
  | Char of (string)

open Parsing;;
# 2 "odoc_text_parser.mly"
(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: odoc_text_parser.mly,v 1.5 2006/01/04 16:55:50 doligez Exp $ *)

open Odoc_types

let identchar =
  "[A-Z a-z_\192-\214\216-\246\248-\255'0-9]"
let blank = "[ \010\013\009\012]"

let remove_beginning_blanks s =
  Str.global_replace (Str.regexp ("^"^blank^"+")) "" s

let remove_trailing_blanks s =
  Str.global_replace (Str.regexp (blank^"+$")) "" s

let print_DEBUG s = print_string s; print_newline ()
# 77 "odoc_text_parser.ml"
let yytransl_const = [|
  257 (* ERROR *);
  258 (* END *);
  260 (* BOLD *);
  261 (* EMP *);
  262 (* CENTER *);
  263 (* LEFT *);
  264 (* RIGHT *);
  265 (* ITALIC *);
  267 (* LIST *);
  268 (* ENUM *);
  269 (* ITEM *);
  270 (* LINK *);
  271 (* CODE *);
  272 (* END_CODE *);
  273 (* CODE_PRE *);
  274 (* END_CODE_PRE *);
  275 (* VERB *);
  276 (* END_VERB *);
  277 (* LATEX *);
  278 (* END_LATEX *);
  279 (* ELE_REF *);
  280 (* VAL_REF *);
  281 (* TYP_REF *);
  282 (* EXC_REF *);
  283 (* MOD_REF *);
  284 (* MODT_REF *);
  285 (* CLA_REF *);
  286 (* CLT_REF *);
  287 (* ATT_REF *);
  288 (* MET_REF *);
  289 (* SEC_REF *);
  290 (* MOD_LIST_REF *);
  291 (* INDEX_LIST *);
  292 (* SUPERSCRIPT *);
  293 (* SUBSCRIPT *);
  294 (* BEGIN_SHORTCUT_LIST_ITEM *);
  295 (* BEGIN_SHORTCUT_ENUM_ITEM *);
  296 (* SHORTCUT_LIST_ITEM *);
  297 (* SHORTCUT_ENUM_ITEM *);
  298 (* END_SHORTCUT_LIST *);
  299 (* BLANK_LINE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  259 (* Title *);
  266 (* CUSTOM *);
  300 (* Char *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\003\000\003\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\005\000\005\000\005\000\005\000\005\000\009\000\007\000\
\007\000\010\000\008\000\008\000\011\000\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\002\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\001\000\
\003\000\003\000\005\000\001\000\003\000\003\000\003\000\003\000\
\001\000\001\000\002\000\002\000\001\000\002\000\003\000\002\000\
\001\000\002\000\002\000\001\000\002\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\032\000\000\000\000\000\
\000\000\000\000\036\000\002\000\000\000\056\000\000\000\003\000\
\000\000\041\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\055\000\001\000\
\005\000\006\000\007\000\010\000\013\000\014\000\015\000\008\000\
\009\000\000\000\016\000\044\000\000\000\000\000\017\000\000\000\
\018\000\019\000\033\000\034\000\020\000\021\000\022\000\023\000\
\024\000\025\000\026\000\027\000\028\000\029\000\030\000\031\000\
\011\000\012\000\000\000\048\000\037\000\038\000\000\000\051\000\
\039\000\040\000\047\000\000\000\050\000\053\000\035\000"

let yydgoto = "\002\000\
\038\000\075\000\040\000\041\000\052\000\042\000\076\000\078\000\
\054\000\116\000\120\000"

let yysindex = "\012\000\
\001\000\000\000\099\255\099\255\099\255\099\255\099\255\099\255\
\099\255\099\255\024\255\024\255\214\254\214\254\214\254\214\254\
\214\254\214\254\214\254\214\254\214\254\214\254\214\254\214\254\
\214\254\214\254\214\254\214\254\214\254\000\000\099\255\099\255\
\099\255\099\255\000\000\000\000\214\254\000\000\038\000\000\000\
\099\255\000\000\037\255\041\255\043\255\049\255\050\255\052\255\
\057\255\058\255\099\255\002\255\024\255\024\255\033\255\059\255\
\046\255\045\255\044\255\047\255\063\255\064\255\065\255\068\255\
\069\255\070\255\071\255\072\255\073\255\074\255\076\255\077\255\
\078\255\079\255\042\255\047\000\048\255\048\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\081\255\000\000\000\000\214\254\214\254\000\000\099\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\099\255\000\000\000\000\000\000\099\255\000\000\
\000\000\000\000\000\000\082\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\044\000\000\000\000\000\000\000\
\086\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\083\255\085\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\049\000\000\000\050\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\086\255\088\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\002\000\051\000\000\000\244\255\003\000\232\255\230\255\
\000\000\000\000\000\000"

let yytablesize = 384
let yytable = "\055\000\
\036\000\037\000\039\000\091\000\043\000\044\000\045\000\046\000\
\047\000\048\000\049\000\050\000\001\000\053\000\053\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\095\000\077\000\051\000\080\000\082\000\079\000\
\093\000\094\000\083\000\054\000\084\000\037\000\118\000\122\000\
\049\000\052\000\085\000\086\000\090\000\087\000\092\000\053\000\
\053\000\092\000\088\000\089\000\096\000\097\000\098\000\099\000\
\101\000\102\000\103\000\037\000\100\000\104\000\105\000\106\000\
\107\000\108\000\109\000\110\000\037\000\111\000\112\000\113\000\
\114\000\115\000\123\000\127\000\042\000\004\000\045\000\043\000\
\119\000\046\000\125\000\081\000\126\000\000\000\000\000\092\000\
\092\000\124\000\000\000\000\000\000\000\003\000\004\000\005\000\
\006\000\007\000\008\000\009\000\010\000\011\000\012\000\000\000\
\013\000\014\000\000\000\015\000\000\000\016\000\000\000\017\000\
\077\000\018\000\019\000\020\000\021\000\022\000\023\000\024\000\
\025\000\026\000\027\000\028\000\029\000\030\000\031\000\032\000\
\033\000\034\000\000\000\000\000\000\000\035\000\037\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\000\004\000\005\000\006\000\007\000\
\008\000\009\000\010\000\011\000\012\000\000\000\013\000\014\000\
\000\000\015\000\000\000\016\000\000\000\017\000\000\000\018\000\
\019\000\020\000\021\000\022\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000\030\000\031\000\032\000\033\000\034\000\
\000\000\000\000\000\000\035\000\037\000\054\000\054\000\054\000\
\054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
\054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
\054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
\054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
\054\000\054\000\054\000\054\000\054\000\054\000\054\000\004\000\
\117\000\121\000\049\000\052\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\004\000\004\000\004\000"

let yycheck = "\012\000\
\000\000\044\001\001\000\002\001\003\000\004\000\005\000\006\000\
\007\000\008\000\009\000\010\000\001\000\011\000\012\000\013\000\
\014\000\015\000\016\000\017\000\018\000\019\000\020\000\021\000\
\022\000\023\000\024\000\025\000\026\000\027\000\028\000\029\000\
\031\000\032\000\002\001\034\000\013\001\000\000\002\001\037\000\
\053\000\054\000\002\001\000\000\002\001\044\001\000\000\000\000\
\000\000\000\000\002\001\002\001\051\000\002\001\052\000\053\000\
\054\000\055\000\002\001\002\001\002\001\016\001\018\001\020\001\
\002\001\002\001\002\001\044\001\022\001\002\001\002\001\002\001\
\002\001\002\001\002\001\002\001\044\001\002\001\002\001\002\001\
\002\001\040\001\002\001\002\001\002\001\000\000\002\001\002\001\
\041\001\002\001\115\000\041\000\119\000\255\255\255\255\093\000\
\094\000\096\000\255\255\255\255\255\255\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\255\255\
\014\001\015\001\255\255\017\001\255\255\019\001\255\255\021\001\
\119\000\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\037\001\
\038\001\039\001\255\255\255\255\255\255\043\001\044\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\255\255\014\001\015\001\
\255\255\017\001\255\255\019\001\255\255\021\001\255\255\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\038\001\039\001\
\255\255\255\255\255\255\043\001\044\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\038\001\039\001\040\001\041\001\042\001\043\001\002\001\
\042\001\042\001\042\001\042\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\040\001\041\001\042\001"

let yynames_const = "\
  ERROR\000\
  END\000\
  BOLD\000\
  EMP\000\
  CENTER\000\
  LEFT\000\
  RIGHT\000\
  ITALIC\000\
  LIST\000\
  ENUM\000\
  ITEM\000\
  LINK\000\
  CODE\000\
  END_CODE\000\
  CODE_PRE\000\
  END_CODE_PRE\000\
  VERB\000\
  END_VERB\000\
  LATEX\000\
  END_LATEX\000\
  ELE_REF\000\
  VAL_REF\000\
  TYP_REF\000\
  EXC_REF\000\
  MOD_REF\000\
  MODT_REF\000\
  CLA_REF\000\
  CLT_REF\000\
  ATT_REF\000\
  MET_REF\000\
  SEC_REF\000\
  MOD_LIST_REF\000\
  INDEX_LIST\000\
  SUPERSCRIPT\000\
  SUBSCRIPT\000\
  BEGIN_SHORTCUT_LIST_ITEM\000\
  BEGIN_SHORTCUT_ENUM_ITEM\000\
  SHORTCUT_LIST_ITEM\000\
  SHORTCUT_ENUM_ITEM\000\
  END_SHORTCUT_LIST\000\
  BLANK_LINE\000\
  EOF\000\
  "

let yynames_block = "\
  Title\000\
  CUSTOM\000\
  Char\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 87 "odoc_text_parser.mly"
           ( _1 )
# 368 "odoc_text_parser.ml"
               : Odoc_types.text))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "odoc_text_parser.mly"
      ( [Raw ""] )
# 374 "odoc_text_parser.ml"
               : Odoc_types.text))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'text_element_list) in
    Obj.repr(
# 92 "odoc_text_parser.mly"
                    ( _1 )
# 381 "odoc_text_parser.ml"
               : 'text))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'text_element) in
    Obj.repr(
# 96 "odoc_text_parser.mly"
               ( [ _1 ] )
# 388 "odoc_text_parser.ml"
               : 'text_element_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'text_element) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'text_element_list) in
    Obj.repr(
# 97 "odoc_text_parser.mly"
                                 ( _1 :: _2 )
# 396 "odoc_text_parser.ml"
               : 'text_element_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int * string option) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 101 "odoc_text_parser.mly"
                 ( let n, l_opt = _1 in Title (n, l_opt, _2) )
# 404 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 102 "odoc_text_parser.mly"
                ( Bold _2 )
# 411 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 103 "odoc_text_parser.mly"
                  ( Italic _2 )
# 418 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 104 "odoc_text_parser.mly"
                  ( Custom (_1, _2) )
# 426 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 105 "odoc_text_parser.mly"
               ( Emphasize _2 )
# 433 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 106 "odoc_text_parser.mly"
                       ( Superscript _2 )
# 440 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 107 "odoc_text_parser.mly"
                     ( Subscript _2 )
# 447 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 108 "odoc_text_parser.mly"
                  ( Center _2 )
# 454 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 109 "odoc_text_parser.mly"
                ( Left _2 )
# 461 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 110 "odoc_text_parser.mly"
                 ( Right _2 )
# 468 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list) in
    Obj.repr(
# 111 "odoc_text_parser.mly"
                ( List _2 )
# 475 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list) in
    Obj.repr(
# 112 "odoc_text_parser.mly"
                ( Enum _2 )
# 482 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 113 "odoc_text_parser.mly"
                       ( Code _2 )
# 489 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 114 "odoc_text_parser.mly"
                               ( CodePre _2 )
# 496 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 115 "odoc_text_parser.mly"
                     (
      let s2 = remove_beginning_blanks _2 in
      let s3 = remove_trailing_blanks s2 in
      Ref (s3, None)
     )
# 507 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 120 "odoc_text_parser.mly"
                     (
      let s2 = remove_beginning_blanks _2 in
      let s3 = remove_trailing_blanks s2 in
      Ref (s3, Some RK_value)
     )
# 518 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 125 "odoc_text_parser.mly"
                     (
      let s2 = remove_beginning_blanks _2 in
      let s3 = remove_trailing_blanks s2 in
      Ref (s3, Some RK_type)
     )
# 529 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 130 "odoc_text_parser.mly"
                     (
      let s2 = remove_beginning_blanks _2 in
      let s3 = remove_trailing_blanks s2 in
      Ref (s3, Some RK_exception)
     )
# 540 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 135 "odoc_text_parser.mly"
                     (
      let s2 = remove_beginning_blanks _2 in
      let s3 = remove_trailing_blanks s2 in
      Ref (s3, Some RK_module)
     )
# 551 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 140 "odoc_text_parser.mly"
                      (
      let s2 = remove_beginning_blanks _2 in
      let s3 = remove_trailing_blanks s2 in
      Ref (s3, Some RK_module_type)
     )
# 562 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 145 "odoc_text_parser.mly"
                     (
      let s2 = remove_beginning_blanks _2 in
      let s3 = remove_trailing_blanks s2 in
      Ref (s3, Some RK_class)
     )
# 573 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 150 "odoc_text_parser.mly"
                     (
      let s2 = remove_beginning_blanks _2 in
      let s3 = remove_trailing_blanks s2 in
      Ref (s3, Some RK_class_type)
     )
# 584 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 155 "odoc_text_parser.mly"
                     (
      let s2 = remove_beginning_blanks _2 in
      let s3 = remove_trailing_blanks s2 in
      Ref (s3, Some RK_attribute)
     )
# 595 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 160 "odoc_text_parser.mly"
                     (
      let s2 = remove_beginning_blanks _2 in
      let s3 = remove_trailing_blanks s2 in
      Ref (s3, Some RK_method)
     )
# 606 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 165 "odoc_text_parser.mly"
                     (
      let s2 = remove_beginning_blanks _2 in
      let s3 = remove_trailing_blanks s2 in
      Ref (s3, Some (RK_section []))
     )
# 617 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 170 "odoc_text_parser.mly"
                          (
      let s2 = remove_beginning_blanks _2 in
      let s3 = remove_trailing_blanks s2 in
      let l = Odoc_misc.split_with_blanks s3 in
      Module_list l
     )
# 629 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    Obj.repr(
# 176 "odoc_text_parser.mly"
             ( Index_list )
# 635 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 177 "odoc_text_parser.mly"
                       ( Verbatim _2 )
# 642 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 178 "odoc_text_parser.mly"
                         ( Latex _2 )
# 649 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 179 "odoc_text_parser.mly"
                           ( Link (_2, _4) )
# 657 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    Obj.repr(
# 180 "odoc_text_parser.mly"
             ( Newline )
# 663 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'shortcut_list) in
    Obj.repr(
# 181 "odoc_text_parser.mly"
                                                           ( List _2 )
# 670 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'shortcut_list) in
    Obj.repr(
# 182 "odoc_text_parser.mly"
                                             ( List _2 )
# 677 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'shortcut_enum) in
    Obj.repr(
# 183 "odoc_text_parser.mly"
                                                           ( Enum _2 )
# 684 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'shortcut_enum) in
    Obj.repr(
# 184 "odoc_text_parser.mly"
                                             ( Enum _2 )
# 691 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'string) in
    Obj.repr(
# 185 "odoc_text_parser.mly"
         ( Raw _1 )
# 698 "odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'string) in
    Obj.repr(
# 189 "odoc_text_parser.mly"
         ( [] (* A VOIR : un test pour voir qu'il n'y a que des blancs *) )
# 705 "odoc_text_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 190 "odoc_text_parser.mly"
              ( _2 )
# 713 "odoc_text_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'string) in
    Obj.repr(
# 191 "odoc_text_parser.mly"
               ( _1 )
# 721 "odoc_text_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'item) in
    Obj.repr(
# 192 "odoc_text_parser.mly"
       ( [ _1 ] )
# 728 "odoc_text_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'item) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 193 "odoc_text_parser.mly"
            ( _1 :: _2 )
# 736 "odoc_text_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 198 "odoc_text_parser.mly"
                  ( _2 )
# 743 "odoc_text_parser.ml"
               : 'item))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'shortcut_list2) in
    Obj.repr(
# 202 "odoc_text_parser.mly"
                         ( _1 :: _2 )
# 751 "odoc_text_parser.ml"
               : 'shortcut_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'text) in
    Obj.repr(
# 203 "odoc_text_parser.mly"
       ( [ _1 ] )
# 758 "odoc_text_parser.ml"
               : 'shortcut_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'shortcut_list) in
    Obj.repr(
# 207 "odoc_text_parser.mly"
                                   ( _2 )
# 765 "odoc_text_parser.ml"
               : 'shortcut_list2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'shortcut_enum2) in
    Obj.repr(
# 211 "odoc_text_parser.mly"
                         ( _1 :: _2 )
# 773 "odoc_text_parser.ml"
               : 'shortcut_enum))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'text) in
    Obj.repr(
# 212 "odoc_text_parser.mly"
       ( [ _1 ] )
# 780 "odoc_text_parser.ml"
               : 'shortcut_enum))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'shortcut_enum) in
    Obj.repr(
# 216 "odoc_text_parser.mly"
                                   ( _2 )
# 787 "odoc_text_parser.ml"
               : 'shortcut_enum2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 221 "odoc_text_parser.mly"
         ( _1 )
# 794 "odoc_text_parser.ml"
               : 'string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'string) in
    Obj.repr(
# 222 "odoc_text_parser.mly"
              ( _1^_2 )
# 802 "odoc_text_parser.ml"
               : 'string))
(* Entry main *)
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
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Odoc_types.text)
;;
