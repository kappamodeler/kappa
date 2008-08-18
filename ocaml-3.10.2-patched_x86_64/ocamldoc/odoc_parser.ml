type token =
  | Description of (string * (string option))
  | See_url of (string)
  | See_file of (string)
  | See_doc of (string)
  | T_PARAM
  | T_AUTHOR
  | T_VERSION
  | T_SEE
  | T_SINCE
  | T_DEPRECATED
  | T_RAISES
  | T_RETURN
  | T_CUSTOM of (string)
  | EOF
  | Desc of (string)

open Parsing;;
# 2 "odoc_parser.mly"
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

(* $Id: odoc_parser.mly,v 1.3 2003/11/24 10:43:11 starynke Exp $ *)

open Odoc_types
open Odoc_comments_global

let uppercase = "[A-Z\192-\214\216-\222]"
let identchar = 
  "[A-Za-z_\192-\214\216-\246\248-\255'0-9]"
let blank = "[ \010\013\009\012]"

let print_DEBUG s = print_string s; print_newline ()
# 43 "odoc_parser.ml"
let yytransl_const = [|
  261 (* T_PARAM *);
  262 (* T_AUTHOR *);
  263 (* T_VERSION *);
  264 (* T_SEE *);
  265 (* T_SINCE *);
  266 (* T_DEPRECATED *);
  267 (* T_RAISES *);
  268 (* T_RETURN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* Description *);
  258 (* See_url *);
  259 (* See_file *);
  260 (* See_doc *);
  269 (* T_CUSTOM *);
  270 (* Desc *);
    0|]

let yylhs = "\255\255\
\003\000\004\000\004\000\004\000\001\000\001\000\002\000\005\000\
\005\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\007\000\008\000\009\000\010\000\011\000\012\000\
\013\000\014\000\015\000\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\001\000\002\000\001\000\
\002\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\005\000\006\000\028\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\029\000\000\000\000\000\010\000\011\000\012\000\013\000\014\000\
\015\000\016\000\017\000\018\000\002\000\003\000\004\000\030\000\
\000\000\019\000\020\000\021\000\022\000\023\000\024\000\025\000\
\026\000\027\000\007\000\009\000\001\000"

let yydgoto = "\004\000\
\007\000\017\000\032\000\033\000\018\000\019\000\020\000\021\000\
\022\000\023\000\024\000\025\000\026\000\027\000\028\000"

let yysindex = "\010\000\
\001\000\253\254\012\255\000\000\000\000\000\000\000\000\242\254\
\003\255\004\255\005\255\006\255\007\255\008\255\009\255\010\255\
\000\000\025\000\253\254\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\026\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\009\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 258
let yytable = "\034\000\
\006\000\008\000\009\000\010\000\011\000\012\000\013\000\014\000\
\015\000\016\000\001\000\002\000\003\000\029\000\030\000\031\000\
\035\000\036\000\037\000\038\000\039\000\040\000\041\000\042\000\
\043\000\008\000\045\000\044\000\000\000\000\000\000\000\000\000\
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
\000\000\005\000"

let yycheck = "\014\001\
\000\000\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\001\000\002\000\003\000\002\001\003\001\004\001\
\014\001\014\001\014\001\014\001\014\001\014\001\014\001\014\001\
\000\000\000\000\014\001\019\000\255\255\255\255\255\255\255\255\
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
\255\255\001\001"

let yynames_const = "\
  T_PARAM\000\
  T_AUTHOR\000\
  T_VERSION\000\
  T_SEE\000\
  T_SINCE\000\
  T_DEPRECATED\000\
  T_RAISES\000\
  T_RETURN\000\
  EOF\000\
  "

let yynames_block = "\
  Description\000\
  See_url\000\
  See_file\000\
  See_doc\000\
  T_CUSTOM\000\
  Desc\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'see_ref) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "odoc_parser.mly"
               ( (_1, _2) )
# 209 "odoc_parser.ml"
               : Odoc_types.see_ref * string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "odoc_parser.mly"
            ( Odoc_types.See_url _1 )
# 216 "odoc_parser.ml"
               : 'see_ref))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 60 "odoc_parser.mly"
           ( Odoc_types.See_file _1 )
# 223 "odoc_parser.ml"
               : 'see_ref))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "odoc_parser.mly"
          ( Odoc_types.See_doc _1 )
# 230 "odoc_parser.ml"
               : 'see_ref))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string * (string option)) in
    Obj.repr(
# 65 "odoc_parser.mly"
              ( Some _1 )
# 237 "odoc_parser.ml"
               : (string * (string option)) option))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "odoc_parser.mly"
      ( None )
# 243 "odoc_parser.ml"
               : (string * (string option)) option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'element_list) in
    Obj.repr(
# 70 "odoc_parser.mly"
                   ( () )
# 250 "odoc_parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'element) in
    Obj.repr(
# 74 "odoc_parser.mly"
          ( () )
# 257 "odoc_parser.ml"
               : 'element_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'element) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'element_list) in
    Obj.repr(
# 75 "odoc_parser.mly"
                       ( () )
# 265 "odoc_parser.ml"
               : 'element_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 79 "odoc_parser.mly"
        ( () )
# 272 "odoc_parser.ml"
               : 'element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'author) in
    Obj.repr(
# 80 "odoc_parser.mly"
         ( () )
# 279 "odoc_parser.ml"
               : 'element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'version) in
    Obj.repr(
# 81 "odoc_parser.mly"
          ( () )
# 286 "odoc_parser.ml"
               : 'element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'see) in
    Obj.repr(
# 82 "odoc_parser.mly"
      ( () )
# 293 "odoc_parser.ml"
               : 'element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'since) in
    Obj.repr(
# 83 "odoc_parser.mly"
        ( () )
# 300 "odoc_parser.ml"
               : 'element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'deprecated) in
    Obj.repr(
# 84 "odoc_parser.mly"
             ( () )
# 307 "odoc_parser.ml"
               : 'element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'raise_exc) in
    Obj.repr(
# 85 "odoc_parser.mly"
            ( () )
# 314 "odoc_parser.ml"
               : 'element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'return) in
    Obj.repr(
# 86 "odoc_parser.mly"
         ( () )
# 321 "odoc_parser.ml"
               : 'element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'custom) in
    Obj.repr(
# 87 "odoc_parser.mly"
         ( () )
# 328 "odoc_parser.ml"
               : 'element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "odoc_parser.mly"
    ( 
      (* isolate the identificator *)
      (* we only look for simple id, no pattern nor tuples *)
      let s = _2 in
      match Str.split (Str.regexp (blank^"+")) s with
        []
      | _ :: [] ->
          raise (Failure "usage: @param id description")
      | id :: _ ->    
          print_DEBUG ("Identificator "^id);
          let reg = identchar^"+" in
          print_DEBUG ("reg="^reg);
          if Str.string_match (Str.regexp reg) id 0 then
            let remain = String.sub s (String.length id) ((String.length s) - (String.length id)) in
            print_DEBUG ("T_PARAM Desc remain="^remain);
            let remain2 = Str.replace_first (Str.regexp ("^"^blank^"+")) "" remain in
            params := !params @ [(id, remain2)] 
          else
            raise (Failure (id^" is not a valid parameter identificator in \"@param "^s^"\""))
    )
# 354 "odoc_parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 114 "odoc_parser.mly"
                  ( authors := !authors @ [ _2 ] )
# 361 "odoc_parser.ml"
               : 'author))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "odoc_parser.mly"
                   ( version := Some _2 )
# 368 "odoc_parser.ml"
               : 'version))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 120 "odoc_parser.mly"
               ( sees := !sees @ [_2] )
# 375 "odoc_parser.ml"
               : 'see))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 123 "odoc_parser.mly"
                 ( since := Some _2 )
# 382 "odoc_parser.ml"
               : 'since))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 126 "odoc_parser.mly"
                      ( deprecated := Some _2 )
# 389 "odoc_parser.ml"
               : 'deprecated))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "odoc_parser.mly"
    ( 
      (* isolate the exception construtor name *)
      let s = _2 in
      match Str.split (Str.regexp (blank^"+")) s with
        []
      | _ :: [] ->
          raise (Failure "usage: @raise Exception description")
      | id :: _ ->    
          print_DEBUG ("exception "^id);
          let reg = uppercase^identchar^"*"^"\\(\\."^uppercase^identchar^"*\\)*" in
          print_DEBUG ("reg="^reg);
          if Str.string_match (Str.regexp reg) id 0 then
            let remain = String.sub s (String.length id) ((String.length s) - (String.length id)) in
            let remain2 = Str.replace_first (Str.regexp ("^"^blank^"+")) "" remain in
            raised_exceptions := !raised_exceptions @ [(id, remain2)] 
          else
            raise (Failure (id^" is not a valid exception constructor in \"@raise "^s^"\""))
    )
# 413 "odoc_parser.ml"
               : 'raise_exc))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 150 "odoc_parser.mly"
                  ( return_value := Some _2 )
# 420 "odoc_parser.ml"
               : 'return))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 154 "odoc_parser.mly"
                  ( customs := !customs @ [(_1, _2)] )
# 428 "odoc_parser.ml"
               : 'custom))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry info_part2 *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry see_info *)
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : (string * (string option)) option)
let info_part2 (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : unit)
let see_info (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : Odoc_types.see_ref * string)
;;
# 158 "odoc_parser.mly"
 
# 463 "odoc_parser.ml"
