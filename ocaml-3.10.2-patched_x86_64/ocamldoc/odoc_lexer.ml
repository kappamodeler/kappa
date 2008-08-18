# 1 "odoc_lexer.mll"
 
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

(* $Id: odoc_lexer.mll,v 1.4 2003/11/24 10:41:04 starynke Exp $ *)

(** The lexer for special comments. *)

open Lexing
open Odoc_parser

let line_number = ref 0


let string_buffer = Buffer.create 32

(** Fonction de remise à zéro de la chaine de caractères tampon *)
let reset_string_buffer () = Buffer.reset string_buffer

(** Fonction d'ajout d'un caractère dans la chaine de caractères tampon *)
let ajout_char_string = Buffer.add_char string_buffer

(** Add a string to the buffer. *)
let ajout_string = Buffer.add_string string_buffer

let lecture_string () = Buffer.contents string_buffer

(** The variable which will contain the description string. 
   Is initialized when we encounter the start of a special comment. *)
let description = ref ""

let blank = "[ \013\009\012]"

(** The nested comments level. *)
let comments_level = ref 0

let print_DEBUG2 s = print_string s; print_newline ()

(** This function returns the given string without the leading and trailing blanks.*)
let remove_blanks s =
  print_DEBUG2 ("remove_blanks "^s);
  let l = Str.split_delim (Str.regexp "\n") s in
  let l2 =
    let rec iter liste =
      match liste with
        h :: q ->
          let h2 = Str.global_replace (Str.regexp ("^"^blank^"+")) "" h in 
          if h2 = "" then
            (
             print_DEBUG2 (h^" n'a que des blancs");
             (* we remove this line and must remove leading blanks of the next one *)
             iter q
            )
          else
            (* we don't remove leading blanks in the remaining lines *)
            h2 :: q
      | _ ->
          []
    in iter l
  in
  let l3 = 
    let rec iter liste = 
      match liste with
        h :: q ->
          let h2 = Str.global_replace (Str.regexp (blank^"+$")) "" h in 
          if h2 = "" then
            (
             print_DEBUG2 (h^" n'a que des blancs");
             (* we remove this line and must remove trailing blanks of the next one *)
             iter q
            )
          else
            (* we don't remove trailing blanks in the remaining lines *)
            h2 :: q
      | _ ->
          []
    in
    List.rev (iter (List.rev l2))
  in
  String.concat "\n" l3

(** Remove first blank characters of each line of a string, until the first '*' *)
let remove_stars s =
  let s2 = Str.global_replace (Str.regexp ("^"^blank^"*\\*")) "" s in
  s2

# 97 "odoc_lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\246\255\000\000\249\255\001\000\254\255\002\000\002\000\
    \004\000\006\000\253\255\252\255\008\000\248\255\011\000\251\255\
    \009\000\000\000\010\000\013\000\255\255\045\000\036\000\013\000\
    \015\000\036\001\009\000\010\000\109\000\109\000\101\001\097\001\
    \050\000\247\255\016\000\016\000\007\000\019\000\024\000\026\000\
    ";
  Lexing.lex_backtrk = 
   "\255\255\255\255\009\000\255\255\009\000\255\255\000\000\008\000\
    \005\000\004\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \004\000\004\000\004\000\004\000\255\255\003\000\255\255\002\000\
    \002\000\255\255\003\000\003\000\255\255\001\000\000\000\002\000\
    \255\255\255\255\008\000\008\000\000\000\005\000\004\000\002\000\
    ";
  Lexing.lex_default = 
   "\001\000\000\000\255\255\000\000\255\255\000\000\255\255\255\255\
    \255\255\255\255\000\000\000\000\255\255\000\000\015\000\000\000\
    \255\255\255\255\255\255\255\255\000\000\255\255\010\000\255\255\
    \255\255\026\000\026\000\026\000\255\255\026\000\026\000\255\255\
    \033\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\006\000\005\000\006\000\006\000\006\000\006\000\006\000\
    \036\000\000\000\000\000\036\000\036\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \006\000\000\000\006\000\000\000\000\000\000\000\000\000\036\000\
    \004\000\013\000\002\000\007\000\008\000\010\000\009\000\011\000\
    \012\000\011\000\012\000\018\000\005\000\019\000\020\000\005\000\
    \020\000\013\000\037\000\036\000\005\000\038\000\036\000\036\000\
    \010\000\011\000\039\000\011\000\039\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\016\000\023\000\000\000\024\000\000\000\
    \000\000\000\000\036\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\035\000\000\000\034\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\027\000\027\000\017\000\
    \021\000\000\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\021\000\000\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\027\000\000\000\000\000\031\000\000\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \003\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\255\255\255\255\255\255\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\255\255\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\030\000\029\000\000\000\
    \030\000\030\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\030\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\028\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\255\255\030\000\000\000\
    \000\000\030\000\030\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \027\000\000\000\000\000\000\000\000\000\030\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \031\000\027\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\015\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \000\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\000\000\000\000\000\000\000\000\255\255";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\006\000\000\000\000\000\006\000\006\000\
    \036\000\255\255\255\255\036\000\036\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\006\000\255\255\255\255\255\255\255\255\036\000\
    \000\000\002\000\000\000\004\000\007\000\008\000\008\000\009\000\
    \009\000\012\000\012\000\014\000\018\000\014\000\019\000\023\000\
    \024\000\034\000\035\000\032\000\032\000\037\000\032\000\032\000\
    \017\000\038\000\038\000\039\000\039\000\255\255\255\255\255\255\
    \255\255\026\000\255\255\014\000\022\000\255\255\022\000\255\255\
    \255\255\255\255\032\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\032\000\255\255\032\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\026\000\027\000\014\000\
    \016\000\255\255\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\021\000\255\255\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \255\255\255\255\255\255\255\255\255\255\029\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\029\000\255\255\255\255\028\000\255\255\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \000\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\026\000\027\000\014\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\022\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\025\000\025\000\255\255\
    \025\000\025\000\032\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\025\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\025\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\029\000\030\000\255\255\
    \255\255\030\000\030\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \025\000\255\255\255\255\255\255\255\255\030\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\030\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \031\000\030\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\025\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \255\255\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\255\255\255\255\255\255\255\255\030\000";
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

let rec main lexbuf =
    __ocaml_lex_main_rec lexbuf 0
and __ocaml_lex_main_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 103 "odoc_lexer.mll"
      ( 
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        main lexbuf
      )
# 300 "odoc_lexer.ml"

  | 1 ->
# 109 "odoc_lexer.mll"
      (
        incr line_number;
        incr Odoc_comments_global.nb_chars;
        main lexbuf 
      )
# 309 "odoc_lexer.ml"

  | 2 ->
# 115 "odoc_lexer.mll"
      (
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        Description ("", None)
      )
# 317 "odoc_lexer.ml"

  | 3 ->
# 121 "odoc_lexer.mll"
      (
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        main lexbuf
      )
# 325 "odoc_lexer.ml"

  | 4 ->
# 127 "odoc_lexer.mll"
      (
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        incr comments_level;
        main lexbuf
      )
# 334 "odoc_lexer.ml"

  | 5 ->
# 134 "odoc_lexer.mll"
      ( 
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        incr comments_level;
        if !comments_level = 1 then
          (
           reset_string_buffer ();
           description := "";
           special_comment lexbuf 
          )
        else
          main lexbuf
      )
# 350 "odoc_lexer.ml"

  | 6 ->
# 148 "odoc_lexer.mll"
      ( EOF )
# 355 "odoc_lexer.ml"

  | 7 ->
# 151 "odoc_lexer.mll"
      (
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        decr comments_level ;
        main lexbuf
      )
# 364 "odoc_lexer.ml"

  | 8 ->
# 158 "odoc_lexer.mll"
      (
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        incr comments_level ;
        main lexbuf
      )
# 373 "odoc_lexer.ml"

  | 9 ->
# 165 "odoc_lexer.mll"
      ( 
        incr Odoc_comments_global.nb_chars;
        main lexbuf
      )
# 381 "odoc_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_main_rec lexbuf __ocaml_lex_state

and special_comment lexbuf =
    __ocaml_lex_special_comment_rec lexbuf 14
and __ocaml_lex_special_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 172 "odoc_lexer.mll"
      ( 
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        if !comments_level = 1 then
          (
           (* there is just a description *)
           let s2 = lecture_string () in
           let s3 = remove_blanks s2 in
           let s4 = 
             if !Odoc_args.remove_stars then
               remove_stars s3
             else
               s3
           in
           Description (s4, None)
          )
        else
          (
           ajout_string s;
           decr comments_level;
           special_comment lexbuf
          )
      )
# 414 "odoc_lexer.ml"

  | 1 ->
# 197 "odoc_lexer.mll"
      (
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        incr comments_level ;
        ajout_string s;
        special_comment lexbuf
      )
# 425 "odoc_lexer.ml"

  | 2 ->
# 206 "odoc_lexer.mll"
      ( 
        let s = Lexing.lexeme lexbuf in
        let c = (Lexing.lexeme_char lexbuf 1) in
        ajout_char_string c;
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        special_comment lexbuf 
      )
# 436 "odoc_lexer.ml"

  | 3 ->
# 215 "odoc_lexer.mll"
      (
        (* we keep the description before we go further *)
        let s = lecture_string () in
        description := remove_blanks s;
        reset_string_buffer ();
        let len = String.length (Lexing.lexeme lexbuf) in
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - len;
        lexbuf.Lexing.lex_curr_p <- 
	  { lexbuf.Lexing.lex_curr_p with
	    pos_cnum = lexbuf.Lexing.lex_curr_p.pos_cnum - len
	  } ;
        (* we don't increment the Odoc_comments_global.nb_chars *)
        special_comment_part2 lexbuf
      )
# 454 "odoc_lexer.ml"

  | 4 ->
# 231 "odoc_lexer.mll"
      ( 
        let c = (Lexing.lexeme_char lexbuf 0) in
        ajout_char_string c;
        if c = '\010' then incr line_number;
        incr Odoc_comments_global.nb_chars;
        special_comment lexbuf 
      )
# 465 "odoc_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_special_comment_rec lexbuf __ocaml_lex_state

and special_comment_part2 lexbuf =
    __ocaml_lex_special_comment_part2_rec lexbuf 22
and __ocaml_lex_special_comment_part2_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 241 "odoc_lexer.mll"
      ( 
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        if !comments_level = 1 then
          (* finally we return the description we kept *)
          let desc = 
            if !Odoc_args.remove_stars then
              remove_stars !description
             else
              !description
          in
          let remain = lecture_string () in
          let remain2 = 
            if !Odoc_args.remove_stars then
              remove_stars remain
             else
               remain
          in
          Description (desc, Some remain2)
        else
          (
           ajout_string s ;
           decr comments_level ;
           special_comment_part2 lexbuf
          )
      )
# 501 "odoc_lexer.ml"

  | 1 ->
# 269 "odoc_lexer.mll"
      (
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        ajout_string s;
        incr comments_level ;
        special_comment_part2 lexbuf
      )
# 512 "odoc_lexer.ml"

  | 2 ->
# 278 "odoc_lexer.mll"
      ( 
        let c = (Lexing.lexeme_char lexbuf 0) in
        ajout_char_string c;
        if c = '\010' then incr line_number;
        incr Odoc_comments_global.nb_chars;
        special_comment_part2 lexbuf 
      )
# 523 "odoc_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_special_comment_part2_rec lexbuf __ocaml_lex_state

and elements lexbuf =
    __ocaml_lex_elements_rec lexbuf 25
and __ocaml_lex_elements_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 288 "odoc_lexer.mll"
      ( 
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        elements lexbuf
      )
# 537 "odoc_lexer.ml"

  | 1 ->
# 294 "odoc_lexer.mll"
      ( incr line_number;
        incr Odoc_comments_global.nb_chars;
        print_DEBUG2 "newline";
        elements lexbuf )
# 545 "odoc_lexer.ml"

  | 2 ->
# 300 "odoc_lexer.mll"
      ( 
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        let s2 = String.sub s 1 ((String.length s) - 1) in
        print_DEBUG2 s2;
        match s2 with
          "param" ->
            T_PARAM 
         | "author" ->
            T_AUTHOR
         | "version" ->
             T_VERSION
         | "see" ->
             T_SEE
         | "since" ->
             T_SINCE
         | "deprecated" ->
             T_DEPRECATED
         | "raise" ->
             T_RAISES
         | "return" ->
             T_RETURN
         | s ->
             if !Odoc_args.no_custom_tags then
               raise (Failure (Odoc_messages.not_a_valid_tag s))
             else
               T_CUSTOM s
      )
# 577 "odoc_lexer.ml"

  | 3 ->
# 330 "odoc_lexer.mll"
      (
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        let s = Lexing.lexeme lexbuf in
        let s2 = remove_blanks s in
        print_DEBUG2 ("Desc "^s2);
        Desc s2
      )
# 588 "odoc_lexer.ml"

  | 4 ->
# 338 "odoc_lexer.mll"
      (
        EOF
      )
# 595 "odoc_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_elements_rec lexbuf __ocaml_lex_state

and simple lexbuf =
    __ocaml_lex_simple_rec lexbuf 32
and __ocaml_lex_simple_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 345 "odoc_lexer.mll"
      ( 
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        simple lexbuf
      )
# 609 "odoc_lexer.ml"

  | 1 ->
# 351 "odoc_lexer.mll"
      ( incr line_number;
        incr Odoc_comments_global.nb_chars;
        simple lexbuf 
      )
# 617 "odoc_lexer.ml"

  | 2 ->
# 357 "odoc_lexer.mll"
      (
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        incr comments_level;
        simple lexbuf
      )
# 626 "odoc_lexer.ml"

  | 3 ->
# 364 "odoc_lexer.mll"
      (
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        simple lexbuf
      )
# 635 "odoc_lexer.ml"

  | 4 ->
# 370 "odoc_lexer.mll"
      (
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        incr comments_level;
        simple lexbuf
      )
# 645 "odoc_lexer.ml"

  | 5 ->
# 378 "odoc_lexer.mll"
      ( 
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        incr comments_level;
        if !comments_level = 1 then
          (
           reset_string_buffer ();
           description := "";
           special_comment lexbuf 
          )
        else
          (
           ajout_string s;
           simple lexbuf
          )
      )
# 665 "odoc_lexer.ml"

  | 6 ->
# 396 "odoc_lexer.mll"
      ( EOF )
# 670 "odoc_lexer.ml"

  | 7 ->
# 399 "odoc_lexer.mll"
      (
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        decr comments_level ;
        simple lexbuf
      )
# 680 "odoc_lexer.ml"

  | 8 ->
# 407 "odoc_lexer.mll"
      (
        incr Odoc_comments_global.nb_chars;
        simple lexbuf
      )
# 688 "odoc_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_simple_rec lexbuf __ocaml_lex_state

;;

