(* Lexeur pour le Pi_calclul *)
{
open Lexing;;
open Yacc;; (* Type token defined in yacc.mli *)
exception Eof;;
} 
rule token = parse
   "\\\n" {NextLine}
|  ('#') {Sharp} 
| eof    { EOF }
| "-" {Minus}
| ">" {Greater}
| "<" {Less}
| '\n' |  "\r\n" | "\n\r" {EOL}
| '%' {Percent}
| '\'' {Flag}
| _ {IDENT(lexeme lexbuf)}

      
