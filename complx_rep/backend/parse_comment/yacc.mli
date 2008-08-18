type token =
  | Comment
  | Percent
  | Sharp
  | Single_rule
  | Double_rule
  | EOL
  | EOF
  | Greater
  | Less
  | Minus
  | Flag
  | NextLine
  | IDENT of ( string )

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> commented_line list
