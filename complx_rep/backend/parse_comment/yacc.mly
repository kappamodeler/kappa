/* File yacc.mly for the plectix compressor*/

 
%{

open Comment_sig


let succ () = (Data_structures.parser_line:=(!Data_structures.parser_line)+1)
%}

 /* declarations */


/* lexer tokens */

%token Comment Percent Sharp Single_rule Double_rule EOL EOF Direct Bi Flag NextLine
%token < string > IDENT   
%start main                   /* the entry point */
%type <commented_line list> main

%% /* rules */

Rule :
  Ident RuleSign Ident {($1,$2,$3)}
| Ident RuleSign {($1,$2,"")}
| RuleSign {"",$1,""}
| RuleSign Ident {"",$1,$2}

RuleSign :
  Direct {"->",1}
| Bi {"<->",2}


Bullshit :
  Bullshititem {$1}
| Bullshit Bullshititem {$2}

Bullshititem :
  NextLine {"\\\n"}

Comment_rule :
    Rule C {let (a,b,c)=$1 in {pref="";arrow=fst b;flag=None;lhs=a;rhs=c;dir=snd b;comments=$2}}
|   Rule   {let (a,b,c)=$1 in {pref="";arrow=fst b;flag=None;lhs=a;rhs=c;dir=snd b;comments=""}}
|   Flag FlagIdent Flag Rule {let (a,b,c)=$4 in {pref="";arrow=fst b;flag=Some $2;lhs=a;rhs=c;dir=snd b;comments=""}}
|   Flag FlagIdent Flag Rule C {let (a,b,c)=$4 in {pref="";arrow=fst b;flag=Some $2;lhs=a;rhs=c;dir=snd b;comments=$5}}
|   Ident Flag FlagIdent Flag Rule {let (a,b,c)=$5 in {pref=$1;arrow=fst b;flag=Some $3;lhs=a;rhs=c;dir=snd b;comments=""}}
|   Ident Flag FlagIdent Flag Rule C {let (a,b,c)=$5 in {pref=$1;arrow=fst b;flag=Some $3;lhs=a;rhs=c;dir=snd b;comments=$6}}

Line :
  D EOL {let _ = succ () in Decl($1)}
| C EOL {let _ = succ () in Mutt($1)}
| Ident C EOL {let _ = succ() in Mutt($1^$2)}
| Comment_rule EOL {let _ = succ () in Rgl($1)}
| EOL {let _ = succ () in Mutt("")}

LastLine :
  D EOF {Decl($1)}
| C EOF {Mutt($1)}
| Comment_rule EOF {Rgl($1)}


FlagIdent :
  Ident FlagIdent {($1)^($2)}
| Ident           {$1}
| Sharp {"#"}
| Sharp FlagIdent {"#"^$2}

Ident :
  IDENT Ident {$1^$2}
| IDENT {$1}
| Bullshititem Ident {$1^$2}

C :
  C Comment_char {$1^$2}
| Sharp {"#"}

Comment_char :
  IDENT {$1}
| Flag {"'"}
| Direct {"->"}
| Bi {"<->"}
| Bullshititem {$1}
| Percent {"%"} 
| Sharp {"#"} 

D : 
  D Comment_char {$1^$2}
| Percent {"%"} 

main :
  EOF {[]}
| LastLine {[$1]}
| Line main {$1::$2}

