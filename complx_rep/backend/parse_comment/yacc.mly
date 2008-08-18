/* File yacc.mly for the plectix compressor*/

 
%{

open Comment_sig

%}


 /* declarations */


/* lexer tokens */

%token Comment Percent Sharp Single_rule Double_rule EOL EOF Greater Less Minus Flag NextLine
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
  Leftarrow {$1,1}
| Less Bullshit Leftarrow {"<"^$2^$3,2}
| Less Leftarrow {"<"^$2,2}

Leftarrow :
  Minus Greater {"->"}
| Minus Bullshit Greater {"-"^$2^">"}

Bullshit :
  Bullshititem {$1}
| Bullshit Bullshititem {$2}

Bullshititem :
  NextLine {"\\\n"}

Comment_rule :
    Rule C {let (a,b,c)=$1 in {arrow=fst b;flag=None;lhs=a;rhs=c;dir=snd b;comments=$2}}
|   Rule   {let (a,b,c)=$1 in {arrow=fst b;flag=None;lhs=a;rhs=c;dir=snd b;comments=""}}
|   Flag FlagIdent Flag Rule {let (a,b,c)=$4 in {arrow=fst b;flag=Some $2;lhs=a;rhs=c;dir=snd b;comments=""}}
|   Flag FlagIdent Flag Rule C {let (a,b,c)=$4 in {arrow=fst b;flag=Some $2;lhs=a;rhs=c;dir=snd b;comments=$5}}

Line :
  D EOL {Decl($1)}
| C EOL {Mutt($1)}
| Comment_rule EOL {Rgl($1)}
| EOL {Mutt("")}

LastLine :
  D EOF {Decl($1)}
| C EOF {Mutt($1)}
| Comment_rule EOF {Rgl($1)}

Char :
   Less {"<"}
| Minus {"-"}
| Greater {">"}

FlagIdent :
  Ident FlagIdent {($1)^($2)}
| Char FlagIdent  {($1)^($2)}
| Char            {$1}
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
| Char {$1}
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

