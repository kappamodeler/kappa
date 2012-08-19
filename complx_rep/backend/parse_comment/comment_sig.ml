type commented_rule = 
    {pref:string;
      flag:string option;
    arrow:string;
    lhs:string;
      rhs:string;
      dir:int;
      comments:string}

type commented_line = 
  | Mutt of string 
  | Rgl of commented_rule 
  | Decl of string
  | Init_line of string
  | Obs_line of string 
