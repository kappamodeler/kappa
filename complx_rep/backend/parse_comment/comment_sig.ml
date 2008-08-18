type commented_rule = 
    {flag:string option;
    arrow:string;
    lhs:string;
      rhs:string;
      dir:int;
      comments:string}

type commented_line = 
    Mutt of string | Rgl of commented_rule | Decl of string
