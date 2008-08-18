type ouput_mode  = 
    MATLAB | MATHEMATICA | LATEX | TXT | DUMP 


type print = 
    {print_string:string ->unit;
     print_int:   int -> unit;
     print_newline: unit -> unit;
     print_float: float -> unit;
     chan:out_channel list  }


type print_desc = 
    {dump: print option;
     txt:print option;
     kappa:print option;
     matlab: print option;
     mathematica: print option;
     latex: print option}
