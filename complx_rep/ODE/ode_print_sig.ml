type ouput_mode  = 
    MATLAB | MATHEMATICA | LATEX | TXT | DUMP | DATA | REACTIONS


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
     data:print option;
     matlab: print option;
     matlab_aux: print option;
     mathematica: print option;
     reactions: print option;
     latex: print option;
     matlab_jacobian: print option;
     matlab_size: print option;
     matlab_activity: print option;
     matlab_obs:print option;
     matlab_init:print option }
