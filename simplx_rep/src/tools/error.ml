exception Syntax of (string * int)
exception Runtime of string
exception Runtime2 of string
exception Found of string
exception Too_expensive 
exception Not_handled_yet of string 

let runtime s = raise (Runtime s)
let expected s = raise (Runtime2 s)
let syntax (s,l) = raise (Syntax (s,l))
let too_expensive () = raise Too_expensive
let warning s = Printf.printf "WARNING: %s\n" s ; flush stdout
let found s = raise (Found s)
