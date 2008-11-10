exception Syntax of (string * int)
exception Runtime of string
exception Runtime2 of string
exception Found of string
exception Too_expensive
exception Not_handled_yet of string 

val store_error_info: (string option * int option * string option) -> unit

val runtime: (string option * int option * string option) -> string -> 'a
val expected: (string option * int option * string option) -> string -> 'a
val syntax: (string option * int option * string option) -> string * int -> 'a
val too_expensive: (string option * int option * string option) -> 'a
val warning: string -> unit
val found: (string option * int option * string option) -> string -> 'a

