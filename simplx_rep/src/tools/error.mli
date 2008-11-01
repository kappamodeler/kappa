exception Syntax of (string * int)
exception Runtime of string
exception Runtime2 of string
exception Found of string
exception Too_expensive
exception Not_handled_yet of string 

val runtime : string -> 'a
val expected: string -> 'a
val syntax : string * int -> 'a
val too_expensive: unit -> 'a
val warning: string -> unit
val found: string -> 'a

