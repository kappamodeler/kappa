type t
val empty : unit -> t
val add : string -> int -> string -> t -> t
val clashing_on_names : ?debug:bool -> (bool * bool) -> t -> t -> bool

