val bench_mode : bool ref

type param = I of int | F of float
exception True
exception False
module StringSet :
  sig
    type elt = string
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
module StringMap :
  sig
    type key = string
    type +'a t
    val empty : 'a t
    val get : 'a t -> key * 'a
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val random : 'a t -> key * 'a
    val size : 'a t -> int
  end
module IntMap :
  sig
    type key = int
    type +'a t
    val empty : 'a t
    val get : 'a t -> key * 'a
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val random : 'a t -> key * 'a
    val size : 'a t -> int
  end
module IntSet :
  sig
    type elt = int
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
module FloatMap :
  sig
    type key = float
    type +'a t
    val empty : 'a t
    val get : 'a t -> key * 'a
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val random : 'a t -> key * 'a
    val size : 'a t -> int
  end
module PortMap :
  sig
    type key = int * string
    type +'a t
    val empty : 'a t
    val get : 'a t -> key * 'a
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val random : 'a t -> key * 'a
    val size : 'a t -> int
  end
module PortSet :
  sig
    type elt = int * string
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
module CoordMap :
  sig
    type key = int list
    type +'a t
    val empty : 'a t
    val get : 'a t -> key * 'a
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val random : 'a t -> key * 'a
    val size : 'a t -> int
  end
module CoordSet :
  sig
    type elt = int list
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
type assoc = int IntMap.t
module AssocArray :
  sig
    type content = assoc
    type 'a t
    val copy : 'a t -> 'a t
    val size : 'a t -> int
    val create : int -> 'a t
    val remove : 'a -> 'a t -> 'a t
    val add : 'a -> content -> 'a t -> 'a t
    val find : 'a -> 'a t -> content
    val iter : ('a -> content -> unit) -> 'a t -> unit
    val fold : ('a -> content -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val random : 'a t -> 'a * content
    val is_empty : 'a t -> bool
    val mem : 'a -> 'a t -> bool
  end
module CoordSetArray :
  sig
    type content = CoordSet.t
    type 'a t
    val copy : 'a t -> 'a t
    val size : 'a t -> int
    val create : int -> 'a t
    val remove : 'a -> 'a t -> 'a t
    val add : 'a -> content -> 'a t -> 'a t
    val find : 'a -> 'a t -> content
    val iter : ('a -> content -> unit) -> 'a t -> unit
    val fold : ('a -> content -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val random : 'a t -> 'a * content
    val is_empty : 'a t -> bool
    val mem : 'a -> 'a t -> bool
  end
module Palette :
  sig
    type t
    type color = float * float * float
    val find : int -> t -> color
    val add : int -> color -> t -> t
    val mem : int -> t -> bool
    val empty : t
    val new_color : unit -> color
    val grey : int -> string
    val string_of_color : color -> string
  end
val color_of_mrk : string -> string
val string_of_set :
  ('a -> 'b) ->
  (('a -> 'b list -> 'b list) -> 'c -> 'd list -> string list) ->
  'c -> string
val string_of_map :
  ('a -> string) ->
  ('b -> string) ->
  (('a -> 'b -> string list -> string list) -> 'c -> 'd list -> string list) ->
  'c -> string
val string_of_port : int * string -> string
val string_of_coord : int list -> string
val sorted_insert : 'a -> 'a list -> 'a list
val chop_extension : string -> string
val chrono: float -> float
val gettime: unit -> float
val random_time_advance: float -> int -> float
