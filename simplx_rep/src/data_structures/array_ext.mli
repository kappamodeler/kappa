module type Content = sig type t val default : t end
module Make :
  functor (T : Content) ->
    sig
      type content = T.t
      type 'a t 
      val copy : 'a t -> 'a t
      val size : 'a t -> int
      val create : int -> 'a t
      val remove : 'a -> 'a t -> 'a t
      val remove_index: int -> 'a t -> 'a t
      val add : 'a -> content -> 'a t -> 'a t
      val find : 'a -> 'a t -> content
      val iter : ('a -> content -> unit) -> 'a t -> unit
      val fold : ('a -> content -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val random : 'a t -> 'a * content
      val is_empty : 'a t -> bool
      val mem : 'a -> 'a t -> bool
    end
