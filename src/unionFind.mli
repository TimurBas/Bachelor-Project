module UnionFind : sig
  type 'a t
  val make_set : 'a -> 'a t
  val find : 'a t -> 'a
  val union : 'a t -> 'a t -> f:('a -> 'a -> int) -> unit
end