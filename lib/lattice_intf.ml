module type LATTICE = sig
  type t
  val bot   : t
  val top   : t
  val join  : t -> t -> t
  val meet  : t -> t -> t
  val leq   : t -> t -> bool
  val co_sub: t -> t -> t
  val equal : t -> t -> bool
end



