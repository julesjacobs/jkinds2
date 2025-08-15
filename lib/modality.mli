type atom = { ctor : string; index : int }

type t

val zero : t
val id : t
val of_atom : atom -> t
val of_levels : int array -> t
val max : t -> t -> t
val compose : t -> t -> t
val co_sub : t -> t -> t
val pp : t -> string

val substitute : (atom -> t option) -> t -> t

val equal : t -> t -> bool
val leq : t -> t -> bool

val ceil : t -> t
val floor : t -> t
