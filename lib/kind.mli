module Var : sig
  type t = int
  val a0 : t
  val pp : t -> string
end

type t

val empty : t
val get : t -> Var.t -> Modality.t
val find_opt : t -> Var.t -> Modality.t option
val set : t -> Var.t -> Modality.t -> t
val max : t -> t -> t
val apply : Modality.t -> t -> t
val pp : t -> string
val normalize : t -> t
val equal : t -> t -> bool
val leq : t -> t -> bool

val ceil : t -> t
val floor : t -> t

exception Substitution_error of string

val substitute_using : (Modality.atom -> Modality.t option) -> t -> t

val zero_entries : t -> t
