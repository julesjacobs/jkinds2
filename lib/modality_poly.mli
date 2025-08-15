module type COEFF = Lattice_intf.LATTICE

module Make (C : COEFF) : sig
  type atom = { ctor : string; index : int }

  type coeff = C.t

  type t

  val zero : t
  val id : t
  val const : coeff -> t
  val of_atom : atom -> t
  val max : t -> t -> t
  val compose : t -> t -> t
  val co_sub : t -> t -> t
  val substitute : (atom -> t option) -> t -> t
  val equal : t -> t -> bool
  val leq : t -> t -> bool
  val terms : t -> (atom list * coeff) list
  val ceil_coeff : t -> coeff
  val floor_coeff : t -> coeff
  val ceil : t -> t
  val floor : t -> t
  val pp : t -> string
end


