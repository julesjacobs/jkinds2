module type LATTICE = Lattice_intf.LATTICE

module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module Make (C : LATTICE) (V : ORDERED) : sig
  module VarSet : Set.S with type elt = V.t
  module VarMap : Map.S with type key = V.t

  type coeff = C.t
  type vars  = VarSet.t

  (* Canonical polynomial type. All constructors and operations return canonical form. *)
  type t

  val empty     : t
  val is_empty  : t -> bool

  val const     : coeff -> t
  val var       : V.t -> t

  (* Build from a list of (vars, coeff) and canonicalize; convenient for tests. *)
  val of_list   : (vars * coeff) list -> t
  val of_terms  : (vars * coeff) list -> t  (* Alias for of_list *)
  val terms     : t -> (vars * coeff) list
  val canonicalize : t -> t

  include LATTICE with type t := t

  val support   : t -> VarSet.t

  val subst     : subs:t VarMap.t -> t -> t
  val subst1    : v:V.t -> by:t -> t -> t

  val eval      : (V.t -> C.t) -> t -> C.t

  val pp        : ?pp_var:(V.t -> string) -> ?pp_coeff:(coeff -> string) -> t -> string
end


