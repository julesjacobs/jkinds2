(* The JKind solver. *)

(** Functor inputs:
    - The coefficient lattice [Lat]
    - The type domain [Ty] with a comparison
    - The constructor domain [Constr] with a comparison

    Output:
    - A Church-encoded kind [ckind = ops -> kind], where [kind] is the backend
      semantic domain and [ops] provides the constructors. The solver interprets
      [ckind] under a lattice-polynomial backend. *)

module Make
    (Lat : Lattice_intf.LATTICE)
    (Ty : sig
      type t

      val compare : t -> t -> int
      val to_string : t -> string
    end)
    (Constr : sig
      type t

      val compare : t -> t -> int
      val to_string : t -> string
    end) : sig
  type ty = Ty.t
  type constr = Constr.t
  type lat = Lat.t
  type kind
  type solver

  type ops = {
    const : lat -> kind;
    join : kind list -> kind;
    modality : lat -> kind -> kind;
    constr : constr -> kind list -> kind;
    kind_of : ty -> kind;
    rigid : ty -> kind;
  }

  type ckind = ops -> kind
  type constr_decl = { args : ty list; kind : ckind; abstract : bool }
  type env = { kind_of : ty -> ckind; lookup : constr -> constr_decl }
  type atom = { constr : constr; arg_index : int }

  module RigidName : sig
    type t = Atom of atom | Ty of ty

    val compare : t -> t -> int
    val to_string : t -> string
  end

  type poly = Ldd.Make(Lat)(RigidName).node

  val make_solver : env -> solver
  val constr_kind_poly : solver -> constr -> poly * poly list
  val normalize : solver -> ckind -> (lat * atom list) list
  val leq : solver -> ckind -> ckind -> bool
  val round_up : solver -> ckind -> lat
end
