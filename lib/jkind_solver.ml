module Make
    (Lat : Lattice_intf.LATTICE)
    (Ty : sig
      type t

      val compare_ty : t -> t -> int
    end)
    (Constr : sig
      type t

      val compare : t -> t -> int
    end) : sig
  type ty = Ty.t
  type constr = Constr.t
  type lat = Lat.t
  type kind

  type ops = {
    const : lat -> kind;
    join : kind list -> kind;
    modality : lat -> kind -> kind;
    constr : constr -> kind list -> kind;
    kind_of : ty -> kind;
  }

  type ckind = ops -> kind
  type constr_decl = { args : ty list; body : ty; abstract : bool }
  type env = { kind_of : ty -> ckind; lookup : constr -> constr_decl }

  val normalize : env -> ckind -> (lat * atom list) list
  val leq : env -> ckind -> ckind -> bool
  val round_up : env -> ckind -> lat

  type atom = { constr : constr; arg_index : int }
end = struct
  type ty = Ty.t
  type constr = Constr.t
  type lat = Lat.t
  type atom = { constr : constr; arg_index : int }

  module Var = struct
    type t = Atom of atom | Ty of ty

    let compare (a : t) (b : t) : int =
      match (a, b) with
      | Atom a, Atom b -> (
        match Constr.compare a.constr b.constr with
        | 0 -> Int.compare a.arg_index b.arg_index
        | c -> c)
      | Ty a, Ty b -> Ty.compare_ty a b
      | Atom _, Ty _ -> -1
      | Ty _, Atom _ -> 1
  end

  module L = Lattice_solver.Make (Lat) (Var)

  type kind = L.poly

  type ops = {
    const : lat -> kind;
    join : kind list -> kind;
    modality : lat -> kind -> kind;
    constr : constr -> kind list -> kind;
    kind_of : ty -> kind;
  }

  type ckind = ops -> kind
  type constr_decl = { args : ty list; body : ty; abstract : bool }
  type env = { kind_of : ty -> ckind; lookup : constr -> constr_decl }

  let normalize = failwith "unimplemented"
  let leq = failwith "unimplemented"
  let round_up = failwith "unimplemented"
end
