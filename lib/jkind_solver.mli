(* The JKind solver. *)

(** Funconstr inputs:
  - A type of types ty
  - A type of construconstr names constr
  - The lattice lat

  Output:
  - A type e of lattice terms
  - Operations 
    * const : lat -> kind
    * join : kind list -> kind
    * modality : lat -> kind -> kind
    * constr : constr -> kind list -> kind
    * link : ty -> kind
  - A type of environments, containing:
    * expand : ty -> kind
    * lookup : constr -> { args : ty list; body: ty; abstract: bool }
  - The following procedures:
    * leq : env -> kind -> kind -> bool
    * round_up : env -> kind -> lat
    * normalize : env -> kind -> (lat * atom list) list
*)

module Make
    (C : Lattice_intf.LATTICE)
    (I : sig
      type ty
      type constr
    end) : sig
  type ty = I.ty
  type constr = I.constr
  type lat = C.t
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
  type atom = { constr : constr; arg_index : int }

  val normalize : env -> ckind -> (lat * atom list) list
  val leq : env -> ckind -> ckind -> bool
  val round_up : env -> ckind -> lat
end
