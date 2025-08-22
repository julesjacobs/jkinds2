(* The JKind solver. *)

(* Funconstr inputs: - A type of types ty - A type of construconstr names constr
   - The lattice lat

   Output: - A type e of lattice terms - Operations * const : lat -> expr * join
   : expr list -> expr * modality : lat -> expr -> expr * constr : constr ->
   expr list -> expr * link : ty -> expr - A type of environments, containing: *
   expand : ty -> expr * lookup : constr -> { args : ty list; body: ty;
   abstract: bool } - The following procedures: * leq : env -> expr -> expr ->
   bool * round_up : env -> expr -> lat * normalize : env -> expr -> (lat * atom
   list) list *)

module Make
    (C : Lattice_intf.LATTICE)
    (I : sig
      type ty
      type constr
    end) : sig
  type ty = I.ty
  type constr = I.constr
  type lat = C.t

  (* Abstract expression language over kinds *)
  type expr

  val const : lat -> expr
  val join : expr list -> expr
  val modality : lat -> expr -> expr
  val constr : constr -> expr list -> expr
  val link : ty -> expr

  type constr_decl = { args : ty list; body : ty; abstract : bool }
  type env = { expand : ty -> expr; lookup : constr -> constr_decl }

  val leq : env -> expr -> expr -> bool
  val round_up : env -> expr -> lat

  type atom = { constr : constr; arg_index : int }

  val normalize : env -> expr -> (lat * atom list) list
end
