open Decl_parser

module TyM = struct
  type t = Type_parser.cyclic

  let compare_ty : t -> t -> int = Stdlib.compare
end

module ConstrM = struct
  type t = string

  let compare = String.compare
end

module JK = Jkind_solver1.Make (Axis_lattice) (TyM) (ConstrM)

type env = JK.env
type lat = Axis_lattice.t
type atom = JK.atom

let kind_of (c : Type_parser.cyclic) : JK.ckind =
 fun (ops : JK.ops) ->
  let open Type_parser in
  match c.node with
  | CUnit -> ops.const Axis_lattice.bot
  | CMod_const lv -> ops.const (Axis_lattice.encode ~levels:lv)
  | CMod_annot (t, lv) ->
    let k = ops.kind_of t in
    ops.modality (Axis_lattice.encode ~levels:lv) k
  | CPair (a, b) | CSum (a, b) -> ops.join [ ops.kind_of a; ops.kind_of b ]
  | CCtor (name, args) ->
    let arg_kinds = List.map (fun t -> ops.kind_of t) args in
    ops.constr name arg_kinds
  | CVar _ -> failwith "kind_of: should not reach variables"

let env_of_program (prog : program) : env =
  let table =
    List.fold_left (fun acc (it : decl_item) -> (it.name, it) :: acc) [] prog
  in
  let lookup (name : string) : JK.constr_decl =
    match List.assoc_opt name table with
    | None -> failwith ("infer3: unknown constructor " ^ name)
    | Some it ->
      let args = it.params in
      let kind = kind_of it.rhs_cyclic in
      let decl : JK.constr_decl = { args; kind; abstract = it.abstract } in
      decl
  in
  { lookup; kind_of }

let normalize_decl (env : env) (it : decl_item) : (lat * atom list) list =
  JK.normalize env (fun ops -> ops.kind_of it.rhs_cyclic)

let leq_kinds (env : env) (lhs : Type_parser.cyclic) (rhs : Type_parser.cyclic)
    : bool =
  JK.leq env (fun ops -> ops.kind_of lhs) (fun ops -> ops.kind_of rhs)

let round_up_kind (env : env) (c : Type_parser.cyclic) : lat =
  JK.round_up env (fun ops -> ops.kind_of c)
