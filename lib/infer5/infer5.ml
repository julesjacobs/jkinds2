open Decl_parser

module TyM = struct
  type t = Type_parser.cyclic

  let compare_ty : t -> t -> int = Stdlib.compare
  let to_string (_ : t) : string = "<ty>"
end

module ConstrM = struct
  type t = string

  let compare = String.compare
  let to_string (s : t) : string = s
end

module JK = Jkind_solver.Make (Axis_lattice) (TyM) (ConstrM)

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

let pp_coeff = Axis_lattice.to_string
let pp_atom (a : atom) : string = Printf.sprintf "%s.%d" a.constr a.arg_index

let pp_terms (ts : (lat * atom list) list) : string =
  match ts with
  | [] -> "⊥"
  | _ ->
    let pp_term (c, atoms) =
      let cstr = Printf.sprintf "[%s]" (pp_coeff c) in
      match atoms with
      | [] -> cstr
      | _ ->
        let astr = atoms |> List.map pp_atom |> String.concat " ⊓ " in
        Printf.sprintf "(%s ⊓ %s)" cstr astr
    in
    ts |> List.map pp_term |> String.concat " ⊔ "

let run_program (prog : Decl_parser.program) : string =
  let env = env_of_program prog in
  let base_terms name : (lat * atom list) list =
    JK.normalize env (fun ops -> ops.constr name [])
  in
  let coeff_terms name arity i : (lat * atom list) list =
    JK.normalize env (fun ops ->
        let ks =
          List.init arity (fun j ->
              if j + 1 = i then ops.const Axis_lattice.top
              else ops.const Axis_lattice.bot)
        in
        ops.constr name ks)
  in
  let filter_out_base name (ts : (lat * atom list) list) :
      (lat * atom list) list =
    List.filter
      (fun (_c, atoms) ->
        not
          (List.exists
             (fun (a : atom) -> a.constr = name && a.arg_index = 0)
             atoms))
      ts
  in
  prog
  |> List.map (fun (it : Decl_parser.decl_item) ->
         let base = base_terms it.name in
         let entries =
           let rec loop i acc =
             if i > it.arity then List.rev acc
             else if i = 0 then loop 1 ((0, base) :: acc)
             else
               let ti =
                 coeff_terms it.name it.arity i |> filter_out_base it.name
               in
               loop (i + 1) ((i, ti) :: acc)
           in
           loop 0 []
         in
         let body =
           entries
           |> List.map (fun (i, ts) -> Printf.sprintf "%d ↦ %s" i (pp_terms ts))
           |> String.concat ", "
         in
         Printf.sprintf "%s: {%s}" it.name body)
  |> String.concat "\n"
