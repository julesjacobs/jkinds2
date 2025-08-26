open Decl_parser

module TyM = struct
  type t = Type_parser.cyclic

  let compare_ty t1 t2 = Int.compare t1.Type_parser.id t2.Type_parser.id
  let to_string (t : t) : string = Type_parser.pp_cyclic t
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
  | CVar _ -> failwith ("infer5: should not reach variable " ^ TyM.to_string c)

let env_of_program (prog : program) : env =
  let table =
    List.fold_left (fun acc (it : decl_item) -> (it.name, it) :: acc) [] prog
  in
  let lookup (name : string) : JK.constr_decl =
    match List.assoc_opt name table with
    | None -> failwith ("infer5: unknown constructor " ^ name)
    | Some it ->
      let args = it.params in
      let kind : JK.ckind = fun ops -> ops.kind_of it.rhs_cyclic in
      let decl : JK.constr_decl = { args; kind; abstract = it.abstract } in
      decl
  in
  { lookup; kind_of }

module OrderedAtom = struct
  type t = atom

  let compare (a : t) (b : t) : int =
    match String.compare a.constr b.constr with
    | 0 -> Int.compare a.arg_index b.arg_index
    | c -> c

  let to_string (a : t) : string = Printf.sprintf "%s.%d" a.constr a.arg_index
end

module PpPoly = Lattice_polynomial.Make (Axis_lattice) (OrderedAtom)

let poly_of_terms (ts : (lat * atom list) list) : PpPoly.t =
  let to_vars (atoms : atom list) : PpPoly.vars =
    List.fold_left
      (fun acc a -> PpPoly.VarSet.add a acc)
      PpPoly.VarSet.empty atoms
  in
  ts |> List.map (fun (c, atoms) -> (to_vars atoms, c)) |> PpPoly.of_list

let pp_terms (ts : (lat * atom list) list) : string =
  let poly = poly_of_terms ts in
  PpPoly.pp ~pp_var:OrderedAtom.to_string ~pp_coeff:Axis_lattice.to_string poly

let run_program (prog : Decl_parser.program) : string =
  let env = env_of_program prog in
  let solver = JK.make_solver env in
  let base_terms name : (lat * atom list) list =
    JK.normalize solver (fun ops -> ops.constr name [])
  in
  let coeff_terms name arity i : (lat * atom list) list =
    JK.normalize solver (fun ops ->
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
         let base_terms_list = base_terms it.name in
         let base_poly = poly_of_terms base_terms_list in
         let entries =
           let rec loop i acc =
             if i > it.arity then List.rev acc
             else if i = 0 then loop 1 ((0, base_terms_list) :: acc)
             else
               let ti_terms =
                 coeff_terms it.name it.arity i |> filter_out_base it.name
               in
               let ti_poly = poly_of_terms ti_terms in
               let ti_minus_base = PpPoly.co_sub_approx ti_poly base_poly in
               let ti_terms' =
                 PpPoly.to_list ti_minus_base
                 |> List.map (fun (s, c) -> (c, PpPoly.VarSet.elements s))
               in
               loop (i + 1) ((i, ti_terms') :: acc)
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
