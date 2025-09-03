open Decl_parser

module TyM = struct
  type t = Type_parser.cyclic

  let compare t1 t2 = Int.compare t1.Type_parser.id t2.Type_parser.id
  let to_string (t : t) : string = Type_parser.pp_cyclic t
end

module ConstrM = struct
  type t = string

  let compare = String.compare
  let to_string (s : t) : string = s
end

module JK = Udd_jkind_solver.Make (Axis_lattice) (TyM) (ConstrM)

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
  | CVar _ -> ops.rigid c

let lookup table (name : string) : JK.constr_decl =
  match List.assoc_opt name table with
  | None -> failwith ("infer8: unknown constructor " ^ name)
  | Some it ->
    let args = it.params in
    let kind : JK.ckind = fun ops -> ops.kind_of it.rhs_cyclic in
    let decl : JK.constr_decl = Ty { args; kind; abstract = it.abstract } in
    decl

let env_of_program (prog : program) : env =
  let table =
    List.fold_left (fun acc (it : decl_item) -> (it.name, it) :: acc) [] prog
  in
  { lookup = lookup table; kind_of }

let run_program (prog : Decl_parser.program) : string =
  let env = env_of_program prog in
  let solver = JK.make_solver env in
  prog
  |> List.map (fun (it : Decl_parser.decl_item) ->
         let base_poly, coeffs = JK.constr_kind_poly solver it.name in
         let body =
           let rec loop i acc =
             if i > it.arity then List.rev acc
             else if i = 0 then
               loop 1 (Printf.sprintf "0 ↦ %s" (JK.pp base_poly) :: acc)
             else
               let ci = List.nth coeffs (i - 1) in
               loop (i + 1) (Printf.sprintf "%d ↦ %s" i (JK.pp ci) :: acc)
           in
           loop 0 [] |> String.concat ", "
         in
         Printf.sprintf "%s: {%s}" it.name body)
  |> String.concat "\n"

(* Debug: dump UDD debug structures for a constructor's base and coefficients *)
let debug_constr (prog : Decl_parser.program) ~(constr : string) : string =
  let env = env_of_program prog in
  let solver = JK.make_solver env in
  let base, coeffs = JK.constr_kind_poly solver constr in
  let b = JK.pp_debug base in
  let b_forced = JK.pp_debug base in
  let parts =
    ref [ "-- base.debug --\n" ^ b; "\n-- base.forced.debug --\n" ^ b_forced ]
  in
  List.iteri
    (fun i c ->
      parts :=
        !parts
        @ [
            Printf.sprintf "\n-- %s.%d.debug --\n%s" constr (i + 1)
              (JK.pp_debug c);
            Printf.sprintf "\n-- %s.%d.forced.debug --\n%s" constr (i + 1)
              (JK.pp_debug c);
          ])
    coeffs;
  String.concat "\n" !parts
