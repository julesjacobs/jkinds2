open Decl_parser

module TyM = struct
  type t = Type_parser.cyclic

  (* Assign stable ids per Ty node to distinguish equal-structure vars from different scopes *)
  let next_id = ref 0
  let ids : (t, int) Hashtbl.t = Hashtbl.create 256
  let id_of (x : t) : int = match Hashtbl.find_opt ids x with Some i -> i | None -> let i = !next_id in incr next_id; Hashtbl.add ids x i; i

  let compare_ty (a : t) (b : t) : int = Int.compare (id_of a) (id_of b)
  let to_string (_ : t) = "ty"
end

module ConstrM = struct
  type t = string

  let compare = String.compare
  let to_string (s : t) = s
end

module JK = Jkind_solver.Make (Axis_lattice) (TyM) (ConstrM)

type env = JK.env
type lat = Axis_lattice.t
type atom = JK.atom

let kind_of (c : Type_parser.cyclic) : JK.ckind =
 fun (ops : JK.ops) ->
  let open Type_parser in
  match c with
  | CUnit -> ops.const Axis_lattice.bot
  | CMod_const lv -> ops.const (Axis_lattice.encode ~levels:lv)
  | CMod_annot (t, lv) ->
    let k = ops.kind_of t in
    ops.modality (Axis_lattice.encode ~levels:lv) k
  | CPair (a, b) | CSum (a, b) -> ops.join [ ops.kind_of a; ops.kind_of b ]
  | CCtor (name, args) ->
    let arg_kinds = List.map (fun t -> ops.kind_of t) args in
    ops.constr name arg_kinds
  | MuLink r -> ops.kind_of !r
  | CVar _ -> ops.kind_of c

let env_of_program (prog : program) : env =
  let table =
    List.fold_left (fun acc (it : decl_item) -> (it.name, it) :: acc) [] prog
  in
  (* Canonical formal parameter nodes per constructor name *)
  let formal_cvars : (string, Type_parser.cyclic array) Hashtbl.t = Hashtbl.create 64 in
  let lookup (name : string) : JK.constr_decl =
    match List.assoc_opt name table with
    | None -> failwith ("infer3: unknown constructor " ^ name)
    | Some it ->
      let args =
        let arr =
          match Hashtbl.find_opt formal_cvars name with
          | Some a when Array.length a = it.arity -> a
          | _ ->
            let a = Array.init it.arity (fun i -> Type_parser.CVar (i+1)) in
            Hashtbl.replace formal_cvars name a; a
        in
        Array.to_list arr
      in
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

let pp_terms (terms : (lat * atom list) list) : string =
  let open Axis_lattice in
  match terms with
  | [] -> "⊥"
  | _ ->
      let term_body ((coeff, atoms) : lat * atom list) : string * bool =
        let vs =
          atoms
          |> List.map (fun (a : atom) -> Printf.sprintf "%s.%d" (ConstrM.to_string a.constr) a.arg_index)
          |> List.sort String.compare
        in
        let is_top = equal coeff top in
        match (vs, is_top) with
        | [], true -> ("⊤", false)
        | [], false -> (to_string coeff, false)
        | _ :: _, true -> (String.concat " ⊓ " vs, List.length vs > 1)
        | _ :: _, false ->
            let body = to_string coeff ^ " ⊓ " ^ String.concat " ⊓ " vs in
            (body, true)
      in
      let items =
        terms
        |> List.map term_body
        |> List.sort (fun (a, _) (b, _) -> String.compare a b)
      in
      let n_terms = List.length items in
      items
      |> List.map (fun (body, has_meet) -> if n_terms > 1 && has_meet then "(" ^ body ^ ")" else body)
      |> String.concat " ⊔ "

let run_program (prog : Decl_parser.program) : string =
  let env = env_of_program prog in
  (* Warm up: touch all constructors to set up LFP/GFP before entering query phase. *)
  let _warmup =
    let k = fun (ops: JK.ops) ->
      let kinds =
        List.map
          (fun (it: decl_item) ->
            let args = List.init it.arity (fun _ -> ops.const Axis_lattice.bot) in
            ops.constr it.name args)
          prog
      in
      ops.join kinds
    in
    (* This call will enter query phase after building the above polynomial. *)
    ignore (JK.normalize env k)
  in
  let lines =
    List.map
      (fun (it : decl_item) ->
        let arity = it.arity in
        let entry0 = Printf.sprintf "0 ↦ %s" (pp_terms (JK.atom_terms env it.name 0)) in
        let entries =
          let rec loop i acc =
            if i > arity then List.rev acc
            else
              let s = Printf.sprintf "%d ↦ %s" i (pp_terms (JK.atom_terms env it.name i)) in
              loop (i + 1) (s :: acc)
          in
          loop 1 []
        in
        it.name ^ ": {" ^ String.concat ", " (entry0 :: entries) ^ "}"
      )
      prog
  in
  String.concat "\n" lines
