open Type_syntax

exception Parse_error of string

(* Extended: recursive types with mu / &'bN *)

type mu_raw =
  | UnitR
  | PairR of mu_raw * mu_raw
  | SumR of mu_raw * mu_raw
  | CR of string * mu_raw list
  | VarR of int (* 'aN *)
  | ModAnnotR of mu_raw * int array
  | ModConstR of int array
  | MuR of int * mu_raw (* mu 'bN. body *)
  | RecvarR of int (* &'bN *)

(* Parser entrypoints are now provided by the Menhir-based driver. Use
   Type_menhir_driver.parse_mu instead. *)

(* Lowering: turn mu_raw into the simple Type_syntax.t when no mu/rec vars *)
let rec to_simple_exn (t : mu_raw) : Type_syntax.t =
  match t with
  | UnitR -> Unit
  | PairR (a, b) -> Pair (to_simple_exn a, to_simple_exn b)
  | SumR (a, b) -> Sum (to_simple_exn a, to_simple_exn b)
  | CR (name, args) -> C (name, List.map to_simple_exn args)
  | VarR v -> Var v
  | ModAnnotR (u, lv) -> Mod_annot (to_simple_exn u, lv)
  | ModConstR lv -> Mod_const lv
  | MuR _ | RecvarR _ ->
    raise (Parse_error "mu/recvar not allowed in simple types")

let to_simple (t : mu_raw) : (Type_syntax.t, string) result =
  try Ok (to_simple_exn t) with Parse_error msg -> Error msg

(* Simple parser wrapper removed; use Type_menhir_driver.parse_mu and then
   to_simple/to_simple_exn from this module. *)

(* Cyclic graph representation where every level is a record with an [id]
   and a mutable [node] field. A µ-binder allocates a fresh record and the
   body refers back to that record directly, forming cycles without a
   dedicated MuLink node. *)
type cyclic = { id : int; mutable node : cnode }

and cnode =
  | CUnit
  | CPair of cyclic * cyclic
  | CSum of cyclic * cyclic
  | CCtor of string * cyclic list
  | CVar of int
  | CMod_annot of cyclic * int array
  | CMod_const of int array

let mk_id : unit -> int =
  let r = ref 0 in
  fun () -> incr r; !r

let mk (n : cnode) : cyclic = { id = mk_id (); node = n }

let to_cyclic (t : mu_raw) : cyclic =
  let rec go env t : cyclic =
    match t with
    | UnitR -> mk CUnit
    | PairR (a, b) -> mk (CPair (go env a, go env b))
    | SumR (a, b) -> mk (CSum (go env a, go env b))
    | CR (name, args) -> mk (CCtor (name, List.map (go env) args))
    | VarR v -> mk (CVar v)
    | ModAnnotR (u, lv) -> mk (CMod_annot (go env u, lv))
    | ModConstR lv -> mk (CMod_const lv)
    | RecvarR bi -> (
        match List.assoc_opt bi env with
        | Some rec_node -> rec_node
        | None -> failwith "unbound 'b index")
    | MuR (bi, body) ->
        let anchor = { id = mk_id (); node = CUnit } in
        let env' = (bi, anchor) :: env in
        let body_c = go env' body in
        anchor.node <- body_c.node;
        anchor
  in
  go [] t

(* Cyclic parsing wrapper removed; compose Type_menhir_driver.parse_mu and
   to_cyclic instead. *)
(* For parsing, use Type_menhir_driver to produce [mu_raw], then [to_cyclic]. *)

(* Pretty-print a cyclic value, cutting cycles with numbered anchors. We only
   introduce an anchor (#n=) when we actually detect a cycle (back-edge) to that
   node. Two-pass approach: first detect nodes that participate in any cycle,
   then print with anchors for those nodes only. *)

let pp_cyclic (root : cyclic) : string =
  (* Pass 1: detect nodes that participate in cycles using DFS with stack. *)
  let onstack : (cyclic, unit) Hashtbl.t = Hashtbl.create 64 in
  let visited : (cyclic, unit) Hashtbl.t = Hashtbl.create 64 in
  let cyclic_nodes : (cyclic, unit) Hashtbl.t = Hashtbl.create 64 in

  let rec dfs (n : cyclic) : unit =
    if Hashtbl.mem visited n then ()
    else (
      Hashtbl.add visited n ();
      Hashtbl.add onstack n ();
      let walk_child (c : cyclic) : unit =
        if Hashtbl.mem onstack c then Hashtbl.replace cyclic_nodes c ()
        else dfs c
      in
      (match n.node with
      | CUnit | CVar _ | CMod_const _ -> ()
      | CMod_annot (t, _) -> walk_child t
      | CPair (a, b) | CSum (a, b) -> walk_child a; walk_child b
      | CCtor (_, args) -> List.iter walk_child args);
      Hashtbl.remove onstack n)
  in
  dfs root;

  (* Pass 2: pretty-print with anchors for nodes in cycles. *)
  let defined : (cyclic, unit) Hashtbl.t = Hashtbl.create 64 in
  let pp_id_of : (cyclic, int) Hashtbl.t = Hashtbl.create 64 in
  let fresh =
    let r = ref 0 in
    fun () -> incr r; !r
  in

  let rec pp (n : cyclic) : string =
    let needs_anchor = Hashtbl.mem cyclic_nodes n in
    if needs_anchor then (
      match Hashtbl.find_opt defined n with
      | Some _ ->
        let k = Hashtbl.find pp_id_of n in
        Printf.sprintf "#%d" k
      | None ->
        Hashtbl.add defined n ();
        let k =
          match Hashtbl.find_opt pp_id_of n with
          | Some k -> k
          | None ->
            let k = fresh () in
            Hashtbl.add pp_id_of n k;
            k
        in
        Printf.sprintf "#%d=%s" k (pp_body n))
    else pp_body n
  and pp_body (n : cyclic) : string =
    match n.node with
    | CUnit -> "unit"
    | CVar v -> Printf.sprintf "'a%d" v
    | CMod_const lv -> Printf.sprintf "[%s]" (levels_to_string lv)
    | CMod_annot (t, lv) -> Printf.sprintf "%s @@ [%s]" (pp t) (levels_to_string lv)
    | CPair (a, b) -> Printf.sprintf "(%s * %s)" (pp a) (pp b)
    | CSum (a, b) -> Printf.sprintf "(%s + %s)" (pp a) (pp b)
    | CCtor (name, []) -> name
    | CCtor (name, args) ->
      let args_s = args |> List.map pp |> String.concat ", " in
      Printf.sprintf "%s(%s)" name args_s
  and levels_to_string (levels : int array) : string =
    levels |> Array.to_list |> List.map string_of_int |> String.concat ","
  in
  pp root
