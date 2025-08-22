open Type_syntax

let make_var (v : int) : t = Var v
let make_c (name : string) (args : t list) : t = C (name, args)

(* Grammar (whitespace ignored): Type := Constr | Var Constr := IDENT [ '(' Type
   (',' Type)* ')' ] Var := 'a' INT (* integers; 'a-1' means a0, reserved
   special *) We also allow bare INT as var for convenience; 'a' prefix is
   optional. *)

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
   Type_menhir_driver.parse_mu / parse_mu_exn instead. *)

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

(* Simple parser wrapper removed; use Type_menhir_driver.parse_mu[_exn] and then
   to_simple[_exn] from this module. *)

(* Cyclic graph representation where only MuLink carries mutability. All other
   nodes are pure. A Mu binder produces a [MuLink r] whose ref ultimately points
   to the binder body, and recursive occurrences refer to the same link. *)
type cyclic =
  | CUnit
  | CPair of cyclic * cyclic
  | CSum of cyclic * cyclic
  | CCtor of string * cyclic list
  | CVar of int
  | CMod_annot of cyclic * int array
  | CMod_const of int array
  | MuLink of cyclic ref

let to_cyclic (t : mu_raw) : cyclic =
  let rec go env t =
    match t with
    | UnitR -> CUnit
    | PairR (a, b) -> CPair (go env a, go env b)
    | SumR (a, b) -> CSum (go env a, go env b)
    | CR (name, args) -> CCtor (name, List.map (go env) args)
    | VarR v -> CVar v
    | ModAnnotR (u, lv) -> CMod_annot (go env u, lv)
    | ModConstR lv -> CMod_const lv
    | RecvarR bi -> (
      match List.assoc_opt bi env with
      | Some r -> MuLink r
      | None -> failwith "unbound 'b index")
    | MuR (bi, body) ->
      let hole : cyclic ref = ref CUnit in
      let env' = (bi, hole) :: env in
      let body_c = go env' body in
      hole := body_c;
      (* Return the link itself so top-level gets an anchor *)
      MuLink hole
  in
  go [] t

(* Cyclic parsing wrapper removed; compose Type_menhir_driver.parse_mu and
   to_cyclic instead. *)

(* Pretty-print a cyclic value, cutting cycles with numbered anchors. We only
   introduce an anchor (#n=) when we actually detect a cycle (back-edge) to that
   node. Two-pass approach: first detect nodes that participate in any cycle,
   then print with anchors for those nodes only. *)

let pp_cyclic (root : cyclic) : string =
  (* Pass 1: detect link targets that participate in cycles. *)
  let onstack_ref : (cyclic ref, unit) Hashtbl.t = Hashtbl.create 64 in
  let visited_ref : (cyclic ref, unit) Hashtbl.t = Hashtbl.create 64 in
  let cyclic_links : (cyclic ref, unit) Hashtbl.t = Hashtbl.create 64 in

  let rec dfs_node (n : cyclic) : unit =
    match n with
    | CUnit | CVar _ | CMod_const _ -> ()
    | CMod_annot (t, _) -> dfs_node t
    | CPair (a, b) | CSum (a, b) ->
      dfs_node a;
      dfs_node b
    | CCtor (_, args) -> List.iter dfs_node args
    | MuLink r -> dfs_ref r
  and dfs_ref (r : cyclic ref) : unit =
    if Hashtbl.mem visited_ref r then ()
    else (
      Hashtbl.add visited_ref r ();
      Hashtbl.add onstack_ref r ();
      (* Explore the target; encountering the same ref onstack marks a cycle *)
      let rec explore (n : cyclic) : unit =
        match n with
        | MuLink r' ->
          if Hashtbl.mem onstack_ref r' then Hashtbl.replace cyclic_links r' ()
          else dfs_ref r'
        | CUnit | CVar _ | CMod_const _ -> ()
        | CMod_annot (t, _) -> explore t
        | CPair (a, b) | CSum (a, b) ->
          explore a;
          explore b
        | CCtor (_, args) -> List.iter explore args
      in
      explore !r;
      Hashtbl.remove onstack_ref r)
  in
  dfs_node root;

  (* Pass 2: print with anchors for cyclic links only. *)
  let id_of : (cyclic ref, int) Hashtbl.t = Hashtbl.create 64 in
  let defined : (cyclic ref, unit) Hashtbl.t = Hashtbl.create 64 in

  let fresh_id =
    let r = ref 0 in
    fun () ->
      incr r;
      !r
  in

  let rec pp (n : cyclic) : string =
    match n with
    | MuLink r -> pp_link r
    | CUnit -> "unit"
    | CVar v -> Printf.sprintf "'a%d" v
    | CMod_const lv -> Printf.sprintf "[%s]" (levels_to_string lv)
    | CMod_annot (t, lv) ->
      Printf.sprintf "%s @@ [%s]" (pp t) (levels_to_string lv)
    | CPair (a, b) -> Printf.sprintf "(%s * %s)" (pp a) (pp b)
    | CSum (a, b) -> Printf.sprintf "(%s + %s)" (pp a) (pp b)
    | CCtor (name, []) -> name
    | CCtor (name, args) ->
      let args_s = args |> List.map pp |> String.concat ", " in
      Printf.sprintf "%s(%s)" name args_s
  and levels_to_string (levels : int array) : string =
    levels |> Array.to_list |> List.map string_of_int |> String.concat ","
  and pp_link (r : cyclic ref) : string =
    let needs_anchor = Hashtbl.mem cyclic_links r in
    if needs_anchor then (
      let id =
        match Hashtbl.find_opt id_of r with
        | Some k -> k
        | None ->
          let k = fresh_id () in
          Hashtbl.add id_of r k;
          k
      in
      match Hashtbl.find_opt defined r with
      | Some _ -> Printf.sprintf "#%d" id
      | None ->
        Hashtbl.add defined r ();
        Printf.sprintf "#%d=%s" id (pp !r))
    else pp !r
  in
  pp root
