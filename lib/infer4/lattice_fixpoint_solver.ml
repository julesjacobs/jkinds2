module type LATTICE = Lattice_intf.LATTICE

module type ORDERED = sig
  type t

  val compare : t -> t -> int
  val to_string : t -> string
end

module Make (C : LATTICE) (V : ORDERED) = struct
  type lat = C.t

  module rec Var : sig
    type t = Rigid of V.t | Var of var_state
    and var_state = { id : int; mutable sol : P.t option }

    val compare : t -> t -> int
  end = struct
    type t = Rigid of V.t | Var of var_state
    and var_state = { id : int; mutable sol : P.t option }

    let tag = function Rigid _ -> 0 | Var _ -> 1

    let compare (a : t) (b : t) : int =
      match (a, b) with
      | Rigid a, Rigid b -> V.compare a b
      | Var a, Var b -> Stdlib.compare a b
      | _ -> Int.compare (tag a) (tag b)
  end

  and P : sig
    module VarSet : Set.S with type elt = Var.t
    module VarMap : Map.S with type key = Var.t

    type t

    val const : C.t -> t
    val var : Var.t -> t
    val to_list : t -> (VarSet.t * C.t) list
    val top : t
    val bot : t
    val join : t -> t -> t
    val meet : t -> t -> t
    val leq : t -> t -> bool
    val support : t -> VarSet.t
    val subst : subs:t VarMap.t -> t -> t

    val pp :
      ?pp_var:(Var.t -> string) -> ?pp_coeff:(C.t -> string) -> t -> string
  end =
    Lattice_polynomial.Make (C) (Var)

  type var = Var.var_state
  type poly = P.t

  (* tracing support *)
  let trace_enabled =
    match Sys.getenv_opt "JKINDS_TRACE" with
    | Some s ->
      let s = String.lowercase_ascii (String.trim s) in
      not (s = "" || s = "0" || s = "false" || s = "no")
    | None -> false

  let log fmt =
    if trace_enabled then Printf.eprintf (fmt ^^ "\n")
    else Printf.ifprintf stderr fmt

  let next_id = ref 0

  (* logging pretty-printer hooks *)
  let log_pp_var : (V.t -> string) option ref = ref None
  let log_pp_coeff : (C.t -> string) option ref = ref None

  let set_log_printers ?pp_var ?pp_coeff () : unit =
    (match pp_var with Some f -> log_pp_var := Some f | None -> ());
    match pp_coeff with Some f -> log_pp_coeff := Some f | None -> ()

  let pp_log (p : poly) : string =
    let pp_var_internal = function
      | Var.Rigid n -> (
        match !log_pp_var with Some f -> f n | None -> V.to_string n)
      | Var.Var s -> Printf.sprintf "v#%d" s.id
    in
    let pp_coeff_internal =
      match !log_pp_coeff with Some f -> f | None -> C.to_string
    in
    P.pp ~pp_var:pp_var_internal ~pp_coeff:pp_coeff_internal p

  (* Public constructors *)
  let new_var () : var =
    let id = !next_id in
    incr next_id;
    let v = { Var.id; Var.sol = None } in
    log "[fix] new_var v#%d" id;
    v

  let var (v : var) : poly = P.var (Var.Var v)
  let rigid (n : V.t) : poly = P.var (Var.Rigid n)
  let const (c : lat) : poly = P.const c
  let top : poly = P.top
  let bot : poly = P.bot
  let join (a : poly) (b : poly) : poly = P.join a b
  let meet (a : poly) (b : poly) : poly = P.meet a b

  (* Internal: force solved solver variables inside a polynomial *)
  let rec force_poly (p : poly) : poly =
    let vars = P.support p in
    let subs =
      P.VarSet.fold
        (fun v acc ->
          match v with
          | Var.Rigid _ -> acc
          | Var.Var s -> (
            match s.Var.sol with
            | Some q ->
              let q' = force_poly q in
              s.Var.sol <- Some q';
              P.VarMap.add v q' acc
            | None -> acc))
        vars P.VarMap.empty
    in
    if P.VarMap.is_empty subs then p else P.subst ~subs p

  let require_no_unsolved (ctx : string) (p : poly) : unit =
    let vars = P.support p in
    P.VarSet.iter
      (function
        | Var.Rigid _ -> ()
        | Var.Var s -> (
          match s.Var.sol with
          | Some _ -> ()
          | None -> failwith (ctx ^ ": contains unsolved solver variable")))
      vars

  let pending_gfp : (var * poly) list ref = ref []

  let solve_fp (v : var) (rhs : poly) (self_value : poly) : unit =
    let rhs' = force_poly rhs in
    let subs = P.VarMap.(empty |> add (Var.Var v) self_value) in
    let cand = P.subst ~subs rhs' in
    v.Var.sol <- Some cand;
    log "[fix] lfp(v%d, %s, %s) = %s" v.Var.id (pp_log rhs) (pp_log self_value)
      (pp_log cand)

  let solve_pending_gfps () : unit =
    if !pending_gfp <> [] then (
      log "[fix] solving %d pending GFPs" (List.length !pending_gfp);
      let items = List.rev !pending_gfp in
      pending_gfp := [];
      List.iter
        (fun (v, rhs) ->
          (match v.Var.sol with
          | Some _ -> failwith "solve_gfps: variable already solved"
          | None -> ());
          solve_fp v rhs P.top)
        items)

  let enter_query_phase () : unit =
    (* No phase: simply solve any pending GFPs now. *)
    log "[fix] enter_query_phase";
    solve_pending_gfps ()

  let solve_lfp (v : var) (rhs : poly) : unit =
    (match v.Var.sol with
    | Some _ -> failwith "solve_lfp: variable already solved"
    | None -> ());
    solve_fp v rhs P.bot

  let enqueue_gfp (v : var) (rhs : poly) : unit =
    (match v.Var.sol with
    | Some _ -> failwith "enqueue_gfp: variable already solved"
    | None -> ());
    pending_gfp := (v, rhs) :: !pending_gfp;
    log "[fix] enqueue_gfp(v%d, %s)" v.Var.id (pp_log rhs)

  (* No explicit solve_gfps: queries enter query phase and solve pending
     GFPs. *)

  let leq (a : poly) (b : poly) : bool =
    log "[fix] leq(%s, %s)" (pp_log a) (pp_log b);
    enter_query_phase ();
    let a' = force_poly a in
    let b' = force_poly b in
    require_no_unsolved "leq(lhs)" a';
    require_no_unsolved "leq(rhs)" b';
    P.leq a' b'

  let normalize (p : poly) : (lat * V.t list) list =
    log "[fix] normalize(%s)" (pp_log p);
    enter_query_phase ();
    let p' = force_poly p in
    require_no_unsolved "normalize" p';
    let terms = P.to_list p' in
    let rigid_names (vs : P.VarSet.t) : V.t list =
      vs
      |> P.VarSet.elements
      |> List.filter_map (function
           | Var.Rigid n -> Some n
           | Var.Var _ -> failwith "normalize: unsolved variable")
    in
    List.map (fun (vars, coeff) -> (coeff, rigid_names vars)) terms

  module RigidPoly = Lattice_polynomial.Make (C) (V)

  let normalize_poly (p : poly) : RigidPoly.t =
    (* Ensure all solver variables are solved and eliminated, as in
       [normalize]. *)
    enter_query_phase ();
    let p' = force_poly p in
    require_no_unsolved "normalize_poly" p';
    let terms = P.to_list p' in
    let to_rigid_vars (vs : P.VarSet.t) : RigidPoly.VarSet.t =
      P.VarSet.fold
        (fun v acc ->
          match v with
          | Var.Rigid n -> RigidPoly.VarSet.add n acc
          | Var.Var _ -> failwith "normalize_poly: unsolved variable")
        vs RigidPoly.VarSet.empty
    in
    let rl = List.map (fun (s, c) -> (to_rigid_vars s, c)) terms in
    RigidPoly.of_list rl

  (* Group polynomial terms by designated rigid variables. *)
  let decompose_by ~(universe : V.t list) (p : poly) : (V.t list * poly) list =
    log "[fix] decompose_by(|U|=%d, p=%s)" (List.length universe) (pp_log p);
    (* Do NOT change phase here. Only force already-solved vars. *)
    let p_norm = force_poly p in
    let module VSet = Set.Make (V) in
    let idx_tbl = Hashtbl.create 16 in
    List.iteri (fun i v -> Hashtbl.replace idx_tbl v i) universe;
    let idx_of (v : V.t) : int =
      match Hashtbl.find_opt idx_tbl v with Some i -> i | None -> max_int
    in
    let universe_set =
      List.fold_left (fun acc v -> VSet.add v acc) VSet.empty universe
    in
    let add_term (acc : (V.t list * poly) list) (key : V.t list)
        (term_poly : poly) : (V.t list * poly) list =
      let keys_equal (a : V.t list) (b : V.t list) : bool =
        List.length a = List.length b
        && List.for_all2 (fun x y -> V.compare x y = 0) a b
      in
      let rec go xs =
        match xs with
        | [] -> [ (key, term_poly) ]
        | (k, p0) :: rest ->
          if keys_equal k key then (k, P.join p0 term_poly) :: rest
          else (k, p0) :: go rest
      in
      go acc
    in
    let terms = P.to_list p_norm in
    List.fold_left
      (fun acc (vars_set, coeff) ->
        let vars_list = P.VarSet.elements vars_set in
        let designated_names, other_vars =
          List.fold_left
            (fun (d, o) v ->
              match v with
              | Var.Rigid n when VSet.mem n universe_set -> (n :: d, o)
              | _ -> (d, v :: o))
            ([], []) vars_list
        in
        let designated_sorted =
          designated_names
          |> List.sort (fun a b -> Int.compare (idx_of a) (idx_of b))
        in
        let term_poly =
          List.fold_left
            (fun acc_poly v -> P.meet acc_poly (P.var v))
            (P.const coeff) other_vars
        in
        add_term acc designated_sorted term_poly)
      [] terms

  let decompose_linear ~(universe : V.t list) (p : poly) :
      poly * (V.t * poly) list * (V.t list * poly) list =
    log "[fix] decompose_linear(|U|=%d, p=%s)" (List.length universe) (pp_log p);
    (* Do NOT change phase here. *)
    let groups = decompose_by ~universe p in
    let base = ref P.bot in
    let singles_tbl : (V.t, poly) Hashtbl.t = Hashtbl.create 16 in
    let mixed = ref [] in
    List.iter
      (fun (key, poly) ->
        match key with
        | [] -> base := P.join !base poly
        | [ v ] ->
          let prev =
            match Hashtbl.find_opt singles_tbl v with
            | Some x -> x
            | None -> P.bot
          in
          Hashtbl.replace singles_tbl v (P.join prev poly)
        | _ -> mixed := (key, poly) :: !mixed)
      groups;
    let singles =
      universe
      |> List.map (fun v ->
             let p =
               match Hashtbl.find_opt singles_tbl v with
               | None -> P.bot
               | Some p -> p
             in
             (v, p))
    in
    (!base, singles, List.rev !mixed)

  let pp ?pp_var ?pp_coeff (p : poly) : string =
    let p = force_poly p in
    let pp_coeff_fn =
      match pp_coeff with Some f -> f | None -> fun (_ : C.t) -> "⊤"
    in
    let pp_var_internal = function
      | Var.Rigid n -> (
        match pp_var with Some f -> f n | None -> V.to_string n)
      | Var.Var s -> Printf.sprintf "v%d" s.Var.id
    in
    P.pp ~pp_var:pp_var_internal ~pp_coeff:pp_coeff_fn p
end
