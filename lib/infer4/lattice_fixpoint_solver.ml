module type LATTICE = Lattice_intf.LATTICE

module type ORDERED = sig
  type t

  val compare : t -> t -> int
end

module Make (C : LATTICE) (V : ORDERED) = struct
  type lat = C.t

  module rec Var : sig
    type t =
      | Rigid of V.t
      | Var of var_state

    and var_state = P.t option ref

    val compare : t -> t -> int
  end = struct
    type t =
      | Rigid of V.t
      | Var of var_state

    and var_state = P.t option ref

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
  end =
    Lattice_polynomial.Make (C) (Var)

  type var = Var.var_state
  type poly = P.t

  (* Public constructors *)
  let new_var () : var = ref None

  let var (v : var) : poly = P.var (Var.Var v)
  let rigid (n : V.t) : poly = P.var (Var.Rigid n)
  let const (c : lat) : poly = P.const c

  let top : poly = P.top
  let bot : poly = P.bot
  let join (a : poly) (b : poly) : poly = P.join a b
  let meet (a : poly) (b : poly) : poly = P.meet a b

  (* Internal: force solved solver variables inside a polynomial *)
  let force_poly (p : poly) : poly =
    let vars = P.support p in
    let subs =
      P.VarSet.fold
        (fun v acc ->
          match v with
          | Var.Rigid _ -> acc
          | Var.Var s -> (
            match !s with Some q -> P.VarMap.add v q acc | None -> acc))
        vars P.VarMap.empty
    in
    if P.VarMap.is_empty subs then p else P.subst ~subs p

  let require_no_unsolved (ctx : string) (p : poly) : unit =
    let vars = P.support p in
    P.VarSet.iter
      (function
        | Var.Rigid _ -> ()
        | Var.Var s -> (
          match !s with
          | Some _ -> ()
          | None -> failwith (ctx ^ ": contains unsolved solver variable")))
      vars

  type phase = Build | Query
  let phase : phase ref = ref Build
  let pending_gfp : (var * poly) list ref = ref []

  let solve_pending_gfps () : unit =
    if !pending_gfp <> [] then (
      let items = List.rev !pending_gfp in
      pending_gfp := [];
      List.iter
        (fun (v, rhs') ->
          (match !v with Some _ -> failwith "solve_gfps: variable already solved" | None -> ());
          let subs = P.VarMap.(empty |> add (Var.Var v) P.top) in
          let cand = P.subst ~subs rhs' in
          let cand' = force_poly cand in
          v := Some cand')
        items)

  let enter_query_phase () : unit =
    if !phase <> Query then (
      solve_pending_gfps ();
      phase := Query)

  let solve_lfp (v : var) (rhs : poly) : unit =
    (match !phase with Build -> () | _ -> failwith "solve_lfp: after query phase");
    (match !v with Some _ -> failwith "solve_lfp: variable already solved" | None -> ());
    let rhs' = force_poly rhs in
    (* substitute self by bot for LFP candidate *)
    let subs = P.VarMap.(empty |> add (Var.Var v) P.bot) in
    let cand = P.subst ~subs rhs' in
    let cand' = force_poly cand in
    v := Some cand'

  let enqueue_gfp (v : var) (rhs : poly) : unit =
    (match !phase with Build -> () | _ -> failwith "enqueue_gfp: after query phase");
    (match !v with Some _ -> failwith "enqueue_gfp: variable already solved" | None -> ());
    pending_gfp := (v, rhs) :: !pending_gfp

  (* No explicit solve_gfps: queries enter query phase and solve pending GFPs. *)

  let leq (a : poly) (b : poly) : bool =
    enter_query_phase ();
    let a' = force_poly a in
    let b' = force_poly b in
    require_no_unsolved "leq(lhs)" a';
    require_no_unsolved "leq(rhs)" b';
    P.leq a' b'

  let normalize (p : poly) : (lat * V.t list) list =
    enter_query_phase ();
    let p' = force_poly p in
    require_no_unsolved "normalize" p';
    let terms = P.to_list p' in
    let rigid_names (vs : P.VarSet.t) : V.t list =
      vs
      |> P.VarSet.elements
      |> List.filter_map (function Var.Rigid n -> Some n | Var.Var _ -> None)
    in
    List.map (fun (vars, coeff) -> (coeff, rigid_names vars)) terms

  (* Group polynomial terms by designated rigid variables. *)
  let decompose_by ~(universe : V.t list) (p : poly) : (V.t list * poly) list =
    (* Do NOT change phase here. Only force already-solved vars. *)
    let p_norm = force_poly p in
    let module VSet = Set.Make (V) in
    let idx_tbl = Hashtbl.create 16 in
    List.iteri (fun i v -> Hashtbl.replace idx_tbl v i) universe;
    let idx_of (v : V.t) : int = match Hashtbl.find_opt idx_tbl v with Some i -> i | None -> max_int in
    let universe_set = List.fold_left (fun acc v -> VSet.add v acc) VSet.empty universe in
    let add_term (acc : (V.t list * poly) list) (key : V.t list) (term_poly : poly)
        : (V.t list * poly) list =
      let keys_equal (a : V.t list) (b : V.t list) : bool =
        List.length a = List.length b && List.for_all2 (fun x y -> V.compare x y = 0) a b
      in
      let rec go xs =
        match xs with
        | [] -> [ (key, term_poly) ]
        | (k, p0) :: rest -> if keys_equal k key then (k, P.join p0 term_poly) :: rest else (k, p0) :: go rest
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
        let designated_sorted = designated_names |> List.sort (fun a b -> Int.compare (idx_of a) (idx_of b)) in
        let term_poly =
          List.fold_left (fun acc_poly v -> P.meet acc_poly (P.var v)) (P.const coeff) other_vars
        in
        add_term acc designated_sorted term_poly)
      [] terms

  let decompose_linear ~(universe : V.t list) (p : poly) :
      poly * (V.t * poly) list * (V.t list * poly) list =
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
          let prev = match Hashtbl.find_opt singles_tbl v with Some x -> x | None -> P.bot in
          Hashtbl.replace singles_tbl v (P.join prev poly)
        | _ -> mixed := (key, poly) :: !mixed)
      groups;
    let singles =
      universe
      |> List.filter_map (fun v -> match Hashtbl.find_opt singles_tbl v with None -> None | Some p -> Some (v, p))
    in
    (!base, singles, List.rev !mixed)

  let pp ?pp_var ?pp_coeff (p : poly) : string =
    let pp_var_fn = match pp_var with Some f -> f | None -> fun (_ : V.t) -> "_" in
    let pp_coeff_fn = match pp_coeff with Some f -> f | None -> fun (_ : C.t) -> "⊤" in
    let var_elems s =
      P.VarSet.elements s
      |> List.map (function Var.Rigid n -> pp_var_fn n | Var.Var _ -> "_")
    in
    let ts = P.to_list p in
    if ts = [] then "⊥"
    else
      let is_top =
        match ts with
        | [ (s, c) ] -> P.VarSet.is_empty s && C.equal c C.top
        | _ -> false
      in
      if is_top then "⊤"
      else
        let term_strings =
          ts
          |> List.map (fun (s, c) ->
                 let vars = var_elems s in
                 if C.equal c C.top then
                   let vars_str = String.concat " ⊓ " vars in
                   if List.length vars = 1 then vars_str else "(" ^ vars_str ^ ")"
                 else if P.VarSet.is_empty s then pp_coeff_fn c
                 else "(" ^ String.concat " ⊓ " (pp_coeff_fn c :: vars) ^ ")")
        in
        match term_strings with [ s ] -> s | _ -> "(" ^ String.concat " ⊔ " term_strings ^ ")"
end
