module type LATTICE = Lattice_intf.LATTICE

module type ORDERED = sig
  type t

  val compare : t -> t -> int
end

module Make (C : LATTICE) (V : ORDERED) = struct
  module rec Var : sig
    type t = {
      name : V.t option;
      id : int;
      mutable bound : P.t;
      mutable eliminated : bool;
      mutable dependents : t list;
    }

    val compare : t -> t -> int
  end = struct
    type t = {
      name : V.t option;
      id : int;
      mutable bound : P.t;
      mutable eliminated : bool;
      mutable dependents : t list;
    }

    let compare a b = Int.compare a.id b.id
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

  type var = Var.t
  type lat = C.t
  type poly = P.t

  let same_var (a : Var.t) (b : Var.t) = Int.equal a.id b.id

  let add_dependent (owner : Var.t) (dep : Var.t) : unit =
    if same_var owner dep then ()
    else if List.exists (fun d -> same_var d dep) owner.dependents then ()
    else owner.dependents <- dep :: owner.dependents

  let remove_dependent (owner : Var.t) (dep : Var.t) : unit =
    owner.dependents <-
      List.filter (fun d -> not (same_var d dep)) owner.dependents

  let update_reverse_edges ~(var : Var.t) ~(old_bound : P.t) ~(new_bound : P.t)
      : unit =
    let old_deps = P.support old_bound in
    let new_deps = P.support new_bound in
    (* Remove var from no-longer dependencies' dependents *)
    P.VarSet.iter
      (fun u ->
        if (not (same_var u var)) && not (P.VarSet.mem u new_deps) then
          remove_dependent u var)
      old_deps;
    (* Add var to new dependencies' dependents *)
    P.VarSet.iter
      (fun u ->
        if (not (same_var u var)) && not (P.VarSet.mem u old_deps) then
          add_dependent u var)
      new_deps

  let set_bound_no_propagate (v : Var.t) (new_bound : P.t) : unit =
    let old_bound = v.bound in
    v.bound <- new_bound;
    update_reverse_edges ~var:v ~old_bound ~new_bound

  let replace_bound (v : Var.t) (new_bound : P.t) : unit =
    (* Set v's bound and reverse edges first *)
    set_bound_no_propagate v new_bound;
    (* Use a snapshot of dependents since updates can mutate v.dependents *)
    let dependents_snapshot = v.dependents in
    let subs = P.VarMap.(empty |> add v new_bound) in
    let update_dependent (d : Var.t) : unit =
      let d_new = P.subst ~subs d.bound in
      set_bound_no_propagate d d_new
    in
    List.iter update_dependent dependents_snapshot

  (* Queue of pending ≤ assertions to be processed lazily. *)
  let pending_asserts : (Var.t * P.t) list ref = ref []

  let has_unsolved_tmp_in (p : P.t) : bool =
    let vars = P.support p in
    P.VarSet.exists
      (fun (u : Var.t) -> match (u.name, u.eliminated) with None, false -> true | _ -> false)
      vars


  let new_var =
    let r = ref 0 in
    fun (name : V.t) ->
      let x = !r in
      r := x + 1;
      let v =
        Var.
          {
            name = Some name;
            id = x;
            bound = P.bot;
            eliminated = false;
            dependents = [];
          }
      in
      (* Initialize bound to the variable itself, not top *)
      v.bound <- P.var v;
      v
  let var (v : var) : poly = P.var v
  let const (c : lat) : poly = P.const c
  let top : poly = P.top
  let bot : poly = P.bot
  let join (a : poly) (b : poly) : poly = P.join a b
  let meet (a : poly) (b : poly) : poly = P.meet a b

  let normalize_poly (p : poly) : poly =
    let vars = P.support p in
    let subs =
      P.VarSet.fold
        (fun (v : Var.t) acc -> P.VarMap.add v v.bound acc)
        vars P.VarMap.empty
    in
    if P.VarMap.is_empty subs then p else P.subst ~subs p

  let process_pending_asserts () : unit =
    let q = !pending_asserts in
    if q <> [] then (
      pending_asserts := [];
      List.iter
        (fun ((v : Var.t), (p : P.t)) ->
          if v.eliminated then failwith "assert_leq: variable already eliminated";
          if has_unsolved_tmp_in p then
            failwith "assert_leq: contains unsolved temporary variable";
          let p' = normalize_poly p in
          let new_bound = P.meet v.bound p' in
          replace_bound v new_bound)
        (List.rev q))

  let assert_leq (v : var) (p : poly) : unit =
    if v.eliminated then failwith "assert_leq: variable already eliminated";
    pending_asserts := (v, p) :: !pending_asserts

  let solve_lfp (v : var) (p : poly) : unit =
    if v.eliminated then failwith "solve_lfp: variable already eliminated";
    (* 1) normalize the poly w.r.t. other vars *)
    let p_norm = normalize_poly p in
    (* 2) substitute var -> bot in that poly; do not meet with var *)
    let subs_v_bot = P.VarMap.(empty |> add v P.bot) in
    let candidate = P.subst ~subs:subs_v_bot p_norm in
    (* 3) check candidate ≤ current bound[v := candidate]*)
    let cand_n = normalize_poly candidate in
    let subs_v_cand = P.VarMap.(empty |> add v candidate) in
    let bound_n = normalize_poly (P.subst ~subs:subs_v_cand v.bound) in
    if not (P.leq cand_n bound_n) then
      failwith "solve_lfp: violates asserted inequalities";
    (* 4) replace bound and eliminate; propagate to dependents *)
    replace_bound v candidate;
    v.eliminated <- true

  let leq (a : poly) (b : poly) : bool =
    process_pending_asserts ();
    let a' = normalize_poly a in
    let b' = normalize_poly b in
    P.leq a' b'

  let normalize (p : poly) : (lat * V.t list) list =
    process_pending_asserts ();
    let p' = normalize_poly p in
    let terms = P.to_list p' in
    let var_names (vs : P.VarSet.t) : V.t list =
      vs |> P.VarSet.elements |> List.filter_map (fun (v : Var.t) -> v.name)
    in
    List.map (fun (vars, coeff) -> (coeff, var_names vars)) terms

  let is_eliminated (v : var) = v.eliminated
  let bound (v : var) : poly =
    process_pending_asserts (); v.bound
  let name (v : var) : V.t =
    match v.name with Some n -> n | None -> failwith "name: temporary var"

  let pp ?pp_var ?pp_coeff (p : poly) : string =
    let pp_coeff_fn = match pp_coeff with Some f -> f | None -> fun (_:C.t)->"⊤" in
    let pp_var_internal (v : Var.t) : string =
      match v.name with
      | Some n -> (match pp_var with Some f -> f n | None -> "_")
      | None -> Printf.sprintf "v%d" v.id
    in
    P.pp ~pp_var:pp_var_internal ~pp_coeff:pp_coeff_fn p

  let pp_state_line ?pp_var ?pp_coeff (v : var) : string =
    process_pending_asserts ();
    let lhs =
      match (v.name, pp_var) with
      | Some n, Some f -> f n
      | Some _, None -> Printf.sprintf "v%d" v.id
      | None, _ -> Printf.sprintf "v%d" v.id
    in
    let rel = if v.eliminated then "=" else "≤" in
    let rhs = pp ?pp_var ?pp_coeff v.bound in
    Printf.sprintf "%s %s %s" lhs rel rhs

  (* Group polynomial terms by the designated variables they mention. The key is
     the list of designated variables (sorted by [universe] order). For the
     grouped poly, we remove designated variables from each term while keeping
     non-designated variables. *)
  let decompose_by ~(universe : V.t list) (p : poly) : (V.t list * poly) list =
    process_pending_asserts ();
    (* Index map for stable ordering of keys *)
    let module VSet = Set.Make (V) in
    let idx_tbl = Hashtbl.create 16 in
    let () = List.iteri (fun i v -> Hashtbl.replace idx_tbl v i) universe in
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
        &&
        (* compare elementwise via V.compare = 0 *)
        List.for_all2 (fun x y -> V.compare x y = 0) a b
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
    let p_norm = normalize_poly p in
    let terms = P.to_list p_norm in
    let groups =
      List.fold_left
        (fun acc (vars_set, coeff) ->
          (* Split vars into designated vs others by NAME *)
          let vars_list = P.VarSet.elements vars_set in
          let designated_names, other_vars =
            List.fold_left
              (fun (d, o) (v : Var.t) ->
                match v.name with
                | Some n when VSet.mem n universe_set -> (n :: d, o)
                | _ -> (d, v :: o))
              ([], []) vars_list
          in
          let designated_sorted =
            designated_names
            |> List.sort (fun a b -> Int.compare (idx_of a) (idx_of b))
          in
          (* Build term poly: coefficient meet non-designated variables *)
          let term_poly =
            List.fold_left
              (fun acc_poly (v : Var.t) -> P.meet acc_poly (P.var v))
              (P.const coeff) other_vars
          in
          add_term acc designated_sorted term_poly)
        [] terms
    in
    groups

  let decompose_linear ~(universe : V.t list) (p : poly) :
      poly * (V.t * poly) list * (V.t list * poly) list =
    process_pending_asserts ();
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
      |> List.filter_map (fun v ->
             match Hashtbl.find_opt singles_tbl v with
             | None -> None
             | Some p -> Some (v, p))
    in
    (!base, singles, List.rev !mixed)
end
