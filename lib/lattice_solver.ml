module type LATTICE = Lattice_intf.LATTICE

module type ORDERED = sig
  type t

  val compare : t -> t -> int
end

module Make (C : LATTICE) (V : ORDERED) = struct
  module rec Var : sig
    type t = {
      name : V.t;
      id : int;
      mutable bound : P.t;
      mutable eliminated : bool;
      mutable dependents : t list;
    }

    val compare : t -> t -> int
  end = struct
    type t = {
      name : V.t;
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

  let new_var =
    let r = ref 0 in
    fun (name : V.t) ->
      let x = !r in
      r := x + 1;
      let v =
        Var.{ name; id = x; bound = P.bot; eliminated = false; dependents = [] }
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

  let assert_leq (v : var) (p : poly) : unit =
    if v.eliminated then failwith "assert_leq: variable already eliminated";
    (* 1) substitute all bounds into p *)
    let p' = normalize_poly p in
    (* 2) new bound is (self ⊓ p') *)
    let new_bound = P.meet (P.var v) p' in
    (* 3) set and propagate to dependents *)
    replace_bound v new_bound

  let solve_lfp (v : var) (p : poly) : unit =
    if v.eliminated then failwith "solve_lfp: variable already eliminated";
    (* 1) normalize the poly w.r.t. other vars *)
    let p_norm = normalize_poly p in
    (* 2) substitute var -> bot in that poly; do not meet with var *)
    let subs_v_bot = P.VarMap.(empty |> add v P.bot) in
    let candidate = P.subst ~subs:subs_v_bot p_norm in
    (* 3) check candidate ≤ current bound *)
    let cand_n = normalize_poly candidate in
    let bound_n = normalize_poly v.bound in
    if not (P.leq cand_n bound_n) then
      failwith "solve_lfp: violates asserted inequalities";
    (* 4) replace bound and eliminate; propagate to dependents *)
    replace_bound v candidate;
    v.eliminated <- true

  let leq (a : poly) (b : poly) : bool =
    let a' = normalize_poly a in
    let b' = normalize_poly b in
    P.leq a' b'

  let normalize (p : poly) : (lat * V.t list) list =
    let p' = normalize_poly p in
    let terms = P.to_list p' in
    let var_names (vs : P.VarSet.t) : V.t list =
      vs |> P.VarSet.elements |> List.map (fun (v : Var.t) -> v.name)
    in
    List.map (fun (vars, coeff) -> (coeff, var_names vars)) terms

  let is_eliminated (v : var) = v.eliminated
  let bound (v : var) : poly = v.bound
  let name (v : var) : V.t = v.name

  let pp ?pp_var ?pp_coeff (p : poly) : string =
    (* Pretty-print like Modality.pp, but generic over vars and coeffs *)
    let pp_var_fn =
      match pp_var with Some f -> f | None -> fun (_ : V.t) -> "_"
    in
    let pp_coeff_fn =
      match pp_coeff with Some f -> f | None -> fun (_ : C.t) -> "⊤"
    in
    let var_elems s =
      P.VarSet.elements s |> List.map (fun (v : Var.t) -> pp_var_fn v.name)
    in
    (* Detect bottom and top via to_list and structure instead of equal *)
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
                   if List.length vars = 1 then vars_str
                   else "(" ^ vars_str ^ ")"
                 else if P.VarSet.is_empty s then pp_coeff_fn c
                 else "(" ^ String.concat " ⊓ " (pp_coeff_fn c :: vars) ^ ")")
        in
        match term_strings with
        | [ s ] -> s
        | _ -> "(" ^ String.concat " ⊔ " term_strings ^ ")"

  let pp_state_line ?pp_var ?pp_coeff (v : var) : string =
    let lhs =
      match pp_var with Some f -> f v.name | None -> Printf.sprintf "v%d" v.id
    in
    let rel = if v.eliminated then "=" else "≤" in
    let rhs = pp ?pp_var ?pp_coeff v.bound in
    Printf.sprintf "%s %s %s" lhs rel rhs
end
