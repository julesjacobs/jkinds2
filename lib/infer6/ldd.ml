(* Lattice-valued ZDDs *)

module type LATTICE = sig
  type t

  val bot : t
  val top : t
  val join : t -> t -> t
  val meet : t -> t -> t
  val co_sub : t -> t -> t (* residual: join a (co_sub b a) = join a b *)
  val to_string : t -> string (* optional, for debug/printing *)
  val equal : t -> t -> bool
  val hash : t -> int
end

module Make (C : LATTICE) = struct
  (* --------- variables --------- *)
  type node =
    | Leaf of { id : int; c : C.t }
    | Node of { id : int; v : var; lo : node; hi : node }

  and var = {
    id : int; (* ZDD order: smaller id = higher *)
    mutable state : var_state; (* type+state of the variable *)
  }

  and var_state = Unsolved | Solved of node | Rigid of string

  module Var = struct
    type t = var

    let var_id = ref (-1)

    let make state =
      incr var_id;
      { id = !var_id; state }

    let make_var () = make Unsolved
    let make_rigid ~name () = make (Rigid name)
    let id v = v.id
  end

  (* --------- stable node ids --------- *)
  let next_node_id = ref (-1)

  let fresh_id () =
    incr next_node_id;
    !next_node_id

  let node_id = function Leaf n -> n.id | Node n -> n.id

  (* --------- leaf interning (ensures one pointer per equal coeff) --------- *)
  module LeafTbl = Hashtbl.Make (C)

  let leaf_tbl : node LeafTbl.t = LeafTbl.create 97

  let leaf (c : C.t) : node =
    match LeafTbl.find_opt leaf_tbl c with
    | Some n -> n
    | None ->
      let n = Leaf { id = fresh_id (); c } in
      LeafTbl.add leaf_tbl c n;
      n

  let leaf_bot = leaf C.bot
  let leaf_top = leaf C.top
  let is_bot_node n = n == leaf_bot

  (* --------- unique table for internal nodes --------- *)
  module UKey = struct
    type t = int * int * int (* v.id, lo.id, hi.id *)

    let equal (a, b, c) (a', b', c') = a = a' && b = b' && c = c'
    let hash = Hashtbl.hash
  end

  module Unique = Hashtbl.Make (UKey)

  let uniq_tbl : node Unique.t = Unique.create 1024

  (* --------- persistent memos --------- *)
  module NodeTbl = struct
    module Tbl = Hashtbl.Make (struct
      type t = int

      let equal = ( = )
      let hash = Hashtbl.hash
    end)

    let create () = Tbl.create 1024
    let find_opt tbl n = Tbl.find_opt tbl (node_id n)
    let add tbl n r = Tbl.add tbl (node_id n) r
    let clear tbl = Tbl.clear tbl
  end

  module NodePairTbl = struct
    module Tbl = Hashtbl.Make (struct
      type t = int * int

      let equal (a, b) (c, d) = a = c && b = d
      let hash = Hashtbl.hash
    end)

    let create () = Tbl.create 1024
    let find_opt tbl n m = Tbl.find_opt tbl (node_id n, node_id m)
    let add tbl n m r = Tbl.add tbl (node_id n, node_id m) r
    let clear tbl = Tbl.clear tbl
  end

  (* For asserting that var ids are strictly increasing down the tree. *)
  let var_index (n : node) : int =
    match n with Leaf _ -> 999999999 | Node n -> n.v.id

  (* Construct a node and hash-cons it. Must be in canonical form: hi = hi -
     low. *)
  let node_raw (v : var) (lo : node) (hi : node) : node =
    assert (v.id < var_index lo);
    assert (v.id < var_index hi);

    if is_bot_node hi then lo
    else
      let key = (v.id, node_id lo, node_id hi) in
      match Unique.find_opt uniq_tbl key with
      | Some n -> n
      | None ->
        let n = Node { id = fresh_id (); v; lo; hi } in
        Unique.add uniq_tbl key n;
        n

  (* Subtract subsets h - l *)
  let memo_subs = NodePairTbl.create ()

  let rec sub_subsets (h : node) (l : node) : node =
    (* Value at empty set *)
    let rec down0 = function
      | Leaf { c; _ } -> c
      | Node { lo; _ } -> down0 lo
    in
    match NodePairTbl.find_opt memo_subs h l with
    | Some r -> r
    | None ->
      let r =
        match (h, l) with
        | Leaf x, _ -> leaf (C.co_sub x.c (down0 l))
        | Node nh, Leaf _ ->
          node_raw nh.v (sub_subsets nh.lo l) (sub_subsets nh.hi l)
        | Node nh, Node nl ->
          if nh.v.id = nl.v.id then
            let lo' = sub_subsets nh.lo nl.lo in
            let hi' = sub_subsets (sub_subsets nh.hi nl.lo) nl.hi in
            node_raw nh.v lo' hi'
          else if nh.v.id < nl.v.id then
            node_raw nh.v (sub_subsets nh.lo l) (sub_subsets nh.hi l)
          else (* h.id > l.id *)
            sub_subsets h nl.lo
      in
      NodePairTbl.add memo_subs h l r;
      r

  let rec node (v : var) (lo : node) (hi : node) : node =
    (* Don't need to memo this because sub_subsets and node_raw are memoized *)
    let hi' = sub_subsets hi lo in
    node_raw v lo hi'

  and memo_join = NodePairTbl.create ()

  and join (a : node) (b : node) =
    match NodePairTbl.find_opt memo_join a b with
    | Some r -> r
    | None ->
      let r =
        match (a, b) with
        | Leaf x, Leaf y -> leaf (C.join x.c y.c)
        | Node na, Node nb ->
          if na.v.id = nb.v.id then
            (* node na.v (join na.lo nb.lo) (join na.hi nb.hi) *)
            node_raw na.v (join na.lo nb.lo)
              (join (sub_subsets na.hi nb.lo) (sub_subsets nb.hi na.lo))
          else if na.v.id < nb.v.id then
            (* node na.v (join na.lo b) (join na.hi b) *)
            node_raw na.v (join na.lo b) (sub_subsets na.hi b)
          else
            (* node nb.v (join a nb.lo) (join a nb.hi) *)
            node_raw nb.v (join a nb.lo) (sub_subsets nb.hi a)
        | Leaf _, Node nb ->
          (* node nb.v (join a nb.lo) (join a nb.hi) *)
          node_raw nb.v (join a nb.lo) (sub_subsets nb.hi a)
        | Node na, Leaf _ ->
          (* node na.v (join na.lo b) (join na.hi b) *)
          node_raw na.v (join na.lo b) (sub_subsets na.hi b)
      in
      NodePairTbl.add memo_join a b r;
      r

  let memo_meet = NodePairTbl.create ()

  let rec meet (a : node) (b : node) =
    match NodePairTbl.find_opt memo_meet a b with
    | Some r -> r
    | None ->
      let r =
        match (a, b) with
        | Leaf x, Leaf y -> leaf (C.meet x.c y.c)
        | Leaf _, Node nb ->
          node nb.v (meet a nb.lo) (meet a nb.hi)
          (* maps meet x across leaves *)
        | Node na, Leaf _ -> node na.v (meet na.lo b) (meet na.hi b)
        | Node na, Node nb ->
          if na.v.id = nb.v.id then
            let lo = meet na.lo nb.lo in
            let hi =
              join (meet na.hi nb.lo)
                (join (meet na.lo nb.hi) (meet na.hi nb.hi))
            in
            node na.v lo hi
          else if na.v.id < nb.v.id then node na.v (meet na.lo b) (meet na.hi b)
          else node nb.v (meet a nb.lo) (meet a nb.hi)
      in
      NodePairTbl.add memo_meet a b r;
      r

  (* --------- public constructors --------- *)
  let const (c : C.t) = leaf c
  let var (v : var) = node v leaf_bot leaf_top
  let rigid (name : string) = var (Var.make_rigid ~name ())
  let new_var () = Var.make_var ()

  (* --------- restrictions (x ← ⊥ / ⊤) --------- *)
  (* TODO: memoize *)
  let rec restrict0 (x : var) (w : node) : node =
    match w with
    | Leaf _ -> w
    | Node n ->
      if n.v.id = x.id then restrict0 x n.lo
      else node n.v (restrict0 x n.lo) (restrict0 x n.hi)

  (* TODO: memoize *)
  let rec restrict1 (x : var) (w : node) : node =
    match w with
    | Leaf _ -> w
    | Node n ->
      if n.v.id = x.id then join n.lo n.hi
      else node n.v (restrict1 x n.lo) (restrict1 x n.hi)

  (* --------- force (per-call memo; no env object) --------- *)
  let memo_force = NodeTbl.create ()

  let rec force (w : node) : node =
    match NodeTbl.find_opt memo_force w with
    | Some r -> r
    | None ->
      let r =
        match w with
        | Leaf _ -> w
        | Node n -> (
          let lo' = force n.lo
          and hi' = force n.hi in
          match n.v.state with
          | Rigid _ -> node n.v lo' hi'
          | Solved d ->
            let d' = force d in
            join lo' (meet hi' d')
          | Unsolved -> node n.v lo' hi')
      in
      NodeTbl.add memo_force w r;
      r

  (* --------- solve-on-install (per-call; no env) --------- *)
  let solve_lfp ~(var : var) ~(rhs_raw : node) : unit =
    match var.state with
    | Rigid _ -> invalid_arg "solve_lfp: rigid variable"
    | Solved _ -> invalid_arg "solve_lfp: solved variable"
    | Unsolved ->
      let rhs_forced = force rhs_raw in
      var.state <- Solved (restrict0 var rhs_forced);
      NodeTbl.clear memo_force

  let solve_gfp ~(var : var) ~(rhs_raw : node) : unit =
    match var.state with
    | Rigid _ -> invalid_arg "solve_gfp: rigid variable"
    | Solved _ -> invalid_arg "solve_gfp: solved variable"
    | Unsolved ->
      let rhs_forced = force rhs_raw in
      var.state <- Solved (restrict1 var rhs_forced);
      NodeTbl.clear memo_force

  (* --------- optional printer --------- *)
  let to_string (pp_var : var -> string) =
    let rec aux pref = function
      | Leaf { c; _ } ->
        if C.equal c C.bot then "⊥"
        else if C.equal c C.top then if pref = "" then "⊤" else pref
        else if pref = "" then C.to_string c
        else C.to_string c ^ " ⊓ " ^ pref
      | Node n ->
        let p =
          let s = pp_var n.v in
          if pref = "" then s else pref ^ " ⊓ " ^ s
        in
        let a = aux pref n.lo
        and b = aux p n.hi in
        if a = "⊥" then b else if b = "⊥" then a else a ^ " ⊔ " ^ b
    in
    aux ""

  (* --------- enumeration to list --------- *)
  (* Turns an LDD into a list of (coeff * rigid_vars).
     - coeff is a C.t from the leaf
     - rigid_vars is the list of names for variables encountered on hi-edges
       that are marked Rigid in their state, in traversal order.
       Rigid variables are guaranteed unique per path, so no dedup is needed.
     - entries with coeff = bot are omitted. *)

  let to_list (w : node) : (C.t * string list) list =
    let rec aux (acc : string list) (w : node) : (C.t * string list) list =
      match w with
      | Leaf { c; _ } -> if C.equal c C.bot then [] else [ (c, acc) ]
      | Node n ->
        let acc_hi =
          match n.v.state with
          | Rigid name -> name :: acc
          | _ -> failwith "to_list: non-rigid variable"
        in
        let lo_list = aux acc n.lo in
        let hi_list = aux acc_hi n.hi in
        lo_list @ hi_list
    in
    aux [] w

  (* --------- polynomial-style pretty printer --------- *)
  (* Prints using the same conventions as lattice_polynomial.pp:
     - Deterministic term ordering (sort by rendered body string)
     - Parentheses around meets when there are multiple terms
     - Constants: ⊥ when empty; ⊤ when constant-top. *)
  let pp_as_polynomial ?(pp_coeff = C.to_string) (w : node) : string =
    (* Aggregate duplicate rigid var-sets by join on coefficients. *)
    let tbl : (string list, C.t) Hashtbl.t = Hashtbl.create 16 in
    let add_entry (c, names) =
      let vs = List.sort String.compare names in
      match Hashtbl.find_opt tbl vs with
      | None -> Hashtbl.add tbl vs c
      | Some prev -> Hashtbl.replace tbl vs (C.join prev c)
    in
    List.iter add_entry (to_list w);
    let terms =
      Hashtbl.fold
        (fun vs c acc -> if C.equal c C.bot then acc else (vs, c) :: acc)
        tbl []
    in
    if terms = [] then "⊥"
    else
      let term_body vs c =
        let is_top = C.equal c C.top in
        match (vs, is_top) with
        | [], true -> ("⊤", false)
        | [], false -> (pp_coeff c, false)
        | _ :: _, true -> (String.concat " ⊓ " vs, List.length vs > 1)
        | _ :: _, false -> (pp_coeff c ^ " ⊓ " ^ String.concat " ⊓ " vs, true)
      in
      let items =
        terms
        |> List.map (fun (vs, c) ->
               let body, has_meet = term_body vs c in
               (body, has_meet))
        |> List.sort (fun (a, _) (b, _) -> String.compare a b)
      in
      let n_terms = List.length items in
      items
      |> List.map (fun (body, has_meet) ->
             if n_terms > 1 && has_meet then "(" ^ body ^ ")" else body)
      |> String.concat " ⊔ "

  (* --------- invariants (for tests/debug) --------- *)
  (* Check that along every path, variable ids are strictly increasing
     (smaller id = higher node). Returns true if order is respected. *)
  let check_var_order (w : node) : bool =
    let rec aux last_id = function
      | Leaf _ -> true
      | Node n ->
        let ok_here = last_id < n.v.id in
        ok_here && aux n.v.id n.lo && aux n.v.id n.hi
    in
    aux (-1) w

  (* --------- structural debug printer --------- *)
  (* Prints full DAG structure with node ids, var ids/states, and shared-node
     references. Useful to diagnose memoization/path effects. *)
  let pp_debug ?(pp_coeff = C.to_string) (w : node) : string =
    let b = Buffer.create 1024 in
    let seen = Hashtbl.create 97 in
    let pp_var_info (v : var) : string =
      let state_s =
        match v.state with
        | Unsolved -> "Unsolved"
        | Rigid name -> "Rigid(" ^ name ^ ")"
        | Solved n -> "Solved(#" ^ string_of_int (node_id n) ^ ")"
      in
      Printf.sprintf "v#%d:%s" v.id state_s
    in
    let rec go indent (n : node) : unit =
      let id = node_id n in
      if Hashtbl.mem seen id then
        Buffer.add_string b (Printf.sprintf "%s#%d = <ref>\n" indent id)
      else (
        Hashtbl.add seen id true;
        match n with
        | Leaf { id; c } ->
          Buffer.add_string b
            (Printf.sprintf "%sLeaf#%d c=%s\n" indent id (pp_coeff c))
        | Node { id; v; lo; hi } ->
          Buffer.add_string b
            (Printf.sprintf "%sNode#%d %s lo=#%d hi=#%d\n" indent id
               (pp_var_info v) (node_id lo) (node_id hi));
          let indent' = indent ^ "  " in
          go indent' lo;
          go indent' hi)
    in
    go "" w;
    Buffer.contents b
end
