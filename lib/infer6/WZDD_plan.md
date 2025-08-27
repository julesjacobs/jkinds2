Below is a **drop‑in guide + OCaml implementation** of a *lattice‑valued ZDD* (“weighted ZDD”) suitable to hand to a coding model for integration.

It implements the minimal design we converged on:

* **No `Zero` constructor**: we use a **single canonical `Leaf C.bot`** as the empty family.
* **No weights on internal nodes**: all coefficients live in terminals (`Leaf c`).
* **Canonical constructor `mk`** enforces:

  1. frontier residualization `hi ← sub_subsets(hi, lo)` (subtract what `lo` already covers),
  2. zero‑suppression via `Leaf C.bot` (if `hi = bot` then return `lo`),
  3. global hash‑consing.
* **Operations**: `join` (pointwise `C.join`), `meet` (union‑product with `C.meet` on leaves), `restrict0/1` (x←⊥/⊤), **persistent `force` cache** (cleared only when equations change).
* **Variables** hold the *closed* macro (x‑free) you “install” after solving at install time (`restrict0` = lfp, `restrict1` = gfp). No per‑node metadata, gens, or stamps.

---

## 1) How to use (quick start)

```ocaml
module C : sig
  type t
  val bot : t
  val top : t
  val join : t -> t -> t
  val meet : t -> t -> t
  val equal : t -> t -> bool
  val hash  : t -> int
  val co_sub : t -> t -> t   (* residual: join a (co_sub b a) = join a b *)
end = struct
  (* ... your lattice ... *)
end

module Z = Weighted_zdd.Make(C)

(* Make variables with a global order: lower id = higher up in the ZDD *)
let x = Z.Var.make ~id:0
let y = Z.Var.make ~id:1

(* Primitives *)
let one_x = Z.var x                (* represents set {x} with weight C.top *)
let one_y = Z.var y
let p = Z.join one_x (Z.meet one_x one_y)  (* x ⊔ (x ⊓ y) canonicalizes to x *)

(* Install macros (solve on install): lfp = x←⊥, gfp = x←⊤ *)
let env = Z.Env.create ()

(* Example: x := (y ⊓ c) ⊔ d (choose lfp or gfp policy per equation) *)
let c_leaf = Z.leaf C.top  (* substitute your own coeffs *)
let rhs = Z.join (Z.meet one_y c_leaf) c_leaf
let () = Z.Env.install env ~var:x ~rhs_raw:rhs ~fix:`Lfp    (* stores closed (x‑free) rhs; clears force cache *)

(* Force a diagram under the installed macros *)
let p_forced = Z.Env.force env p
```

---

## 2) Implementation notes you can hand to a coding model

* **Invariants** (hold for every constructed node):

  * Leaves are canonicalized with `leaf`: all `C.bot` map to the same `leaf_bot`.
  * `mk v lo hi` returns either `lo` (if `hi` cancels to bottom) or `unique (v.id, lo, hi')` where `hi' = sub_subsets hi lo`.
  * Variable order is **by `var.id`**. All APPLYs (`join`, `meet`) split on the *smaller* frontier id.
* **`sub_subsets hi lo`** implements:

  $$
  (hi \ominus_{\subseteq} lo)[S] = C.\mathrm{co\_sub}\big(hi[S], \bigvee_{T\subset S} lo[T]\big)
  $$

  It’s a memoized, ZDD‑structured recursion on the pair `(hi, lo)`; it calls `join` when it must consider “include frontier var” vs “exclude” on the `lo` side.
* **Force**: persistent memo `node → node` (weak or strong table). **Clear only on new/changed equations.** When inlining `v.rhs`, **recurse into it once** to expand nested macros.
* **No per‑node state**: all tables are outside nodes; *only* `var` carries a mutable `rhs : node option`.

---

## 3) OCaml code (single file)

> Save as `weighted_zdd.ml`. The only requirement is that your lattice module provides `equal` and `hash` for leaf interning.

```ocaml
(* weighted_zdd.ml *)
module type LATTICE = sig
  type t
  val bot    : t
  val top    : t
  val join   : t -> t -> t
  val meet   : t -> t -> t
  val equal  : t -> t -> bool
  val hash   : t -> int
  val co_sub : t -> t -> t      (* residual: join a (co_sub b a) = join a b *)
  val to_string : t -> string   (* for debugging; not used by core *)
end

module Make (C : LATTICE) = struct
  (* ---------- Variables ---------- *)
  type var = {
    id   : int;                 (* ZDD order key: smaller id = higher level *)
    mutable rhs : node option;  (* CLOSED macro (x-free) if Some, else None *)
  }

  and node =
    | Leaf of C.t
    | Node of { v : var; lo : node; hi : node }

  module Var = struct
    type t = var
    let make ~id = { id; rhs = None }
    let id v = v.id
    let set_rhs v d = v.rhs <- Some d
    let clear_rhs v = v.rhs <- None
  end

  (* ---------- Leaf interning (canonical Leaf C.bot) ---------- *)
  module LeafTbl = struct
    (* Hash-cons leaves by (C.hash, C.equal) *)
    module H = Hashtbl.Make(struct
      type t = int * C.t
      let equal (h1,c1) (h2,c2) = h1 = h2 && C.equal c1 c2
      let hash = Hashtbl.hash
    end)
    let tbl : node H.t = H.create 97
  end

  let leaf (c : C.t) : node =
    if C.equal c C.bot then
      (* Unique bottom *)
      let k = (C.hash C.bot, C.bot) in
      match LeafTbl.H.find_opt LeafTbl.tbl k with
      | Some n -> n
      | None ->
          let n = Leaf C.bot in
          LeafTbl.H.add LeafTbl.tbl k n; n
    else
      let k = (C.hash c, c) in
      match LeafTbl.H.find_opt LeafTbl.tbl k with
      | Some n -> n
      | None ->
          let n = Leaf c in
          LeafTbl.H.add LeafTbl.tbl k n; n

  let leaf_bot = leaf C.bot
  let leaf_top = leaf C.top

  let is_bot_node = function Leaf c -> C.equal c C.bot | _ -> false

  (* ---------- Unique table for internal nodes ---------- *)
  module NodeKey = struct
    type t = int * node * node
    let equal (i1,a0,a1) (i2,b0,b1) = i1 = i2 && a0 == b0 && a1 == b1
    let hash (i,a0,a1) = Hashtbl.hash (i, Oo.id a0, Oo.id a1)
  end
  module UniqueTbl = Hashtbl.Make(NodeKey)
  let unique_tbl : node UniqueTbl.t = UniqueTbl.create 4096

  let id_of_node = function
    | Leaf _ -> max_int
    | Node {v; _} -> v.id

  (* ---------- Persistent APPLY memos (environment-independent) ---------- *)
  module PairKey = struct
    type t = node * node
    let equal (a1,b1) (a2,b2) = a1 == a2 && b1 == b2
    let hash (a,b) = Hashtbl.hash (Oo.id a, Oo.id b)
  end
  module MemoJoin = Hashtbl.Make(PairKey)
  module MemoProd = Hashtbl.Make(PairKey)
  module MemoSubS = Hashtbl.Make(struct
    type t = node * node
    let equal = PairKey.equal
    let hash  = PairKey.hash
  end)

  let memo_join : node MemoJoin.t = MemoJoin.create 4096
  let memo_prod : node MemoProd.t = MemoProd.create 4096
  let memo_subs : node MemoSubS.t = MemoSubS.create 4096

  let clear_apply_memos () =
    MemoJoin.clear memo_join;
    MemoProd.clear memo_prod;
    MemoSubS.clear memo_subs

  (* ---------- down0: value at the empty set (follow 'lo' only) ---------- *)
  let rec down0 = function
    | Leaf c -> c
    | Node {lo; _} -> down0 lo

  (* ---------- Forward declarations ---------- *)
  let rec join : node -> node -> node = fun a b ->
    (* canonicalize argument order for commutativity *)
    let a', b' = if Oo.id a <= Oo.id b then a, b else b, a in
    match MemoJoin.find_opt memo_join (a', b') with
    | Some r -> r
    | None ->
      let r =
        match a, b with
        | Leaf x, Leaf y -> leaf (C.join x y)
        | Node {v=x; lo=a0; hi=a1}, Node {v=y; lo=b0; hi=b1} when x.id = y.id ->
            mk x (join a0 b0) (join a1 b1)
        | Node {v=x; lo=a0; hi=a1}, _ when x.id < id_of_node b ->
            mk x (join a0 b) (join a1 b)
        | _, Node {v=y; lo=b0; hi=b1} ->
            mk y (join a b0) (join a b1)
        | Leaf _, _ | _, Leaf _ -> assert false  (* handled above *)
      in
      MemoJoin.add memo_join (a', b') r; r

  and prod : node -> node -> node = fun a b ->
    (* commutative; canonicalize order *)
    let a', b' = if Oo.id a <= Oo.id b then a, b else b, a in
    match MemoProd.find_opt memo_prod (a', b') with
    | Some r -> r
    | None ->
      let r =
        match a, b with
        | Leaf x, Leaf y -> leaf (C.meet x y)
        | Leaf x, Node _ ->
            map_leaves_left (fun c -> C.meet x c) b
        | Node _, Leaf y ->
            map_leaves_right (fun c -> C.meet c y) a
        | Node {v=x; lo=a0; hi=a1}, Node {v=y; lo=b0; hi=b1} when x.id = y.id ->
            let lo = prod a0 b0 in
            let hi = join (prod a1 b0) (join (prod a0 b1) (prod a1 b1)) in
            mk x lo hi
        | Node {v=x; lo=a0; hi=a1}, _ when x.id < id_of_node b ->
            mk x (prod a0 b) (prod a1 b)
        | _, Node {v=y; lo=b0; hi=b1} ->
            mk y (prod a b0) (prod a b1)
      in
      MemoProd.add memo_prod (a', b') r; r

  and map_leaves_left (f : C.t -> C.t) (w : node) : node =
    match w with
    | Leaf c -> leaf (f c)
    | Node {v; lo; hi} -> mk v (map_leaves_left f lo) (map_leaves_left f hi)

  and map_leaves_right (f : C.t -> C.t) (w : node) : node =
    match w with
    | Leaf c -> leaf (f c)
    | Node {v; lo; hi} -> mk v (map_leaves_right f lo) (map_leaves_right f hi)

  (* ---------- hi ⊖subset lo (subset residualization) ---------- *)
  and sub_subsets (h : node) (l : node) : node =
    match MemoSubS.find_opt memo_subs (h, l) with
    | Some r -> r
    | None ->
      let r =
        match h, l with
        | Leaf a, _ ->
            let cover = down0 l in
            leaf (C.co_sub a cover)
        | Node {v=x; lo=h0; hi=h1}, Leaf _ ->
            mk x (sub_subsets h0 l) (sub_subsets h1 l)
        | Node {v=x; lo=h0; hi=h1}, Node {v=y; lo=l0; hi=l1} ->
            if x.id = y.id then
              let lo' = sub_subsets h0 l0 in
              let hi' = sub_subsets h1 (join l0 l1) in
              mk x lo' hi'
            else if x.id < y.id then
              mk x (sub_subsets h0 l) (sub_subsets h1 l)
            else
              let a0 = sub_subsets h l0 in
              let a1 = sub_subsets h (join l0 l1) in
              mk y a0 a1
      in
      MemoSubS.add memo_subs (h, l) r; r

  and mk (v : var) (lo : node) (hi : node) : node =
    let hi' = sub_subsets hi lo in
    match hi' with
    | Leaf c when C.equal c C.bot -> lo
    | _ ->
      (* hash-cons the node *)
      match UniqueTbl.find_opt unique_tbl (v.id, lo, hi') with
      | Some n -> n
      | None ->
          let n = Node { v; lo; hi = hi' } in
          UniqueTbl.add unique_tbl (v.id, lo, hi') n; n

  (* ---------- Convenience constructors ---------- *)
  let const (c : C.t) = leaf c
  let var (v : var) = mk v leaf_bot leaf_top

  (* ---------- Restrictions (x ← ⊥ / ⊤) ---------- *)
  let rec restrict0 (x : var) (w : node) : node =
    match w with
    | Leaf _ -> w
    | Node {v; lo; hi} ->
        if v.id = x.id then restrict0 x lo
        else mk v (restrict0 x lo) (restrict0 x hi)

  let rec restrict1 (x : var) (w : node) : node =
    match w with
    | Leaf _ -> w
    | Node {v; lo; hi} ->
        if v.id = x.id then join lo hi
        else mk v (restrict1 x lo) (restrict1 x hi)

  (* ---------- Environment (persistent force cache) ---------- *)
  module Env = struct
    module ForceTbl = struct
      module H = Hashtbl.Make(struct
        type t = node
        let equal (a:t) (b:t) = a == b
        let hash (x:t) = Oo.id x
      end)
      let create n = H.create n
    end

    type t = { force_tbl : node ForceTbl.H.t }
    let create () = { force_tbl = ForceTbl.create 4096 }
    let clear_force_cache (e:t) = Hashtbl.clear e.force_tbl

    (* Install equation: solve on install (lfp/gfp), store CLOSED macro, clear force cache. *)
    let install (e:t) ~(var:var) ~(rhs_raw:node) ~(fix:[`Lfp | `Gfp]) : unit =
      let closed =
        match fix with
        | `Lfp -> restrict0 var rhs_raw
        | `Gfp -> restrict1 var rhs_raw
      in
      var.rhs <- Some closed;
      clear_force_cache e

    (* Force: inline CLOSED macros. Recurse into macro bodies once. *)
    let rec force (e:t) (w:node) : node =
      match Hashtbl.find_opt e.force_tbl w with
      | Some r -> r
      | None ->
          let r =
            match w with
            | Leaf _ -> w
            | Node {v; lo; hi} ->
                let lo' = force e lo in
                let hi' = force e hi in
                (match v.rhs with
                 | None -> mk v lo' hi'
                 | Some d ->
                     let d' = force e d in
                     join lo' (prod hi' d'))
          in
          Hashtbl.add e.force_tbl w r; r
  end

  (* ---------- Public aliases ---------- *)
  let join = join
  let meet = prod
  let to_string (pp_var : var -> string) =
    let rec aux prefix = function
      | Leaf c ->
          if C.equal c C.bot then "⊥"
          else if C.equal c C.top then (if prefix = "" then "⊤" else prefix)
          else
            let coeff = C.to_string c in
            if prefix = "" then coeff else coeff ^ " ⊓ " ^ prefix
      | Node {v; lo; hi} ->
          let pv = let s = pp_var v in if prefix = "" then s else prefix ^ " ⊓ " ^ s in
          let s_lo = aux prefix lo in
          let s_hi = aux pv hi in
          if s_lo = "⊥" then s_hi
          else if s_hi = "⊥" then s_lo
          else s_lo ^ " ⊔ " ^ s_hi
    in
    aux ""
end
```

---

## 4) Design choices (why this works)

* **No `Zero`**: `Leaf C.bot` is the unique empty diagram; zero‑suppression checks compare against that leaf.
* **No weights inside nodes**: all coefficients are in leaves. The local **residualization at `mk`** (via `sub_subsets`) guarantees the “hat” property at every frontier, so `join`/`meet` never need a global canonicalization pass.
* **Correctness of algebra**:

  * `join` is pointwise `C.join` (APPLY).
  * `meet` is the **union‑product** (your polynomial meet), with duplicates joined by `C.join`. Both end with `mk`.
  * `restrict0/1` implement exact **lfp/gfp** for single‑var recursion (and, applied to each equation on install, give you the SCC “matrix‑star” semantics).
* **Force**: output‑sensitive with a persistent `node→node` memo; clear it only when macros change. We also recurse into macro bodies once so nested macros expand.

---

## 5) What to customize / extend

* **Leaf interning**: if `C.t` is large, ensure `C.hash` is good. You can switch the leaf cache to a **weak table** if long‑running.
* **Unique table**: likewise can be converted to weak (Ephemeron) to bound memory by live nodes.
* **Extra APPLYs**: keep `MemoJoin`, `MemoProd`, `memo_subs` persistent; they’re env‑independent.
* **Bitset fast‑path** (optional): add `mask : int64/Int128` to `var` and an `env_fp` in `Env` for O(1) “unaffected” tests before recursing in `force`.

---

If you want this split into `.mli`/`.ml` files, tests, or a CUDD‑backed variant, say the word and I’ll provide that too.
