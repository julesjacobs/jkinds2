(* Inference core: compute a "kind" (modality environment) from type syntax. -
   Index 0 (Kind.Var.a0) is the administrative entry for the constructor. -
   Indices 1..arity are parameter positions. Composition uses meet; alternatives
   use join. *)
let kindof (t : Type_syntax.t) : Kind.t =
  let open Type_syntax in
  let rec go (t : Type_syntax.t) : Kind.t =
    match t with
    | Var a ->
      Kind.set (Kind.set Kind.empty a Modality.id) Kind.Var.a0 Modality.zero
    | Unit -> Kind.set Kind.empty Kind.Var.a0 Modality.zero
    | Pair (a, b) | Sum (a, b) -> Kind.max (go a) (go b)
    | Mod_annot (t', levels) ->
      let k = go t' in
      let m = Modality.of_levels levels in
      Kind.apply m k
    | Mod_const levels ->
      let m = Modality.of_levels levels in
      (* As a type, a bare modality constant contributes that meet to the sole
         var 1 *)
      Kind.set Kind.empty 0 m
    | C (name, args) ->
      let base =
        Kind.set Kind.empty Kind.Var.a0
          (Modality.of_atom { Modality.ctor = name; index = 0 })
      in
      let step (acc : Kind.t) (arg : Type_syntax.t) (i : int) : Kind.t =
        let mi = Modality.of_atom { ctor = name; index = i } in
        let ki = go arg in
        Kind.max acc (Kind.apply mi ki)
      in
      List.mapi (fun i t -> (i + 1, t)) args
      |> List.fold_left (fun acc (i, t) -> step acc t i) base
  in
  go t

module StrMap = Map.Make (String)

(* One-pass initial kinds for name→RHS bindings. *)
let kinds_of_decls_bindings (bs : (string * Type_syntax.t) list) :
    (string * Kind.t) list =
  List.map (fun (k, v) -> (k, kindof v)) bs

(* Substitute constructor atoms in lhs using rhs solutions; missing indices
   default to ⊥ through Kind.get. *)
let substitute_kinds_bindings ~(lhs : (string * Kind.t) list)
    ~(rhs : (string * Kind.t) list) : (string * Kind.t) list =
  let rhs_map =
    List.fold_left (fun acc (n, k) -> StrMap.add n k acc) StrMap.empty rhs
  in
  let subst_one (n, k) =
    let lookup_atom (a : Modality.atom) : Modality.t option =
      match StrMap.find_opt a.ctor rhs_map with
      | None -> None
      | Some k_rhs -> Some (Kind.get k_rhs a.index)
    in
    (n, Kind.substitute_using lookup_atom k)
  in
  List.map subst_one lhs

(* Bottom element for fixpoint iteration: zero-out all entries. *)
let zero_constructor_entries_bindings (bs : (string * Kind.t) list) :
    (string * Kind.t) list =
  List.map (fun (n, k) -> (n, Kind.zero_entries k)) bs

(* Two-phase least fixpoint: 1) iterate concretes from ⊥; 2) iterate abstracts
   with self-init and self-meet, substituting concrete solutions into RHS;
   finally, substitute abstract solutions back. *)
let least_fixpoint_bindings_with_self_init ?(max_iters = 10)
    ~(abstracts : (string * int) list) (bs : (string * Kind.t) list) :
    (string * Kind.t) list =
  let arity_by_name =
    List.fold_left (fun acc (n, a) -> StrMap.add n a acc) StrMap.empty abstracts
  in
  let self_kind name arity : Kind.t =
    let rec loop i acc =
      if i > arity then acc
      else
        loop (i + 1)
          (Kind.set acc i
             (Modality.of_atom { Modality.ctor = name; index = i }))
    in
    loop 0 Kind.empty
  in
  let lists_equal l1 l2 =
    let sort = List.sort (fun (a, _) (b, _) -> String.compare a b) in
    let l1 = sort l1
    and l2 = sort l2 in
    List.length l1 = List.length l2
    && List.for_all2
         (fun (n1, k1) (n2, k2) -> n1 = n2 && Kind.equal k1 k2)
         l1 l2
  in
  let is_abstract name = StrMap.mem name arity_by_name in
  let conc_bs, abs_bs = List.partition (fun (n, _) -> not (is_abstract n)) bs in
  (* Phase 1: iterate concretes only from ⊥ until convergence. *)
  let rec loop_conc i current =
    if i > max_iters then (
      prerr_endline
        "[warn] least_fixpoint (concrete phase): did not converge within \
         bound; returning last iterate";
      current)
    else
      let lines =
        List.map (fun (n, k) -> Printf.sprintf "  %s: %s" n (Kind.pp k)) current
        |> String.concat "\n"
      in
      Printf.printf "[lfp] iter %d:\n%s\n" i lines;
      let next = substitute_kinds_bindings ~lhs:conc_bs ~rhs:current in
      if lists_equal next current then current else loop_conc (i + 1) next
  in
  let start_conc = zero_constructor_entries_bindings conc_bs in
  let conc_sol = loop_conc 0 start_conc in
  (* Phase 2: iterate abstracts, substituting concrete solutions in RHS, with
     self init and meet with self each iteration. *)
  let rec loop_abs i current =
    if i > max_iters then (
      prerr_endline
        "[warn] least_fixpoint (abstract phase): did not converge within \
         bound; returning last iterate";
      current)
    else
      let lines =
        List.map (fun (n, k) -> Printf.sprintf "  %s: %s" n (Kind.pp k)) current
        |> String.concat "\n"
      in
      Printf.printf "[lfp] iter %d:\n%s\n" i lines;
      let rhs = conc_sol @ current in
      let next0 = substitute_kinds_bindings ~lhs:abs_bs ~rhs in
      let next =
        List.map
          (fun (n, k) ->
            match StrMap.find_opt n arity_by_name with
            | Some arity ->
              let rec meet_self i acc =
                if i > arity then acc
                else
                  let mi = Modality.of_atom { Modality.ctor = n; index = i } in
                  let cur = Kind.get acc i in
                  let m' = Modality.compose mi cur in
                  meet_self (i + 1) (Kind.set acc i m')
              in
              (n, meet_self 0 k)
            | None -> (n, k))
          next0
      in
      if lists_equal next current then current else loop_abs (i + 1) next
  in
  let start_abs =
    List.map
      (fun (n, _) ->
        let arity = StrMap.find n arity_by_name in
        (n, self_kind n arity))
      abs_bs
  in
  let abs_sol = loop_abs 0 start_abs in
  (* Phase 3: substitute abstract solutions back into concrete ones and return
     combined. *)
  let conc_final =
    substitute_kinds_bindings ~lhs:conc_bs ~rhs:(abs_sol @ conc_sol)
  in
  (* Reassemble in original order *)
  List.map
    (fun (n, _) ->
      match List.assoc_opt n conc_final with
      | Some k -> (n, k)
      | None -> (n, List.assoc n abs_sol))
    bs

(* CLI driver: print initial kinds then run the fixpoint. *)
let solve_program (prog : Decl_parser.program) ~(max_iters : int) :
    (string * Kind.t) list =
  let bindings =
    List.map (fun (it : Decl_parser.decl_item) -> (it.name, it.rhs)) prog
  in
  let abstracts =
    List.filter_map
      (fun (it : Decl_parser.decl_item) ->
        if it.abstract then Some (it.name, it.arity) else None)
      prog
  in
  let kinds = kinds_of_decls_bindings bindings in
  print_endline "Kinds:";
  List.iter (fun (n, k) -> Printf.printf "%s: %s\n" n (Kind.pp k)) kinds;
  print_endline "\nLeast fixpoint kinds:";
  least_fixpoint_bindings_with_self_init ~max_iters ~abstracts kinds
