let kindof (t : Type_syntax.t) : Kind.t =
  let open Type_syntax in
  let rec go (t : Type_syntax.t) : Kind.t =
    match t with
    | Var a ->
        Kind.set (Kind.set Kind.empty a Modality.id) Kind.Var.a0 Modality.zero
    | Unit ->
        Kind.set Kind.empty Kind.Var.a0 Modality.zero
    | Pair (a, b) | Sum (a, b) ->
        Kind.max (go a) (go b)
    | Mod_annot (t', levels) ->
        let k = go t' in
        let m = Modality.of_levels levels in
        Kind.apply m k
    | Mod_const levels ->
        let m = Modality.of_levels levels in
        (* As a type, a bare modality constant contributes that meet to the sole var 1 *)
        Kind.set Kind.empty 0 m
    | C (name, args) ->
        let base =
          Kind.set Kind.empty Kind.Var.a0 (Modality.of_atom { Modality.ctor = name; index = 0 })
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

let kinds_of_decls (decls : Type_syntax.t Decl_parser.NameMap.t) : Kind.t Decl_parser.NameMap.t =
  Decl_parser.NameMap.map kindof decls

let kinds_of_decls_bindings (bs : (string * Type_syntax.t) list) : (string * Kind.t) list =
  List.map (fun (k, v) -> (k, kindof v)) bs

let substitute_kinds ~lhs ~rhs : Kind.t Decl_parser.NameMap.t =
  let lookup_atom (a : Modality.atom) : Modality.t option =
    match Decl_parser.NameMap.find_opt a.ctor rhs with
    | None -> None
    | Some k_rhs ->
        (* Default missing indices to ⊥ to allow zero-arity ctors to substitute admin index safely *)
        Some (Kind.get k_rhs a.index)
  in
  Decl_parser.NameMap.map (fun k -> Kind.substitute_using lookup_atom k) lhs

let substitute_kinds_bindings ~(lhs : (string * Kind.t) list) ~(rhs : (string * Kind.t) list) : (string * Kind.t) list =
  let rhs_map = List.fold_left (fun acc (n, k) -> Decl_parser.NameMap.add n k acc) Decl_parser.NameMap.empty rhs in
  let subst_one (n, k) =
    let lookup_atom (a : Modality.atom) : Modality.t option =
      match Decl_parser.NameMap.find_opt a.ctor rhs_map with
      | None -> None
      | Some k_rhs -> Some (Kind.get k_rhs a.index)
    in
    (n, Kind.substitute_using lookup_atom k)
  in
  List.map subst_one lhs

let zero_constructor_entries (km : Kind.t Decl_parser.NameMap.t) : Kind.t Decl_parser.NameMap.t =
  Decl_parser.NameMap.map Kind.zero_entries km

let zero_constructor_entries_bindings (bs : (string * Kind.t) list) : (string * Kind.t) list =
  List.map (fun (n, k) -> (n, Kind.zero_entries k)) bs

let least_fixpoint ?(max_iters = 10) (km : Kind.t Decl_parser.NameMap.t) : Kind.t Decl_parser.NameMap.t =
  let pp_map_lines (m : Kind.t Decl_parser.NameMap.t) : string =
    Decl_parser.NameMap.bindings m
    |> List.map (fun (n, k) -> Printf.sprintf "  %s: %s" n (Kind.pp k))
    |> String.concat "\n"
  in
  let rec loop i current =
    if i > max_iters then
      failwith "least_fixpoint: did not converge within bound"
    else (
      Printf.printf "[lfp] iter %d:\n%s\n" i (pp_map_lines current);
      let next = substitute_kinds ~lhs:km ~rhs:current in
      let kinds_equal a b =
        Decl_parser.NameMap.equal (fun k1 k2 -> Kind.equal k1 k2) a b
      in
      if kinds_equal current next then current else loop (i + 1) next)
  in
  let start = zero_constructor_entries km in
  loop 0 start

let least_fixpoint_bindings ?(max_iters = 10) (bs : (string * Kind.t) list) : (string * Kind.t) list =
  let rec loop i current =
    if i > max_iters then
      failwith "least_fixpoint: did not converge within bound"
    else (
      let lines = List.map (fun (n, k) -> Printf.sprintf "  %s: %s" n (Kind.pp k)) current |> String.concat "\n" in
      Printf.printf "[lfp] iter %d:\n%s\n" i lines;
      let next = substitute_kinds_bindings ~lhs:bs ~rhs:current in
      let lists_equal l1 l2 =
        let sort = List.sort (fun (a, _) (b, _) -> String.compare a b) in
        let l1 = sort l1 and l2 = sort l2 in
        List.length l1 = List.length l2 && List.for_all2 (fun (n1, k1) (n2, k2) -> n1 = n2 && Kind.equal k1 k2) l1 l2
      in
      if lists_equal next current then current else loop (i + 1) next)
  in
  let start = zero_constructor_entries_bindings bs in
  loop 0 start

