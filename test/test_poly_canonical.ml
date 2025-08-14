open Jkinds_lib

let assert_true msg b = if not b then failwith ("assert: " ^ msg)

module C = Product_lattice.Make(struct let axis_sizes = [|3;2|] end)
module V = struct type t = string let compare = String.compare end
module P = Lattice_polynomial.Make(C)(V)

let encode a b = C.encode ~levels:[|a;b|]

let reconstruct_from_canonical (p : P.t) : P.t =
  let bindings = P.terms p in
  let sets = List.map fst bindings in
  let raw_terms =
    List.map
      (fun s ->
         let c =
           List.fold_left
             (fun acc (t, ch) -> if P.VarSet.subset t s then C.join acc ch else acc)
             C.bot bindings
         in
         (s, c))
      sets
  in
  P.of_terms raw_terms |> P.canonicalize

let () =
  Random.init 20250814;
  (* 1) Reconstruction property: canonical -> raw -> canonical *)
  for _=1 to 200 do
    let v s = P.VarSet.of_list s in
    let ts = [
      (v ["x"], encode (Random.int 3) (Random.int 2));
      (v ["y"], encode (Random.int 3) (Random.int 2));
      (v ["x";"y"], encode (Random.int 3) (Random.int 2));
      (v [], encode (Random.int 3) (Random.int 2));
    ] in
    let p = P.of_terms ts |> P.canonicalize in
    let p' = reconstruct_from_canonical p in
    assert_true "reconstruct canonical" (P.equal p p')
  done;

  (* 2) No redundancy/minimality checks *)
  let v s = P.VarSet.of_list s in
  let base = P.of_terms [ (v ["x"], encode 2 1); (v ["y"], encode 1 1); (v [], encode 1 0) ] |> P.canonicalize in
  (* No ⊥ entries *)
  List.iter (fun (_s,c) -> assert_true "no bot in canonical" (not (C.leq c C.bot))) (P.terms base);
  (* For any S⊂T present, adding lower into upper then canonicalize yields same *)
  let bindings = P.terms base in
  List.iter (fun (s,c_s) ->
    List.iter (fun (t,_) ->
      if s != t && P.VarSet.subset s t then (
        let mutated =
          bindings
          |> List.map (fun (u,c) -> if P.VarSet.compare u t = 0 then (u, C.join c c_s) else (u,c))
          |> P.of_terms |> P.canonicalize
        in
        assert_true "superset join lower unchanged" (P.equal mutated base)
      )
    ) bindings
  ) bindings;

  (* 3) Deterministic ordering: nondecreasing degree then lex *)
  let p_order = P.of_terms [ (v ["y"], encode 1 0); (v [], encode 1 1); (v ["x";"y"], encode 2 1); (v ["x"], encode 2 0) ] |> P.canonicalize in
  let terms = P.terms p_order in
  let degrees = List.map (fun (s,_) -> P.VarSet.cardinal s) terms in
  assert_true "nondecreasing degree" (List.sort compare degrees = degrees);
  (* Check lex within equal degree *)
  let rec check_lex = function
    | (s1,_)::(s2,_)::tl when P.VarSet.cardinal s1 = P.VarSet.cardinal s2 ->
        assert_true "lex within degree" (P.VarSet.compare s1 s2 <= 0);
        check_lex ((s2, C.bot)::tl)
    | _::tl -> check_lex tl
    | [] -> ()
  in
  check_lex terms;

  (* 4) of_terms duplicate merging *)
  let sxy = v ["x";"y"] in
  let c1 = encode 1 1 and c2 = encode 2 0 in
  let p_dup = P.of_terms [ (sxy, c1); (sxy, c2); (v ["x"], encode 0 1) ] |> P.canonicalize in
  let p_merged = P.of_terms [ (sxy, C.join c1 c2); (v ["x"], encode 0 1) ] |> P.canonicalize in
  assert_true "duplicates merged via join" (P.equal p_dup p_merged);

  (* 5) Support laws *)
  let p1 = P.of_terms [ (v ["x"], encode 1 0); (v ["y"], encode 0 1) ] |> P.canonicalize in
  let p2 = P.of_terms [ (v ["z"], encode 2 1); (v ["x";"y"], encode 1 1) ] |> P.canonicalize in
  let sup_join = P.support (P.join p1 p2) in
  let sup_meet = P.support (P.meet p1 p2) in
  let union = P.VarSet.union (P.support p1) (P.support p2) in
  assert_true "support join" (P.VarSet.compare sup_join union = 0);
  assert_true "support meet" (P.VarSet.compare sup_meet union = 0);

  (* 6) Substitution homomorphisms *)
  let sub = P.VarMap.(empty |> add "x" (P.of_terms [ (v ["z"], encode 1 1) ] |> P.canonicalize)) in
  let lhs_j = P.subst ~subs:sub (P.join p1 p2) in
  let rhs_j = P.join (P.subst ~subs:sub p1) (P.subst ~subs:sub p2) in
  assert_true "subst over join" (P.equal lhs_j rhs_j);
  let lhs_m = P.subst ~subs:sub (P.meet p1 p2) in
  let rhs_m = P.meet (P.subst ~subs:sub p1) (P.subst ~subs:sub p2) in
  assert_true "subst over meet" (P.equal lhs_m rhs_m);
  print_endline "poly canonical tests passed"


