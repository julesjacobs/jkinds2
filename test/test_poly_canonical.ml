open Poly_support
open Poly

(* Test: Canonical form reconstruction property *)
let test_canonical_reconstruction () =
  let reconstruct_from_canonical (p : P.t) : P.t =
    let bindings = P.to_list p in
    let sets = List.map fst bindings in
    let raw_terms =
      List.map
        (fun s ->
          let c =
            List.fold_left
              (fun acc (t, ch) ->
                if P.VarSet.subset t s then C.join acc ch else acc)
              C.bot bindings
          in
          (s, c))
        sets
    in
    P.of_list raw_terms
  in

  run_property_test "canonical reconstruction" ~count:200 (fun () ->
      let ts =
        [
          (v [ "x" ], encode (Random.int 3) (Random.int 2));
          (v [ "y" ], encode (Random.int 3) (Random.int 2));
          (v [ "x"; "y" ], encode (Random.int 3) (Random.int 2));
          (v [], encode (Random.int 3) (Random.int 2));
        ]
      in
      let p = P.of_list ts in
      let p' = reconstruct_from_canonical p in
      assert_poly_eq "reconstruct canonical" p p')

(* Test: Canonical form minimality and no redundancy *)
let test_canonical_minimality () =
  let base =
    P.of_list
      [ (v [ "x" ], encode 2 1); (v [ "y" ], encode 1 1); (v [], encode 1 0) ]
  in

  (* No ⊥ entries in canonical form *)
  List.iter
    (fun (_s, c) -> assert_false "no bot in canonical" (C.equal c C.bot))
    (P.to_list base);

  (* For any S⊂T present, adding lower into upper then canonicalize yields
     same *)
  let bindings = P.to_list base in
  List.iter
    (fun (s, c_s) ->
      List.iter
        (fun (t, _) ->
          if s != t && P.VarSet.subset s t then
            let mutated =
              bindings
              |> List.map (fun (u, c) ->
                     if P.VarSet.compare u t = 0 then (u, C.join c c_s)
                     else (u, c))
              |> P.of_list
            in
            assert_poly_eq "superset join lower unchanged" mutated base)
        bindings)
    bindings

(* Test: Deterministic ordering of terms *)
let test_canonical_ordering () =
  let p_order =
    P.of_list
      [
        (v [ "y" ], encode 1 0);
        (v [], encode 1 1);
        (v [ "x"; "y" ], encode 2 1);
        (v [ "x" ], encode 2 0);
      ]
  in

  let terms = P.to_list p_order in
  let degrees = List.map (fun (s, _) -> P.VarSet.cardinal s) terms in

  (* Check nondecreasing degree *)
  assert_true "nondecreasing degree" (List.sort compare degrees = degrees);

  (* Check lexicographic order within equal degree *)
  let rec check_lex = function
    | (s1, _) :: (s2, _) :: tl when P.VarSet.cardinal s1 = P.VarSet.cardinal s2
      ->
      assert_true "lex within degree" (P.VarSet.compare s1 s2 <= 0);
      check_lex ((s2, C.bot) :: tl)
    | _ :: tl -> check_lex tl
    | [] -> ()
  in
  check_lex terms

(* Test: Duplicate term merging *)
let test_duplicate_merging () =
  let sxy = v [ "x"; "y" ] in
  let c1 = encode 1 1
  and c2 = encode 2 0 in
  let p_dup = P.of_list [ (sxy, c1); (sxy, c2); (v [ "x" ], encode 0 1) ] in
  let p_merged = P.of_list [ (sxy, C.join c1 c2); (v [ "x" ], encode 0 1) ] in
  assert_poly_eq "duplicates merged via join" p_dup p_merged

(* Test: Support laws *)
let test_support_laws () =
  let p1 = P.of_list [ (v [ "x" ], encode 1 0); (v [ "y" ], encode 0 1) ] in
  let p2 =
    P.of_list [ (v [ "z" ], encode 2 1); (v [ "x"; "y" ], encode 1 1) ]
  in

  let sup_join = P.support (P.join p1 p2) in
  let sup_meet = P.support (P.meet p1 p2) in
  let union = P.VarSet.union (P.support p1) (P.support p2) in

  assert_true "support join = union" (P.VarSet.equal sup_join union);
  assert_true "support meet ⊆ union" (P.VarSet.subset sup_meet union)

(* Test: Substitution homomorphisms *)
let test_substitution_homomorphisms () =
  let p1 = P.of_list [ (v [ "x" ], encode 1 0); (v [ "y" ], encode 0 1) ] in
  let p2 =
    P.of_list [ (v [ "z" ], encode 2 1); (v [ "x"; "y" ], encode 1 1) ]
  in

  let sub =
    P.VarMap.(empty |> add "x" (P.of_list [ (v [ "z" ], encode 1 1) ]))
  in

  (* Substitution distributes over join *)
  let lhs_j = P.subst ~subs:sub (P.join p1 p2) in
  let rhs_j = P.join (P.subst ~subs:sub p1) (P.subst ~subs:sub p2) in
  assert_poly_eq "subst over join" lhs_j rhs_j;

  (* Substitution distributes over meet *)
  let lhs_m = P.subst ~subs:sub (P.meet p1 p2) in
  let rhs_m = P.meet (P.subst ~subs:sub p1) (P.subst ~subs:sub p2) in
  assert_poly_eq "subst over meet" lhs_m rhs_m

(* Test: Edge cases *)
let test_edge_cases () =
  (* Bot polynomial *)
  assert_poly_eq "bot = bot" P.bot P.bot;
  assert_true "bot equals bot" (P.equal P.bot P.bot);

  (* Constant polynomials *)
  let c1 = P.const (encode 1 1) in
  assert_false "const not bot" (P.equal c1 P.bot);

  (* Bot constant *)
  let c_bot = P.const C.bot in
  assert_poly_eq "const bot = bot" c_bot P.bot;

  (* Single variable *)
  let px = P.var "x" in
  assert_true "var x support = {x}" (P.VarSet.equal (P.support px) (vs "x"))

(* Main test runner *)
let () =
  Random.init 20250814;

  test_canonical_reconstruction ();
  print_endline "✓ Canonical reconstruction tests passed";

  test_canonical_minimality ();
  print_endline "✓ Canonical minimality tests passed";

  test_canonical_ordering ();
  print_endline "✓ Canonical ordering tests passed";

  test_duplicate_merging ();
  print_endline "✓ Duplicate merging tests passed";

  test_support_laws ();
  print_endline "✓ Support laws tests passed";

  test_substitution_homomorphisms ();
  print_endline "✓ Substitution homomorphism tests passed";

  test_edge_cases ();
  print_endline "✓ Edge case tests passed";

  print_endline "\nAll poly canonical tests passed ✓"
