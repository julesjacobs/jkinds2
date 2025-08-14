open Poly_support
open Poly

(* Test: Semantic homomorphisms for join and meet *)
let test_semantic_homomorphisms () =
  run_property_test "semantic homomorphisms" ~count:500 (fun () ->
    let p = gen_poly () in
    let q = gen_poly () in
    let rho = gen_env () in
    
    (* eval distributes over join *)
    let ej = P.eval rho (P.join p q) in
    let ej_expected = C.join (P.eval rho p) (P.eval rho q) in
    assert_eq "eval join" ej_expected ej;
    
    (* eval distributes over meet *)
    let em = P.eval rho (P.meet p q) in
    let em_expected = C.meet (P.eval rho p) (P.eval rho q) in
    assert_eq "eval meet" em_expected em;
    
    (* Canonicalization preserves semantics *)
    let canonical_p = P.join p P.bot in
    assert_eq "eval canonicalize" (P.eval rho p) (P.eval rho canonical_p)
  )

(* Test: Substitution semantics *)
let test_substitution_semantics () =
  run_property_test "substitution semantics" ~count:300 (fun () ->
    let p = gen_poly () in
    
    (* Build small substitution mapping *)
    let subs = ref P.VarMap.empty in
    let k = Random.int 3 in
    for _=1 to k do
      subs := P.VarMap.add (gen_var ()) (gen_poly ()) !subs
    done;
    
    let rho = gen_env () in
    
    (* Modified environment that evaluates substituted variables *)
    let rho' v = match P.VarMap.find_opt v !subs with
      | None -> rho v
      | Some pv -> P.eval rho pv
    in
    
    let lhs = P.eval rho (P.subst ~subs:!subs p) in
    let rhs = P.eval rho' p in
    assert_eq "subst semantics" lhs rhs
  )

(* Test: Lattice unit laws *)
let test_lattice_units () =
  run_property_test "lattice units" ~count:200 (fun () ->
    let p = gen_poly () in
    
    (* Bot is unit for join *)
    assert_poly_eq "join bot" (P.join p P.bot) p;
    
    (* Top is unit for meet *)
    assert_poly_eq "meet top" (P.meet p P.top) p;
    
    (* Top is absorbing for join *)
    assert_poly_eq "join top" (P.join p P.top) P.top;
    
    (* Bot is absorbing for meet *)
    assert_poly_eq "meet bot" (P.meet p P.bot) P.bot
  )

(* Test: Idempotence *)
let test_idempotence () =
  run_property_test "idempotence" ~count:100 (fun () ->
    let p = gen_poly () in
    assert_poly_eq "join idempotent" (P.join p p) p;
    assert_poly_eq "meet idempotent" (P.meet p p) p
  )

(* Test: Commutativity *)
let test_commutativity () =
  run_property_test "commutativity" ~count:100 (fun () ->
    let p = gen_poly () in
    let q = gen_poly () in
    assert_poly_eq "join commutative" (P.join p q) (P.join q p);
    assert_poly_eq "meet commutative" (P.meet p q) (P.meet q p)
  )

(* Test: Associativity *)
let test_associativity () =
  run_property_test "associativity" ~count:100 (fun () ->
    let p = gen_poly ~max_terms:2 () in
    let q = gen_poly ~max_terms:2 () in
    let r = gen_poly ~max_terms:2 () in
    
    (* Join associativity *)
    let j1 = P.join (P.join p q) r in
    let j2 = P.join p (P.join q r) in
    assert_poly_eq "join associative" j1 j2;
    
    (* Meet associativity *)
    let m1 = P.meet (P.meet p q) r in
    let m2 = P.meet p (P.meet q r) in
    assert_poly_eq "meet associative" m1 m2
  )

(* Test: Absorption laws *)
let test_absorption () =
  run_property_test "absorption" ~count:100 (fun () ->
    let p = gen_poly () in
    let q = gen_poly () in
    
    (* p ∨ (p ∧ q) = p *)
    let abs1 = P.join p (P.meet p q) in
    assert_poly_eq "absorption 1" abs1 p;
    
    (* p ∧ (p ∨ q) = p *)
    let abs2 = P.meet p (P.join p q) in
    assert_poly_eq "absorption 2" abs2 p
  )

(* Test: Order properties *)
let test_order_properties () =
  (* Reflexivity *)
  run_property_test "reflexivity" ~count:50 (fun () ->
    let p = gen_poly () in
    assert_true "p ≤ p" (P.leq p p)
  );
  
  (* Antisymmetry *)
  run_property_test "antisymmetry" ~count:50 (fun () ->
    let p = gen_poly () in
    let q = gen_poly () in
    if P.leq p q && P.leq q p then
      assert_poly_eq "antisymmetric" p q
  );
  
  (* Transitivity *)
  run_property_test "transitivity" ~count:50 (fun () ->
    let p = gen_poly ~max_terms:2 () in
    let q = P.join p (gen_poly ~max_terms:1 ()) in
    let r = P.join q (gen_poly ~max_terms:1 ()) in
    assert_true "transitive" (P.leq p r)
  )

(* Test: Specific semantic examples *)
let test_specific_semantics () =
  (* Test 1: Simple polynomial evaluation *)
  let p = P.join (P.meet x (P.const (encode 2 0))) 
                 (P.meet y (P.const (encode 1 1))) in
  let rho = fixed_env (encode 2 1) (encode 0 1) C.bot C.bot in
  let result = P.eval rho p in
  let expected = C.join (C.meet (encode 2 1) (encode 2 0))
                        (C.meet (encode 0 1) (encode 1 1)) in
  assert_eq "specific eval 1" expected result;
  
  (* Test 2: Constant polynomial *)
  let c = P.const (encode 2 1) in
  let result2 = P.eval rho c in
  assert_eq "const eval" (encode 2 1) result2;
  
  (* Test 3: Bot polynomial *)
  let result3 = P.eval rho P.bot in
  assert_eq "bot eval" C.bot result3

(* Main test runner *)
let () =
  Random.init 20250813;
  
  test_semantic_homomorphisms ();
  print_endline "✓ Semantic homomorphism tests passed";
  
  test_substitution_semantics ();
  print_endline "✓ Substitution semantics tests passed";
  
  test_lattice_units ();
  print_endline "✓ Lattice unit tests passed";
  
  test_idempotence ();
  print_endline "✓ Idempotence tests passed";
  
  test_commutativity ();
  print_endline "✓ Commutativity tests passed";
  
  test_associativity ();
  print_endline "✓ Associativity tests passed";
  
  test_absorption ();
  print_endline "✓ Absorption tests passed";
  
  test_order_properties ();
  print_endline "✓ Order property tests passed";
  
  test_specific_semantics ();
  print_endline "✓ Specific semantics tests passed";
  
  print_endline "\nAll poly semantics tests passed ✓"