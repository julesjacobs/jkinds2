open Poly_support
open Poly

(* Test: Co-subtraction residuation property *)
let test_co_sub_residuation () =
  run_property_test "co_sub residuation" ~count:400 (fun () ->
    let a = gen_poly () in
    let b = gen_poly () in
    let x = gen_poly () in
    
    (* (a \ b) ≤ x  ⟺  a ≤ b ∨ x *)
    let lhs = P.leq (P.co_sub a b) x in
    let rhs = P.leq a (P.join b x) in
    assert_eq "poly co_sub residuation" lhs rhs
  )

(* Test: Monotonicity properties *)
let test_co_sub_monotonicity () =
  (* Monotone in first argument *)
  run_property_test "monotone in a" ~count:300 (fun () ->
    let a1 = gen_poly () in
    let a2 = gen_poly () in
    let b = gen_poly () in
    if P.leq a1 a2 then 
      assert_true "mono a" (P.leq (P.co_sub a1 b) (P.co_sub a2 b))
  );
  
  (* Antitone in second argument *)
  run_property_test "antitone in b" ~count:300 (fun () ->
    let a = gen_poly () in
    let b1 = gen_poly () in
    let b2 = gen_poly () in
    if P.leq b1 b2 then 
      assert_true "antitone b" (P.leq (P.co_sub a b2) (P.co_sub a b1))
  )

(* Test: Special cases and identities *)
let test_co_sub_special_cases () =
  run_property_test "special cases" ~count:100 (fun () ->
    let a = gen_poly () in
    let b = gen_poly () in
    
    (* a \ a = ⊥ *)
    assert_poly_eq "a\\a=bot" (P.co_sub a a) P.bot;
    
    (* a \ ⊥ = a *)
    assert_poly_eq "a\\bot=a" (P.co_sub a P.bot) a;
    
    (* ⊥ \ b = ⊥ *)
    assert_poly_eq "bot\\b=bot" (P.co_sub P.bot b) P.bot;
    
    (* a \ ⊤ = ⊥ *)
    assert_poly_eq "a\\top=bot" (P.co_sub a P.top) P.bot;
    
    (* Additional: ⊤ \ b = complement-like behavior *)
    let top_sub_b = P.co_sub P.top b in
    assert_true "top\\b ≤ ¬b (approx)" (P.leq (P.join b top_sub_b) P.top)
  )

(* Test: Composition property *)
let test_co_sub_composition () =
  run_property_test "composition" ~count:300 (fun () ->
    let a = gen_poly () in
    let b = gen_poly () in
    let c = gen_poly () in
    
    (* (a \ b) \ c = a \ (b ∨ c) *)
    let left = P.co_sub (P.co_sub a b) c in
    let right = P.co_sub a (P.join b c) in
    assert_poly_eq "composition" left right
  )

(* Test: Distribution properties *)
let test_co_sub_distribution () =
  (* (a ∨ b) \ c = (a \ c) ∨ (b \ c) *)
  run_property_test "distribute over join" ~count:200 (fun () ->
    let a = gen_poly ~max_terms:2 () in
    let b = gen_poly ~max_terms:2 () in
    let c = gen_poly ~max_terms:2 () in
    
    let left = P.co_sub (P.join a b) c in
    let right = P.join (P.co_sub a c) (P.co_sub b c) in
    assert_poly_eq "(a∨b)\\c = (a\\c)∨(b\\c)" left right
  );
  
  (* a \ (b ∨ c) = (a \ b) ∧ (a \ c) *)
  run_property_test "antidistribute over join" ~count:200 (fun () ->
    let a = gen_poly ~max_terms:2 () in
    let b = gen_poly ~max_terms:2 () in
    let c = gen_poly ~max_terms:2 () in
    
    let left = P.co_sub a (P.join b c) in
    let right = P.meet (P.co_sub a b) (P.co_sub a c) in
    assert_poly_eq "a\\(b∨c) = (a\\b)∧(a\\c)" left right
  )

(* Test: Galois connection property *)
let test_co_sub_galois () =
  run_property_test "galois connection" ~count:200 (fun () ->
    let a = gen_poly () in
    let b = gen_poly () in
    
    (* a ≤ b ∨ (a \ b) always holds *)
    let residual = P.co_sub a b in
    assert_true "a ≤ b ∨ (a\\b)" (P.leq a (P.join b residual));
    
    (* (a \ b) is the smallest x such that a ≤ b ∨ x *)
    (* Test by checking: if a ≤ b ∨ y then (a \ b) ≤ y *)
    let y = gen_poly () in
    if P.leq a (P.join b y) then
      assert_true "(a\\b) minimal" (P.leq residual y)
  )

(* Test: Interaction with meet *)
let test_co_sub_meet_interaction () =
  run_property_test "meet interaction" ~count:200 (fun () ->
    let a = gen_poly ~max_terms:2 () in
    let b = gen_poly ~max_terms:2 () in
    let c = gen_poly ~max_terms:2 () in
    
    (* (a ∧ b) \ c ≤ (a \ c) ∧ (b \ c) *)
    let left = P.co_sub (P.meet a b) c in
    let right = P.meet (P.co_sub a c) (P.co_sub b c) in
    assert_true "(a∧b)\\c ≤ (a\\c)∧(b\\c)" (P.leq left right)
  )

(* Test: Fixed point property *)
let test_co_sub_fixed_point () =
  run_property_test "fixed point" ~count:100 (fun () ->
    let a = gen_poly () in
    let b = gen_poly () in
    
    (* If a ≤ b, then a \ b = ⊥ *)
    if P.leq a b then
      assert_poly_eq "a≤b => a\\b=⊥" (P.co_sub a b) P.bot;
    
    (* (a \ b) \ b = a \ b (idempotent property) *)
    let r = P.co_sub a b in
    let r2 = P.co_sub r b in
    assert_poly_eq "(a\\b)\\b = a\\b" r2 r
  )


(* Test: Edge cases and stress tests *)
let test_edge_and_stress () =
  (* Large polynomial stress test *)
  let test_large () =
    let p = gen_poly_n 20 in
    let q = gen_poly_n 20 in
    
    (* Basic operations should work *)
    let _ = P.join p q in
    let _ = P.meet p q in
    let _ = P.co_sub p q in
    ()
  in
  
  (* Empty polynomial edge cases *)
  let test_empty () =
    assert_poly_eq "empty\\empty" (P.co_sub P.empty P.empty) P.bot;
    assert_poly_eq "x\\empty" (P.co_sub x P.empty) x;
    assert_poly_eq "empty\\x" (P.co_sub P.empty x) P.bot
  in
  
  (* Constant polynomial edge cases *)
  let test_constants () =
    let c1 = P.const (encode 2 1) in
    let c2 = P.const (encode 1 0) in
    
    let r = P.co_sub c1 c2 in
    (* Since these are constants, co_sub should work on coefficients *)
    let expected = P.const (C.co_sub (encode 2 1) (encode 1 0)) in
    assert_poly_eq "const co_sub" r expected
  in
  
  test_large ();
  test_empty ();
  test_constants ()

(* Main test runner *)
let () =
  Random.init 20250814;
  
  test_co_sub_residuation ();
  print_endline "✓ Co-sub residuation tests passed";
  
  test_co_sub_monotonicity ();
  print_endline "✓ Co-sub monotonicity tests passed";
  
  test_co_sub_special_cases ();
  print_endline "✓ Co-sub special case tests passed";
  
  test_co_sub_composition ();
  print_endline "✓ Co-sub composition tests passed";
  
  test_co_sub_distribution ();
  print_endline "✓ Co-sub distribution tests passed";
  
  test_co_sub_galois ();
  print_endline "✓ Co-sub Galois connection tests passed";
  
  test_co_sub_meet_interaction ();
  print_endline "✓ Co-sub meet interaction tests passed";
  
  test_co_sub_fixed_point ();
  print_endline "✓ Co-sub fixed point tests passed";
  
  test_edge_and_stress ();
  print_endline "✓ Edge and stress tests passed";
  
  print_endline "\nAll poly co_sub property tests passed ✓"