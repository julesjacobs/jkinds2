open Jkinds_lib

let mk_atom ctor index = Modality.of_atom { Modality.ctor = ctor; index }

let () =
  (* rhs kinds: F -> {'a1 -> A.1, 'a2 -> B.1} *)
  let rhs_f = Kind.set (Kind.set Kind.empty 1 (mk_atom "A" 1)) 2 (mk_atom "B" 1) in
  let rhs = [ ("F", rhs_f) ] in
  (* lhs kinds: H -> {'a1 -> F.1, 'a2 -> F.2} *)
  let lhs_h = Kind.set (Kind.set Kind.empty 1 (mk_atom "F" 1)) 2 (mk_atom "F" 2) in
  let lhs = [ ("H", lhs_h) ] in
  let res = Infer.substitute_kinds_bindings ~lhs ~rhs in
  let h' = List.assoc "H" res in
  assert (Kind.get h' 1 = mk_atom "A" 1);
  assert (Kind.get h' 2 = mk_atom "B" 1);
  print_endline "✓ substitution basic replacement passed";
;;

let () =
  (* distribution over max: F.1 ⊓ D.1 with F.1 -> (A.1 ⊔ B.1) *)
  let rhs_f = Kind.set Kind.empty 1 (Modality.max (mk_atom "A" 1) (mk_atom "B" 1)) in
  let rhs = [ ("F", rhs_f) ] in
  let lhs_h = Kind.set Kind.empty 1 (Modality.compose (mk_atom "F" 1) (mk_atom "D" 1)) in
  let lhs = [ ("H", lhs_h) ] in
  let res = Infer.substitute_kinds_bindings ~lhs ~rhs in
  let h' = List.assoc "H" res in
  let expected =
    Modality.max
      (Modality.compose (mk_atom "A" 1) (mk_atom "D" 1))
      (Modality.compose (mk_atom "B" 1) (mk_atom "D" 1))
  in
  let actual = Kind.get h' 1 in
  assert (Modality.pp actual = Modality.pp expected);
  print_endline "✓ substitution distributivity passed";
;;

let () =
  (* missing constructor remains unchanged *)
  let rhs = [] in
  let lhs_h = Kind.set Kind.empty 1 (mk_atom "X" 1) in
  let lhs = [ ("H", lhs_h) ] in
  let res = Infer.substitute_kinds_bindings ~lhs ~rhs in
  let h' = List.assoc "H" res in
  assert (Kind.get h' 1 = mk_atom "X" 1);
  print_endline "✓ substitution missing constructor passed";
;;

let () =
  (* out-of-bounds arity should raise *)
  let rhs_f = Kind.set Kind.empty 1 (mk_atom "A" 1) in
  let rhs = [ ("F", rhs_f) ] in
  let lhs_h = Kind.set Kind.empty 1 (mk_atom "F" 2) in
  let lhs = [ ("H", lhs_h) ] in
  (* New behavior: missing indices default to ⊥ rather than raising *)
  let res = Infer.substitute_kinds_bindings ~lhs ~rhs in
  let h' = List.assoc "H" res in
  assert (Kind.get h' 1 = Modality.zero);
  print_endline "✓ substitution out-of-bounds defaults to ⊥ passed";
;;

let () =
  (* identity substitution: F.1 -> id, then F.1 ⊓ D.2 becomes D.2 *)
  let rhs_f = Kind.set Kind.empty 1 Modality.id in
  let rhs = [ ("F", rhs_f) ] in
  let lhs_h = Kind.set Kind.empty 1 (Modality.compose (mk_atom "F" 1) (mk_atom "D" 2)) in
  let lhs = [ ("H", lhs_h) ] in
  let res = Infer.substitute_kinds_bindings ~lhs ~rhs in
  let h' = List.assoc "H" res in
  assert (Kind.get h' 1 = mk_atom "D" 2);
  print_endline "✓ substitution identity passed";
;;

let () =
  (* multiple constructors in lhs should be substituted *)
  let rhs_f = Kind.set Kind.empty 1 (mk_atom "A" 1) in
  let rhs_g = Kind.set Kind.empty 1 (mk_atom "B" 2) in
  let rhs = [ ("F", rhs_f); ("G", rhs_g) ] in
  let h_kind = Kind.set Kind.empty 1 (mk_atom "F" 1) in
  let q_kind = Kind.set Kind.empty 1 (mk_atom "G" 1) in
  let lhs = [ ("H", h_kind); ("Q", q_kind) ] in
  let res = Infer.substitute_kinds_bindings ~lhs ~rhs in
  assert (Kind.get (List.assoc "H" res) 1 = mk_atom "A" 1);
  assert (Kind.get (List.assoc "Q" res) 1 = mk_atom "B" 2);
  print_endline "✓ substitution across multiple constructors passed";
;;

let () =
  (* zeroing all constructor entries *)
  let km = [ ("F", Kind.set Kind.empty 1 (mk_atom "A" 1)); ("G", Kind.set Kind.empty 1 (Modality.max (mk_atom "B" 1) (mk_atom "C" 2))) ] in
  let km0 = Infer.zero_constructor_entries_bindings km in
  let f0 = List.assoc "F" km0 in
  let g0 = List.assoc "G" km0 in
  assert (Kind.get f0 1 = Modality.zero);
  assert (Kind.get g0 1 = Modality.zero);
  print_endline "✓ zero constructor entries passed";
;;

let () =
  (* least fixpoint simple test *)
  (* Kinds: H('a1) = F.1 ⊓ H.1; F('a1) = 'a1. Expect lfp H.1 = {}, F unused. *)
  let h_kind = Kind.set Kind.empty 1 (Modality.compose (mk_atom "F" 1) (mk_atom "H" 1)) in
  let f_kind = Kind.set Kind.empty 1 (mk_atom "A" 1) in
  let km = [ ("H", h_kind); ("F", f_kind) ] in
  let _ = Infer.least_fixpoint_bindings km in
  print_endline "✓ least fixpoint ran";
;;
