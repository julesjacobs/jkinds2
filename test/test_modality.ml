open Jkinds_lib

let () =
  let open Modality in
  let f0 = of_atom { ctor = "F"; index = 0 } in
  let f1 = of_atom { ctor = "F"; index = 1 } in
  let idm = id in
  let zero = zero in
  assert ((compose idm f1) = f1);
  assert ((compose zero f1) = zero);
  assert ((compose f1 zero) = zero);
  ignore f0;
  print_endline "✓ modality tests passed"
;;

let () =
  (* pruning: {{A.1 ⊓ B.1} ⊔ {A.1}} -> {{A.1 ⊓ B.1}} *)
  let open Modality in
  let a1 = of_atom { ctor = "A"; index = 1 } in
  let b1 = of_atom { ctor = "B"; index = 1 } in
  let ab = compose a1 b1 in
  let m = max ab a1 in
  (* With supersets pruned, {{A.1 ⊓ B.1} ⊔ {A.1}} -> {{A.1}} *)
  assert (equal m a1);
  print_endline "✓ modality pruning test passed"

