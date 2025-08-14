open Jkinds_lib

let () =
  let m1 = Modality.of_levels [|1;0|] in
  let a = Modality.of_atom { Modality.ctor = "C"; index = 1 } in
  let m = Modality.compose m1 a in
  let s = Modality.pp m in
  if not (String.equal s "{{[1,0] ⊓ {C.1}}}") then
    failwith ("unexpected pp: " ^ s)


