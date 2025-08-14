open Jkinds_lib

let () =
  let open Kind in
  let k = set (set empty 1 Modality.id) 2 Modality.id in
  let m = Modality.of_levels [|1;0|] in
  let k' = apply m k in
  match find_opt k' 1, find_opt k' 2 with
  | Some m1, Some m2 ->
      let s = Modality.pp m1 in
      let s2 = Modality.pp m2 in
      if not (String.contains s '[' && String.contains s2 '[') then
        failwith ("apply should inject coeffs: " ^ s ^ "; " ^ s2)
  | _ -> failwith "missing vars"


