open Jkinds_lib

let () =
  let open Kind in
  let open Modality in
  let k1 = set empty 1 id in
  let k2 = set empty 2 (of_atom { ctor = "F"; index = 1 }) in
  let k = Kind.max k1 k2 in
  assert (get k 1 = id);
  assert (get k 2 = of_atom { ctor = "F"; index = 1 });
  let m = of_atom { ctor = "G"; index = 3 } in
  let k' = apply m k in
  assert (get k' 1 = compose m id);
  assert (get k' 2 = compose m (of_atom { ctor = "F"; index = 1 }));
  print_endline "✓ kind ops tests passed"

let () =
  let open Type_syntax in
  let k_var =
    Jkinds_lib.Infer.solve_program
      [
        { Decl_parser.name = "DUMMY"; arity = 1; rhs = Var 5; abstract = false };
      ]
      ~max_iters:0
    |> ignore;
    Kind.set Kind.empty 5 Modality.id
  in
  assert (Kind.get k_var 5 = Modality.id);
  let t = C ("F", [ Var 5 ]) in
  let _ =
    Jkinds_lib.Infer.solve_program
      [ { Decl_parser.name = "DUMMY2"; arity = 0; rhs = t; abstract = false } ]
      ~max_iters:0
  in
  let k =
    Kind.max
      (Kind.set Kind.empty Kind.Var.a0
         (Modality.of_atom { Modality.ctor = "F"; index = 0 }))
      (Kind.apply
         (Modality.of_atom { Modality.ctor = "F"; index = 1 })
         (Kind.set Kind.empty 5 Modality.id))
  in
  assert (Kind.get k Kind.Var.a0 = Modality.of_atom { ctor = "F"; index = 0 });
  assert (Kind.get k 5 = Modality.of_atom { ctor = "F"; index = 1 });
  print_endline "✓ kindof tests passed"
