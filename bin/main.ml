open Jkinds_lib

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

let () =
  let usage = "Usage: jkinds <typedef-file> [--max-iters N]" in
  let argc = Array.length Sys.argv in
  if argc < 2 then (
    prerr_endline usage;
    exit 2);
  let file = Sys.argv.(1) in
  let max_iters =
    let rec find_flag i =
      if i + 1 < argc then
        if Sys.argv.(i) = "--max-iters" then int_of_string Sys.argv.(i + 1)
        else find_flag (i + 1)
      else 10
    in
    find_flag 2
  in
  let content = read_file file in
  (* Decl_parser will populate mu_table when mu-typed RHS is detected. *)
  let prog = Jkinds_lib.Decl_parser.parse_program_exn content in
  let kinds_lfp = Jkinds_lib.Infer.solve_program prog ~max_iters in
  (* Infer2: print polynomial translation of each RHS *)
  print_endline "Infer2: RHS as polys:";
  List.iter
    (fun (it : Jkinds_lib.Decl_parser.decl_item) ->
      let p = Jkinds_lib.Infer2.to_poly_decl_rhs it in
      Printf.printf "%s: %s\n" it.name (Jkinds_lib.Infer2.pp_poly p))
    prog;
  (* Infer2: decompose by type variables and assert linearity *)
  print_endline "\nInfer2: linear decomposition (base + coeffs):";
  List.iter
    (fun (it : Jkinds_lib.Decl_parser.decl_item) ->
      let p = Jkinds_lib.Infer2.to_poly_decl_rhs it in
      let base, coeffs, mixed =
        Jkinds_lib.Infer2.decompose_by_tyvars ~arity:it.arity p
      in
      (if mixed <> [] then
         let key_str (lbls : Jkinds_lib.Infer2.var_label list) : string =
           let pp = function
             | Jkinds_lib.Infer2.VarLabel.TyVar i -> Printf.sprintf "'a%d" i
             | Jkinds_lib.Infer2.VarLabel.TyRec i -> Printf.sprintf "μb%d" i
             | Jkinds_lib.Infer2.VarLabel.Atom a ->
               Printf.sprintf "%s.%d" a.Jkinds_lib.Modality.ctor a.index
           in
           "{" ^ (lbls |> List.map pp |> String.concat ", ") ^ "}"
         in
         let parts =
           mixed
           |> List.map (fun (k, poly) ->
                  Printf.sprintf "%s: %s" (key_str k)
                    (Jkinds_lib.Infer2.pp_poly poly))
         in
         let msg =
           Printf.sprintf "non-linear terms for %s: %s" it.name
             (String.concat "; " parts)
         in
         failwith msg);
      Printf.printf "%s: base=%s" it.name (Jkinds_lib.Infer2.pp_poly base);
      Array.iteri
        (fun i ci ->
          Printf.printf ", 'a%d=%s" (i + 1) (Jkinds_lib.Infer2.pp_poly ci))
        coeffs;
      print_newline ())
    prog;
  (* Infer2: solve linear system over atoms and display solutions *)
  print_endline "\nInfer2: solving atoms:";
  Jkinds_lib.Infer2.solve_linear_for_program prog;
  Jkinds_lib.Infer2.atom_state_lines_for_program prog |> List.iter print_endline;
  print_endline "\nInfer2: Normalized kinds:";
  List.iter
    (fun (it : Jkinds_lib.Decl_parser.decl_item) ->
      let entries = Jkinds_lib.Infer2.normalized_kind_for_decl it in
      let body =
        entries
        |> List.map (fun (i, p) ->
               Printf.sprintf "%d ↦ %s" i (Jkinds_lib.Infer2.pp_poly p))
        |> String.concat ", "
      in
      Printf.printf "%s: {%s}\n" it.name body)
    prog;
  print_endline "\nNormalized kinds:";
  List.iter (fun (n, k) -> Printf.printf "%s: %s\n" n (Kind.pp k)) kinds_lfp;
  print_endline "\nCeil/Floor kinds:";
  List.iter
    (fun (n, k) ->
      let kc = Kind.ceil k in
      let kf = Kind.floor k in
      Printf.printf "%s: ceil=%s, floor=%s\n" n (Kind.pp kc) (Kind.pp kf))
    kinds_lfp;
  print_endline "\nLEQ relationships:";
  let names = List.map fst kinds_lfp in
  let lookup = kinds_lfp in
  List.iter
    (fun (n, k) ->
      let supersets =
        names
        |> List.filter (fun m ->
               if m = n then false
               else
                 let km = List.assoc m lookup in
                 Kind.leq k km)
      in
      if supersets <> [] then
        Printf.printf "%s <= %s\n" n (String.concat ", " supersets)
      else Printf.printf "%s <= (none)\n" n)
    kinds_lfp;
  ()
