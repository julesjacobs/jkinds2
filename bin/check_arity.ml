open Jkinds_lib

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

module SS = Set.Make (String)
module SM = Map.Make (String)

let rec collect_uses acc (t : Type_parser.mu_raw) : (string * int) list =
  match t with
  | Type_parser.UnitR | VarR _ | ModConstR _ -> acc
  | ModAnnotR (u, _) -> collect_uses acc u
  | PairR (a, b) | SumR (a, b) -> collect_uses (collect_uses acc a) b
  | CR (name, args) ->
      let acc = (name, List.length args) :: acc in
      List.fold_left collect_uses acc args
  | MuR (_, body) -> collect_uses acc body
  | RecvarR _ -> acc

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "Usage: check_arity <file.types>";
    exit 2);
  let file = Sys.argv.(1) in
  let content = read_file file in
  let prog = Decl_parser.parse_program_exn content in
  let decl_arity =
    List.fold_left
      (fun m (it : Decl_parser.decl_item) -> SM.add it.name it.arity m)
      SM.empty prog
  in
  let uses =
    List.fold_left
      (fun acc (it : Decl_parser.decl_item) -> collect_uses acc it.rhs_mu_raw)
      [] prog
  in
  (* Aggregate uses by name -> set of arities *)
  let used_arities =
    List.fold_left
      (fun m (name, n) ->
        let set = match SM.find_opt name m with Some s -> s | None -> SS.empty in
        SM.add name (SS.add (string_of_int n) set) m)
      SM.empty uses
  in
  let mismatches_declared, mismatches_undeclared =
    SM.fold
      (fun name ar_set (md, mu) ->
        match SM.find_opt name decl_arity with
        | Some ar ->
            let ok = SS.mem (string_of_int ar) ar_set && SS.cardinal ar_set = 1 in
            if ok then (md, mu)
            else ((name, ar, SS.elements ar_set) :: md, mu)
        | None ->
            if SS.cardinal ar_set <= 1 then (md, mu)
            else (md, (name, SS.elements ar_set) :: mu))
      used_arities ([], [])
  in
  if mismatches_declared = [] && mismatches_undeclared = [] then (
    print_endline "OK: no arity mismatches (declared or undeclared)"; exit 0)
  else (
    if mismatches_declared <> [] then (
      print_endline "Arity mismatches against declarations:";
      List.iter
        (fun (name, ar, useds) ->
          Printf.printf "- %s: declared %d, used with {%s}\n" name ar (String.concat "," useds))
        mismatches_declared);
    if mismatches_undeclared <> [] then (
      print_endline "Arity inconsistencies among undeclared constructors:";
      List.iter
        (fun (name, useds) ->
          Printf.printf "- %s: used with {%s}\n" name (String.concat "," useds))
        mismatches_undeclared);
    exit 1)
