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
  if argc < 2 then (prerr_endline usage; exit 2);
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
  let prog = Jkinds_lib.Decl_parser.parse_program_exn content in
  let kinds_lfp = Jkinds_lib.Infer.solve_program prog ~max_iters in
  print_endline "\nNormalized kinds:";
  List.iter (fun (n,k) -> Printf.printf "%s: %s\n" n (Kind.pp k)) kinds_lfp;
  print_endline "\nCeil/Floor kinds:";
  List.iter (fun (n,k) ->
    let kc = Kind.ceil k in
    let kf = Kind.floor k in
    Printf.printf "%s: ceil=%s, floor=%s\n" n (Kind.pp kc) (Kind.pp kf)
  ) kinds_lfp;
  print_endline "\nLEQ relationships:";
  let names = List.map fst kinds_lfp in
  let lookup = kinds_lfp in
  List.iter (fun (n,k) ->
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
    else
      Printf.printf "%s <= (none)\n" n
  ) kinds_lfp;
  ()