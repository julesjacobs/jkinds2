

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
    let rec loop i acc = if i + 1 < argc && Sys.argv.(i) = "--max-iters" then int_of_string Sys.argv.(i + 1) else if i + 1 < argc then loop (i + 1) acc else acc in
    loop 2 10
  in
  let content = read_file file in
  let prog = Jkinds_lib.Decl_parser.parse_program_exn content in
  let out2 = Jkinds_lib.Infer2.run_program prog in
  let out4 = Jkinds_lib.Infer4.run_program prog in
  let outc = Jkinds_lib.Infer.run_program prog ~max_iters in
  let items = [ ("Infer2", out2); ("Infer4", out4); ("Classic", outc) ] in
  let groups = ref [] in
  let add_group label text =
    let rec aux acc =
      match acc with
      | [] -> [ (text, [ label ]) ]
      | (t, labs) :: rest when t = text -> (t, labs @ [ label ]) :: rest
      | g :: rest -> g :: aux rest
    in
    groups := aux !groups
  in
  List.iter (fun (label, text) -> add_group label text) items;
  (* Print groups in insertion order *)
  List.iter
    (fun (text, labels) ->
      let header = String.concat " & " labels ^ " normalized kinds:" in
      Printf.printf "%s\n%s\n\n" header text)
    (List.rev !groups)
