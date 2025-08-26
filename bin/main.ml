let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

let () =
  let usage = "Usage: jkinds <typedef-file> [--max-iters N] [--bench N]" in
  let argc = Array.length Sys.argv in
  if argc < 2 then (
    prerr_endline usage;
    exit 2);
  let file = Sys.argv.(1) in
  let max_iters =
    let rec loop i acc =
      if i + 1 < argc && Sys.argv.(i) = "--max-iters" then
        int_of_string Sys.argv.(i + 1)
      else if i + 1 < argc then loop (i + 1) acc
      else acc
    in
    loop 2 10
  in
  let bench_runs =
    let rec loop i acc =
      if i + 1 < argc && Sys.argv.(i) = "--bench" then
        Some (int_of_string Sys.argv.(i + 1))
      else if i + 1 < argc then loop (i + 1) acc
      else acc
    in
    loop 2 None
  in
  let content = read_file file in
  let prog = Jkinds_lib.Decl_parser.parse_program_exn content in
  let _unused = max_iters in
  (* keep flag accepted to avoid breaking scripts *)
  match bench_runs with
  | Some n ->
    let t0 = Unix.gettimeofday () in
    for _i = 1 to n do
      ignore (Jkinds_lib.Infer5.run_program prog)
    done;
    let t1 = Unix.gettimeofday () in
    let total_ms = (t1 -. t0) *. 1000.0 in
    let avg_ms = total_ms /. float_of_int n in
    Printf.printf "Bench: Infer5 x %d -> total %.3f ms, avg %.3f ms\n" n
      total_ms avg_ms;
    (* Print global counters, sorted by name *)
    let items = Jkinds_lib.Global_counters.counters () in
    let items = List.sort (fun (a, _) (b, _) -> String.compare a b) items in
    List.iter (fun (k, v) -> Printf.printf "%s: %d\n" k v) items
  | None ->
    let t0 = Unix.gettimeofday () in
    let out2 = Jkinds_lib.Infer2.run_program prog in
    let t1 = Unix.gettimeofday () in
    let out4 = Jkinds_lib.Infer4.run_program prog in
    let t2 = Unix.gettimeofday () in
    let out5 = Jkinds_lib.Infer5.run_program prog in
    let t3 = Unix.gettimeofday () in
    let items = [ ("Infer2", out2); ("Infer4", out4); ("Infer5", out5) ] in
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
    List.iter
      (fun (text, labels) ->
        let header = String.concat " & " labels ^ " normalized kinds:" in
        Printf.printf "%s\n%s\n\n" header text)
      (List.rev !groups);
    let msf x = x *. 1000.0 in
    let infer2_ms = msf (t1 -. t0) in
    let infer4_ms = msf (t2 -. t1) in
    let infer5_ms = msf (t3 -. t2) in
    Printf.printf "Timing: Infer2: %.3f ms, Infer4: %.3f ms, Infer5: %.3f ms\n"
      infer2_ms infer4_ms infer5_ms
