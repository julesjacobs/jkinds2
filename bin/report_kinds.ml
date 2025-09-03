open Jkinds_lib

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

module TyM = struct
  type t = Type_parser.cyclic

  let compare t1 t2 = Int.compare t1.Type_parser.id t2.Type_parser.id
  let to_string (t : t) : string = Type_parser.pp_cyclic t
end

module ConstrM = struct
  type t = string

  let compare = String.compare
  let to_string (s : t) : string = s
end

module S6 = Infer6
module JK6 = Ldd_jkind_solver.Make (Axis_lattice) (TyM) (ConstrM)
module JK8 = Udd_jkind_solver.Make (Axis_lattice) (TyM) (ConstrM)

let env6_of_program (prog : Decl_parser.program) : JK6.env =
  let table =
    List.fold_left
      (fun acc (it : Decl_parser.decl_item) -> (it.name, it) :: acc)
      [] prog
  in
  let lookup (name : string) : JK6.constr_decl =
    match List.assoc_opt name table with
    | None -> failwith ("report: unknown constructor " ^ name)
    | Some it ->
      let args = it.params in
      let kind : JK6.ckind = fun ops -> ops.kind_of it.rhs_cyclic in
      JK6.Ty { args; kind; abstract = it.abstract }
  in
  { JK6.kind_of = (fun c ops -> ops.kind_of c); lookup }

let env8_of_program (prog : Decl_parser.program) : JK8.env =
  let table =
    List.fold_left
      (fun acc (it : Decl_parser.decl_item) -> (it.name, it) :: acc)
      [] prog
  in
  let lookup (name : string) : JK8.constr_decl =
    match List.assoc_opt name table with
    | None -> failwith ("report: unknown constructor " ^ name)
    | Some it ->
      let args = it.params in
      let kind : JK8.ckind = fun ops -> ops.kind_of it.rhs_cyclic in
      JK8.Ty { args; kind; abstract = it.abstract }
  in
  { JK8.kind_of = (fun c ops -> ops.kind_of c); lookup }

let ckind_for_decl (name : string) (params : Type_parser.cyclic list)
    (ops : JK6.ops) : JK6.kind =
  let args = List.map ops.rigid params in
  ops.constr name args

let main () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "Usage: report_kinds <typedef-file>";
    exit 2)
  else
    let file = Sys.argv.(1) in
    let content = read_file file in
    let prog = Decl_parser.parse_program_exn content in
    let env6 = env6_of_program prog in
    let env8 = env8_of_program prog in
    let s6 = JK6.make_solver env6 in
    let s8 = JK8.make_solver env8 in
    let decls = prog in
    let _names = List.map (fun (d : Decl_parser.decl_item) -> d.name) decls in
    let ckinds6 =
      List.map
        (fun (d : Decl_parser.decl_item) ->
          (d.name, fun (ops : JK6.ops) -> ckind_for_decl d.name d.params ops))
        decls
    in
    let ckinds8 =
      List.map
        (fun (d : Decl_parser.decl_item) ->
          ( d.name,
            fun (ops : JK8.ops) ->
              let args = List.map ops.rigid d.params in
              ops.constr d.name args ))
        decls
    in
    let leq_matrix make_leq ckinds =
      List.map
        (fun (ni, _ki) ->
          let leqs =
            ckinds
            |> List.filter (fun (nj, _kj) ->
                   try make_leq nj ni with _ -> false)
            |> List.map fst
          in
          (ni, leqs))
        ckinds
    in
    let make_leq6 nj ni =
      let ki = List.assoc ni ckinds6 in
      let kj = List.assoc nj ckinds6 in
      JK6.leq s6 kj ki
    in
    let make_leq8 nj ni =
      let ki = List.assoc ni ckinds8 in
      let kj = List.assoc nj ckinds8 in
      JK8.leq s8 kj ki
    in
    let m6 = leq_matrix make_leq6 ckinds6 in
    let m8 = leq_matrix make_leq8 ckinds8 in
    let roundups6 =
      List.map
        (fun (n, k) -> (n, JK6.round_up s6 k |> Axis_lattice.to_string))
        ckinds6
    in
    let roundups8 =
      List.map
        (fun (n, k) -> (n, JK8.round_up s8 k |> Axis_lattice.to_string))
        ckinds8
    in
    (* Prepare text blocks *)
    let pp_pairs_to_string xs =
      let b = Buffer.create 256 in
      List.iter
        (fun (n, lst) ->
          let body = String.concat ", " lst in
          Buffer.add_string b (Printf.sprintf "%s <=: [%s]\n" n body))
        xs;
      Buffer.contents b
    in
    let pp_roundups_to_string xs =
      let b = Buffer.create 256 in
      List.iter
        (fun (n, s) -> Buffer.add_string b (Printf.sprintf "%s: %s\n" n s))
        xs;
      Buffer.contents b
    in
    let leq6_s = pp_pairs_to_string m6 in
    let leq8_s = pp_pairs_to_string m8 in
    let ru6_s = pp_roundups_to_string roundups6 in
    let ru8_s = pp_roundups_to_string roundups8 in

    (* Consolidated printing: if 6 and 8 match, print once with combined
       header *)
    if String.equal leq6_s leq8_s then
      Printf.printf "LEQ (Infer6 & Infer8)\n%s" leq6_s
    else (
      Printf.printf "LEQ (Infer6)\n%s" leq6_s;
      Printf.printf "LEQ (Infer8)\n%s" leq8_s);
    if String.equal ru6_s ru8_s then
      Printf.printf "ROUND_UP (Infer6 & Infer8)\n%s" ru6_s
    else (
      Printf.printf "ROUND_UP (Infer6)\n%s" ru6_s;
      Printf.printf "ROUND_UP (Infer8)\n%s" ru8_s)

let () = main ()
