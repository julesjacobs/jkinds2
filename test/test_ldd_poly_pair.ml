open Jkinds_lib

module C = struct
  include Product_lattice.Make (struct
    let axis_sizes = [| 3; 2 |]
  end)

  let hash = Hashtbl.hash
end

let show_c (x : C.t) = C.pp x

module V = struct
  type t = string

  let compare = String.compare

  (* let hash = Stdlib.Hashtbl.hash *)
  let to_string s = s
end

module P = Lattice_polynomial.Make (C) (V)
module L = Ldd.Make (C) (V)

let pp_poly = P.pp
let pp_ldd = L.pp

let assert_eq msg a b =
  if a <> b then
    failwith
      (Printf.sprintf "%s:\n  Expected: %s\n  Got: %s" msg (String.escaped a)
         (String.escaped b))

module Pair = struct
  type t = P.t * L.node

  let pp_pair ((p, w) : t) : string * string = (pp_poly p, pp_ldd w)

  let fail_op ~op ?a ?b ((p, w) : t) : 'a =
    let sp, sw = pp_pair (p, w) in
    let buf = Buffer.create 256 in
    Buffer.add_string buf (Printf.sprintf "op: %s\n" op);
    (match a with
    | None -> ()
    | Some (ap, aw) ->
      let sap, saw = pp_pair (ap, aw) in
      if sap <> saw then
        Buffer.add_string buf
          (Printf.sprintf "arg1 mismatch (BUG):\n  poly: %s\n  ldd:  %s\n" sap
             saw)
      else Buffer.add_string buf (Printf.sprintf "arg1: %s\n" sap));
    (match b with
    | None -> ()
    | Some (bp, bw) ->
      let sbp, sbw = pp_pair (bp, bw) in
      if sbp <> sbw then
        Buffer.add_string buf
          (Printf.sprintf "arg2 mismatch (BUG):\n  poly: %s\n  ldd:  %s\n" sbp
             sbw)
      else Buffer.add_string buf (Printf.sprintf "arg2: %s\n" sbp));
    Buffer.add_string buf (Printf.sprintf "result(poly): %s\n" sp);
    Buffer.add_string buf (Printf.sprintf "result(ldd):  %s\n" sw);
    ();
    (match a with
    | Some (_ap, aw) ->
      Buffer.add_string buf "arg1.debug:\n";
      Buffer.add_string buf (L.pp_debug aw)
    | None -> ());
    (match b with
    | Some (_bp, bw) ->
      Buffer.add_string buf "arg2.debug:\n";
      Buffer.add_string buf (L.pp_debug bw)
    | None -> ());
    Buffer.add_string buf "result.debug:\n";
    Buffer.add_string buf (L.pp_debug w);
    (* Persist full debug to a file for inspection. Use tmp dir to survive dune
       sandbox. *)
    let log = Buffer.contents buf in
    let tmp_dir = Filename.get_temp_dir_name () in
    let base = Printf.sprintf "ldd_pair_failure-%08x.log" (Random.bits ()) in
    let path = Filename.concat tmp_dir base in
    (try
       let oc = open_out_bin path in
       output_string oc log;
       close_out oc
     with _ -> ());
    failwith (Printf.sprintf "pp parity mismatch; wrote full debug to %s" path)

  let check ~op ?a ?b (r : t) : unit =
    let sp, sw = pp_pair r in
    if sp <> sw then fail_op ~op ?a ?b r

  let const (c : C.t) : t =
    let r = (P.const c, L.const c) in
    check ~op:(Printf.sprintf "const %s" (show_c c)) r;
    r

  let rigid (name : string) : t =
    let r = (P.var name, L.var (L.rigid name)) in
    check ~op:(Printf.sprintf "rigid %s" name) r;
    r

  let join ((p1, w1) : t) ((p2, w2) : t) : t =
    let r = (P.join p1 p2, L.join w1 w2) in
    check ~op:"join" ~a:(p1, w1) ~b:(p2, w2) r;
    r

  let meet ((p1, w1) : t) ((p2, w2) : t) : t =
    let r = (P.meet p1 p2, L.meet w1 w2) in
    check ~op:"meet" ~a:(p1, w1) ~b:(p2, w2) r;
    r

  let to_string ((p, w) : t) : string * string = (pp_poly p, pp_ldd w)
end

let c a b = C.encode ~levels:[| a; b |]

let () =
  (* Randomized closure under join from a fixed seed and iteration count. *)
  let pool : Pair.t list ref = ref [] in
  let seen : (string, unit) Hashtbl.t = Hashtbl.create 1024 in
  let add v =
    let sp, sw = Pair.to_string v in
    (* by construction parity should hold for anything we add *)
    assert_eq "add parity" sp sw;
    if not (Hashtbl.mem seen sp) then (
      Hashtbl.add seen sp ();
      pool := v :: !pool)
  in
  (* Create rigids with explicit sequencing to fix LDD variable order. *)
  let xr = Pair.rigid "a" in
  let yr = Pair.rigid "b" in
  let zr = Pair.rigid "c" in
  let dr = Pair.rigid "d" in
  List.iter add
    [
      xr;
      yr;
      zr;
      dr;
      Pair.const (c 0 1);
      Pair.const (c 1 0);
      Pair.const (c 2 0);
      Pair.const (c 1 1);
      Pair.const (c 2 1);
      Pair.const C.top;
      Pair.const C.bot;
    ];
  let get_nth lst n =
    let rec aux i = function
      | [] -> failwith "get_nth: out of bounds"
      | x :: xs -> if i = 0 then x else aux (i - 1) xs
    in
    aux n lst
  in
  Random.init 42;
  let iters = 10000 in
  for _ = 1 to iters do
    let len = List.length !pool in
    let i = Random.int len in
    let j = Random.int len in
    let a = get_nth !pool i in
    let b = get_nth !pool j in
    (* Exercise both join and meet; dedup by printed canonical form. *)
    add (Pair.join a b);
    add (Pair.meet a b)
  done
