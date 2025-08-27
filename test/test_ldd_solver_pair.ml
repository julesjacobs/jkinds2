open Jkinds_lib

module C = struct
  include Product_lattice.Make (struct
    let axis_sizes = [| 3; 2 |]
  end)

  let hash = Hashtbl.hash
end

let show_c (x : C.t) = C.pp x

module Name = struct
  type t = string

  let compare = String.compare
end

module LS = Lattice_solver.Make (C) (Name)
module L = Ldd.Make (C)
module PStr = Lattice_polynomial.Make (C) (Name)

(* Unused: kept for reference of LS pretty printer *)
let _pp_ls = LS.pp ~pp_var:(fun s -> s) ~pp_coeff:show_c
let var_names : (L.var, string) Hashtbl.t = Hashtbl.create 97

let assert_eq msg a b =
  if a <> b then failwith (Printf.sprintf "%s:\n  %s\n  %s" msg a b)

module Pair = struct
  type t = LS.poly * L.node

  let render_terms (ts : (C.t * string list) list) : string =
    (* Canonicalize via polynomial over string names to eliminate supersets *)
    let to_set vs =
      List.fold_left (fun acc v -> PStr.VarSet.add v acc) PStr.VarSet.empty vs
    in
    let poly = PStr.of_list (List.map (fun (c, vs) -> (to_set vs, c)) ts) in
    let ts =
      PStr.to_list poly |> List.map (fun (s, c) -> (c, PStr.VarSet.elements s))
    in
    if ts = [] then "⊥"
    else
      let bodies =
        List.map
          (fun (c, vs) ->
            let vs = List.sort String.compare vs in
            let body =
              if C.equal c C.top then
                if vs = [] then "⊤" else String.concat " ⊓ " vs
              else if vs = [] then show_c c
              else show_c c ^ " ⊓ " ^ String.concat " ⊓ " vs
            in
            let has_meet =
              ((not (C.equal c C.top)) && vs <> []) || List.length vs > 1
            in
            (body, has_meet))
          ts
      in
      let items = List.sort (fun (a, _) (b, _) -> String.compare a b) bodies in
      let n = List.length items in
      items
      |> List.map (fun (b, has_meet) ->
             if n > 1 && has_meet then "(" ^ b ^ ")" else b)
      |> String.concat " ⊔ "

  let to_string ((p, w) : t) : string * string =
    let sp = render_terms (LS.normalize p) in
    let sw_terms =
      L.to_named_terms
        ~pp_var:(fun v -> Hashtbl.find_opt var_names v)
        (L.normalize w)
    in
    let sw = render_terms sw_terms in
    (sp, sw)

  let check msg r =
    let sp, sw = to_string r in
    assert_eq msg sp sw

  let const c =
    let r = (LS.const c, L.const c) in
    check "const" r;
    r

  let rigid name =
    let v = LS.new_var name in
    let r = (LS.var v, L.rigid name) in
    check "rigid" r;
    r

  let join (p1, w1) (p2, w2) =
    let r = (LS.join p1 p2, L.join w1 w2) in
    check "join" r;
    r

  let meet (p1, w1) (p2, w2) =
    let r = (LS.meet p1 p2, L.meet w1 w2) in
    check "meet" r;
    r
end

let c a b = C.encode ~levels:[| a; b |]

let () =
  (* Seed rigids a,b,c,d and a few consts *)
  let pool : Pair.t list ref = ref [] in
  let seen : (string, unit) Hashtbl.t = Hashtbl.create 1024 in
  let add v =
    let sp, sw = Pair.to_string v in
    assert_eq "parity" sp sw;
    if not (Hashtbl.mem seen sp) then Hashtbl.add seen sp () else ();
    pool := v :: !pool
  in
  let a = Pair.rigid "a" in
  let b = Pair.rigid "b" in
  let c_ = Pair.rigid "c" in
  let d = Pair.rigid "d" in
  List.iter add
    [ a; b; c_; d; Pair.const (c 0 1); Pair.const (c 1 0); Pair.const (c 2 0) ];

  (* Unknowns we will solve: u0,u1,u2 *)
  (* Maintain a registry of unknowns; add more over time to avoid exhaustion. *)
  let unknowns : (string * (LS.var * L.var) * bool ref) list ref = ref [] in
  let fresh_id = ref 0 in
  let add_unknown () =
    let name = Printf.sprintf "u%d" !fresh_id in
    incr fresh_id;
    let vs = LS.new_var name in
    let vl = L.new_var () in
    Hashtbl.replace var_names vl name;
    unknowns := (name, (vs, vl), ref false) :: !unknowns;
    (* Expose unsolved variable in pool to participate in formulas. *)
    add (LS.var vs, L.var vl)
  in
  (* Seed a few unknowns *)
  add_unknown ();
  add_unknown ();
  add_unknown ();

  let get_nth lst n =
    let rec aux i = function
      | [] -> failwith "nth"
      | x :: xs -> if i = 0 then x else aux (i - 1) xs
    in
    aux n lst
  in
  let mk_rhs_including (vs : LS.var) (vl : L.var) : Pair.t =
    (* Build an rhs that includes the unknown on both branches in various
       ways. *)
    let vpair = (LS.var vs, L.var vl) in
    match Random.int 4 with
    | 0 ->
      Pair.join vpair (get_nth !pool (Random.int (max 1 (List.length !pool))))
    | 1 -> Pair.meet vpair (Pair.const (c (Random.int 3) (Random.int 2)))
    | 2 ->
      let t = get_nth !pool (Random.int (max 1 (List.length !pool))) in
      Pair.join
        (Pair.meet (Pair.const (c (Random.int 3) (Random.int 2))) vpair)
        t
    | _ ->
      let t1 = get_nth !pool (Random.int (max 1 (List.length !pool))) in
      let t2 = get_nth !pool (Random.int (max 1 (List.length !pool))) in
      Pair.join (Pair.meet vpair t1) t2
  in
  Random.init 7;
  let iters = 30000 in
  for _ = 1 to iters do
    (* 70% join/meet, 30% solve if available *)
    let len = List.length !pool in
    if len >= 2 && Random.int 10 < 7 then
      let i = Random.int len
      and j = Random.int len in
      let x = get_nth !pool i
      and y = get_nth !pool j in
      match Random.int 2 with
      | 0 -> add (Pair.join x y)
      | _ -> add (Pair.meet x y)
    else (
      (* occasionally add a fresh unknown to avoid exhausting solvables *)
      if Random.int 10 = 0 then add_unknown ();
      (* pick an unsolved unknown and a small rhs from pool *)
      let unsolved =
        List.filter (fun (_n, _vars, solved) -> not !solved) !unknowns
      in
      if unsolved <> [] then (
        let name, (vs, vl), solved_flag =
          get_nth unsolved (Random.int (List.length unsolved))
        in
        let rhs =
          if len > 0 && Random.int 2 = 0 then get_nth !pool (Random.int len)
          else mk_rhs_including vs vl
        in
        (* solve on both sides; LDD needs normalize for printing later *)
        let sp, sw = Pair.to_string rhs in
        ignore sp;
        ignore sw;
        let rp, rw = rhs in
        LS.solve_lfp vs rp;
        L.solve_lfp ~var:vl ~rhs_raw:rw;
        solved_flag := true;
        (* After solving, re-check parity of a few random terms, and add the
           solved var bound as a term *)
        let check_count = min 20 len in
        for _k = 1 to check_count do
          let t = get_nth !pool (Random.int len) in
          let sp, sw = Pair.to_string t in
          assert_eq ("post-solve parity (" ^ name ^ ")") sp sw
        done;
        let bound_pair = (LS.bound vs, L.normalize (L.var vl)) in
        add bound_pair)
      else ())
  done
