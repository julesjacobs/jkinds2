open Jkinds_lib

let assert_true msg b = if not b then failwith ("assert failed: " ^ msg)

let assert_eq msg expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "assert_eq failed: %s" msg)

let assert_false msg b = if b then failwith ("assert_false failed: " ^ msg)

module Poly = struct
  module C = Product_lattice.Make (struct
    let axis_sizes = [| 3; 2 |]
  end)

  module V = struct
    type t = string

    let compare = String.compare
    let to_string s = s
  end

  module P = Lattice_polynomial.Make (C) (V)

  let axis_names = [| "a"; "b" |]
  let pp_coeff (x : C.t) = C.pp ~axis_names x
  let pp_poly p = P.pp p
  let encode a b = C.encode ~levels:[| a; b |]

  (* Common variable set builders *)
  let v vars = P.VarSet.of_list vars
  let vs s = P.VarSet.singleton s

  (* Common polynomial builders *)
  let x = P.var "x"
  let y = P.var "y"
  let z = P.var "z"
  let w = P.var "w"

  (* Random generators with configurable parameters *)
  let gen_var () =
    match Random.int 4 with 0 -> "x" | 1 -> "y" | 2 -> "z" | _ -> "w"

  let gen_vset ?(max_size = 2) () =
    let s = ref P.VarSet.empty in
    for _ = 0 to Random.int (max_size + 1) do
      s := P.VarSet.add (gen_var ()) !s
    done;
    !s

  let gen_coeff () = encode (Random.int 3) (Random.int 2)

  let gen_coeff_bounded a_max b_max =
    encode (Random.int a_max) (Random.int b_max)

  let gen_term () = (gen_vset (), gen_coeff ())

  let gen_poly ?(max_terms = 3) () : P.t =
    let n = 1 + Random.int max_terms in
    let ts = Array.init n (fun _ -> gen_term ()) |> Array.to_list in
    P.join (P.of_list ts) P.bot

  let gen_poly_n n : P.t =
    let ts = Array.init n (fun _ -> gen_term ()) |> Array.to_list in
    P.join (P.of_list ts) P.bot

  (* Test-side constructor using only public ops: build from list of (vars,
     coeff) *)
  let of_list (ts : (P.vars * C.t) list) : P.t =
    let term_poly (s : P.vars) (c : C.t) : P.t =
      let pv =
        P.VarSet.fold (fun v acc -> P.meet acc (P.var v)) s (P.const C.top)
      in
      P.meet pv (P.const c)
    in
    List.fold_left (fun acc (s, c) -> P.join acc (term_poly s c)) P.bot ts

  let gen_env () : V.t -> C.t =
    let x = gen_coeff ()
    and y = gen_coeff ()
    and z = gen_coeff ()
    and w = gen_coeff () in
    fun v ->
      match v with "x" -> x | "y" -> y | "z" -> z | "w" -> w | _ -> C.bot

  (* Build fixed environment for reproducible tests *)
  let fixed_env x_val y_val z_val w_val =
   fun v ->
    match v with
    | "x" -> x_val
    | "y" -> y_val
    | "z" -> z_val
    | "w" -> w_val
    | _ -> C.bot

  (* Evaluate a raw list of terms without going through P. *)
  let eval_raw (rho : V.t -> C.t) (ts : (P.vars * C.t) list) : C.t =
    let eval_set s = P.VarSet.fold (fun v acc -> C.meet acc (rho v)) s C.top in
    List.fold_left
      (fun acc (s, c) -> C.join acc (C.meet c (eval_set s)))
      C.bot ts

  (* Property test helpers *)
  let run_property_test name ~count test_fn =
    for i = 1 to count do
      try test_fn ()
      with e ->
        Printf.printf "Property test '%s' failed at iteration %d\n" name i;
        raise e
    done

  (* Test that two polynomials are equal, with better error reporting *)
  let assert_poly_eq msg p1 p2 =
    if not (P.equal p1 p2) then
      failwith
        (Printf.sprintf "%s:\n  Expected: %s\n  Got: %s" msg (pp_poly p1)
           (pp_poly p2))
end
