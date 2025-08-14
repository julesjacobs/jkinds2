open Jkinds_lib

let assert_true msg b = if not b then failwith ("assert: " ^ msg)

module C = Product_lattice.Make(struct let axis_sizes = [|3;2|] end)
module V = struct type t = string let compare = String.compare end
module P = Lattice_polynomial.Make(C)(V)

let eval (rho : V.t -> C.t) (p : P.t) : C.t =
  let eval_set s =
    P.VarSet.fold (fun v acc -> C.meet acc (rho v)) s C.top
  in
  List.fold_left
    (fun acc (s,c) -> C.join acc (C.meet c (eval_set s)))
    C.bot
    (P.terms p)

let gen_coeff () =
  let a = Random.int 3 in
  let b = Random.int 2 in
  C.encode ~levels:[|a;b|]

let gen_var () = match Random.int 4 with 0->"x" | 1->"y" | 2->"z" | _->"w"

let gen_vset () =
  let s = ref P.VarSet.empty in
  for _=0 to Random.int 3 do
    s := P.VarSet.add (gen_var ()) !s
  done;
  !s

let gen_term () = (gen_vset (), gen_coeff ())

let gen_poly () =
  let n = 1 + Random.int 3 in
  let ts = Array.init n (fun _ -> gen_term ()) |> Array.to_list in
  P.join (P.of_terms ts) P.bot  (* canonicalize path *)

let gen_env () : (V.t -> C.t) =
  let x = gen_coeff () and y = gen_coeff () and z = gen_coeff () and w = gen_coeff () in
  fun v -> match v with "x"->x | "y"->y | "z"->z | "w"->w | _->C.bot

let () =
  Random.init 20250813;
  (* Semantic homomorphisms for join/meet *)
  for _=1 to 500 do
    let p = gen_poly () in
    let q = gen_poly () in
    let rho = gen_env () in
    let ej = eval rho (P.join p q) in
    let em = eval rho (P.meet p q) in
    assert_true "eval join" (ej = C.join (eval rho p) (eval rho q));
    assert_true "eval meet" (em = C.meet (eval rho p) (eval rho q));
    (* canonicalization preserves semantics *)
    assert_true "eval canonicalize" (eval rho p = eval rho (P.join p P.bot))
  done;
  (* Substitution semantics *)
  for _=1 to 300 do
    let p = gen_poly () in
    (* small substitution mapping some variables to small polys *)
    let subs = ref P.VarMap.empty in
    let k = Random.int 3 in
    for _=1 to k do
      subs := P.VarMap.add (gen_var ()) (gen_poly ()) !subs
    done;
    let rho = gen_env () in
    let rho' v = match P.VarMap.find_opt v !subs with
      | None -> rho v
      | Some pv -> eval rho pv
    in
    let lhs = eval rho (P.subst ~subs:!subs p) in
    let rhs = eval rho' p in
    assert_true "subst semantics" (lhs = rhs)
  done;
  (* Units for polynomials *)
  for _=1 to 200 do
    let p = gen_poly () in
    assert_true "join bot" (P.equal (P.join p P.bot) p);
    assert_true "meet top" (P.equal (P.meet p P.top) p);
    assert_true "join top" (P.equal (P.join p P.top) P.top);
    assert_true "meet bot" (P.equal (P.meet p P.bot) P.bot)
  done;
  print_endline "poly semantics tests passed"


