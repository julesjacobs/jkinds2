open Jkinds_lib

let assert_true msg b = if not b then failwith ("assert: " ^ msg)

module C = Product_lattice.Make(struct let axis_sizes = [|3;2|] end)
module V = struct type t = string let compare = String.compare end
module P = Lattice_polynomial.Make(C)(V)

let encode a b = C.encode ~levels:[|a;b|]

let gen_var () = match Random.int 4 with 0->"x" | 1->"y" | 2->"z" | _->"w"

let gen_vset () =
  let s = ref P.VarSet.empty in
  for _=0 to Random.int 2 do s := P.VarSet.add (gen_var ()) !s done;
  !s

let gen_coeff () = encode (Random.int 3) (Random.int 2)

let gen_term () = (gen_vset (), gen_coeff ())

let gen_poly () =
  let n = 1 + Random.int 3 in
  let ts = Array.init n (fun _ -> gen_term ()) |> Array.to_list in
  P.join (P.of_terms ts) P.bot

let () =
  Random.init 20250814;
  (* Residuation: (a\\b) <= x  <->  a <= b \/ x *)
  for _=1 to 400 do
    let a = gen_poly () and b = gen_poly () and x = gen_poly () in
    let lhs = P.leq (P.co_sub a b) x in
    let rhs = P.leq a (P.join b x) in
    assert_true "poly co_sub residuation" (lhs = rhs)
  done;
  (* Monotone in a, antitone in b *)
  for _=1 to 300 do
    let a1 = gen_poly () and a2 = gen_poly () and b = gen_poly () in
    if P.leq a1 a2 then assert_true "mono a" (P.leq (P.co_sub a1 b) (P.co_sub a2 b))
  done;
  for _=1 to 300 do
    let a = gen_poly () and b1 = gen_poly () and b2 = gen_poly () in
    if P.leq b1 b2 then assert_true "antitone b" (P.leq (P.co_sub a b2) (P.co_sub a b1))
  done;
  (* Specials *)
  for _=1 to 100 do
    let a = gen_poly () and b = gen_poly () in
    assert_true "a\\a=bot" (P.equal (P.co_sub a a) P.bot);
    assert_true "a\\bot=a" (P.equal (P.co_sub a P.bot) a);
    assert_true "bot\\b=bot" (P.equal (P.co_sub P.bot b) P.bot);
    assert_true "a\\top=bot" (P.equal (P.co_sub a P.top) P.bot)
  done;
  (* Composition: (a\\b)\\c = a\\(b \/ c) *)
  for _=1 to 300 do
    let a = gen_poly () and b = gen_poly () and c = gen_poly () in
    let left = P.co_sub (P.co_sub a b) c in
    let right = P.co_sub a (P.join b c) in
    assert_true "composition" (P.equal left right)
  done;
  print_endline "poly co_sub properties passed"


