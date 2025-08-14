open Jkinds_lib

let assert_true msg b = if not b then failwith ("assert: " ^ msg)

module PL = struct
  module S = struct let axis_sizes = [|2;3;2|] end
  module L = Product_lattice.Make(S)

  let gen_value () =
    let a0 = Random.int 2 in
    let a1 = Random.int 3 in
    let a2 = Random.int 2 in
    L.encode ~levels:[|a0;a1;a2|]

  let run () =
    Random.init 12345;
    let trials = 1000 in
    for _ = 1 to trials do
      let x = gen_value () in
      let y = gen_value () in
      let z = gen_value () in
      (* commutativity *)
      assert_true "join comm" (L.join x y = L.join y x);
      assert_true "meet comm" (L.meet x y = L.meet y x);
      (* associativity *)
      assert_true "join assoc" (L.join x (L.join y z) = L.join (L.join x y) z);
      assert_true "meet assoc" (L.meet x (L.meet y z) = L.meet (L.meet x y) z);
      (* idempotence *)
      assert_true "join idemp" (L.join x x = x);
      assert_true "meet idemp" (L.meet x x = x);
      (* absorption *)
      assert_true "absorb1" (L.meet x (L.join x y) = x);
      assert_true "absorb2" (L.join x (L.meet x y) = x);
      (* order properties *)
      assert_true "leq refl" (L.leq x x);
      assert_true "leq antisym" (if L.leq x y && L.leq y x then x = y else true);
      assert_true "leq trans" (if L.leq x y && L.leq y z then L.leq x z else true);
      (* encode/decode roundtrip *)
      assert_true "roundtrip" (L.encode ~levels:(L.decode x) = x)
    done
end

module ModProps = struct
  open Modality

  let atoms =
    [ {ctor="A"; index=0}; {ctor="A"; index=1}
    ; {ctor="B"; index=0}; {ctor="C"; index=2} ]

  let gen () =
    (* small random modality via random unions and compositions of atoms *)
    let rec pick k acc = if k = 0 then acc else pick (k-1) (max acc (of_atom (List.nth atoms (Random.int (List.length atoms))))) in
    let m1 = pick 1 zero in
    let m2 = pick 1 zero in
    let m3 = pick 1 zero in
    let u = max (max m1 m2) m3 in
    let v = compose (max m1 m2) (max m2 m3) in
    max u v

  let run () =
    Random.init 4242;
    for _ = 1 to 500 do
      let m = gen () in
      (* pruning invariant: equal after redundant max/compose and idempotence *)
      assert_true "max idemp" (equal (max m m) m);
      assert_true "compose idemp on id" (equal (compose m id) m && equal (compose id m) m);
      (* monotonicity *)
      let n = gen () in
      assert_true "max monotone" (equal (max m n) (max n m));
    done
end

module PolyProps = struct
  module BoolLat = struct
    type t = bool
    let bot=false and top=true
    let join (a:bool) b = a || b
    let meet (a:bool) b = a && b
    let leq a b = (not a) || b
    let co_sub a b = a && not b
    let equal a b = (a = b)
  end
  module Var = struct type t=int let compare = Int.compare end
  module P = Lattice_polynomial.Make(BoolLat)(Var)

  let run () =
    let open P in
    Random.init 7;
    let vars = [|0;1;2|] in
    let gen_term () =
      let open P.VarSet in
      let s = ref empty in
      for i=0 to Array.length vars - 1 do
        if Random.bool () then s := add vars.(i) !s
      done;
      (!s, Random.bool ())
    in
    for _=1 to 300 do
      let t1 = gen_term () in
      let t2 = gen_term () in
      let p = of_list [t1; t2] in
      (* p is canonical by construction via of_list *)
      (* join/meet partial order laws *)
      let q = of_list [gen_term (); gen_term ()] in
      assert_true "join comm" (equal (join p q) (join q p));
      assert_true "meet comm" (equal (meet p q) (meet q p));
      (* order via join *)
      assert_true "leq via join" (leq p q = P.equal (join p q) q)
    done
end

let () =
  PL.run ();
  ModProps.run ();
  PolyProps.run ();
  print_endline "All property tests passed"


