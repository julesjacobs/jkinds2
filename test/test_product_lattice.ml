open Jkinds_lib
module Product_lattice = Product_lattice

module S = struct
  (* axes: 2-valued, 3-valued, 1-valued (0 bits), 2-valued *)
  let axis_sizes = [| 2; 3; 1; 2 |]
end

module L = Product_lattice.Make (S)

let assert_eq msg a b = if a <> b then failwith ("assert: " ^ msg)

let () =
  (* shape *)
  assert_eq "num_axes" L.num_axes 4;
  assert_eq "axis_bits(0)" L.axis_bits.(0) 1;
  assert_eq "axis_bits(1)" L.axis_bits.(1) 2;
  assert_eq "axis_bits(2)" L.axis_bits.(2) 0;
  assert_eq "axis_bits(3)" L.axis_bits.(3) 1;
  assert (L.total_bits <= Sys.int_size - 1);
  print_endline "✓ product_lattice shape ok";

  (* encode/decode roundtrip *)
  let lvls0 = [| 0; 0; 0; 0 |] in
  let v0 = L.encode ~levels:lvls0 in
  assert_eq "bot enc" v0 L.bot;
  assert (L.decode v0 = lvls0);

  let lvls1 = [| 1; 2; 0; 1 |] in
  let v1 = L.encode ~levels:lvls1 in
  assert (L.decode v1 = lvls1);
  print_endline "✓ encode/decode ok";

  (* axis getters/setters *)
  assert_eq "get_axis 0" (L.get_axis v1 ~axis:0) 1;
  assert_eq "get_axis 1" (L.get_axis v1 ~axis:1) 2;
  assert_eq "get_axis 2" (L.get_axis v1 ~axis:2) 0;
  assert_eq "get_axis 3" (L.get_axis v1 ~axis:3) 1;

  let v1' = L.set_axis v1 ~axis:1 ~level:1 in
  assert_eq "set_axis" (L.get_axis v1' ~axis:1) 1;
  print_endline "✓ axis accessors ok";

  (* lattice ops match componentwise max/min *)
  let a = L.encode ~levels:[| 0; 1; 0; 1 |] in
  let b = L.encode ~levels:[| 1; 0; 0; 0 |] in
  let j = L.join a b in
  let m = L.meet a b in
  assert (L.decode j = [| 1; 1; 0; 1 |]);
  assert (L.decode m = [| 0; 0; 0; 0 |]);
  assert (L.leq m j);
  assert (L.leq a j && L.leq b j);
  assert (L.leq m a && L.leq m b);
  print_endline "✓ join/meet/leq ok";

  (* top *)
  let top_levels = Array.mapi (fun _ n -> n - 1) L.axis_sizes in
  assert (L.decode L.top = top_levels);
  print_endline "✓ top/bot ok";

  (* co-Heyting subtraction: axiswise if a<=b then ⊥ else a *)
  let a = L.encode ~levels:[| 1; 2; 0; 1 |] in
  let b = L.encode ~levels:[| 0; 2; 0; 0 |] in
  let c = L.co_sub a b in
  (* axis 0: 1>0 keep 1; axis1: 2<=2 -> 0; axis2: 0<=0 -> 0; axis3: 1>0 keep
     1 *)
  assert (L.decode c = [| 1; 0; 0; 1 |]);
  let d = L.co_sub b a in
  (* axis0: 0<=1 ->0; axis1: 2<=2 ->0; axis3: 0<=1 ->0 *)
  assert (L.decode d = [| 0; 0; 0; 0 |]);
  print_endline "✓ co_sub ok";

  (* co_sub identities *)
  assert (L.co_sub a a = L.bot);
  assert (L.co_sub a L.top = L.bot);
  assert (L.co_sub L.bot a = L.bot);
  let top_b = L.co_sub L.top b in
  let expected_top_b =
    Array.mapi
      (fun i n -> if L.get_axis b ~axis:i = n - 1 then 0 else n - 1)
      L.axis_sizes
  in
  assert (L.decode top_b = expected_top_b);
  (* admissibility: a <= b \/ (a \ b) *)
  assert (L.leq a (L.join b (L.co_sub a b)));
  (* monotone in a, antitone in b *)
  let a' = L.encode ~levels:[| 1; 2; 0; 1 |] in
  let b' = L.encode ~levels:[| 1; 2; 0; 1 |] in
  assert (L.leq (L.co_sub a b) (L.co_sub a' b));
  assert (L.leq (L.co_sub a b') (L.co_sub a b));

  (* Exhaustive property check on a small shape [|2;3;2|] *)
  let module Sx = struct
    let axis_sizes = [| 2; 3; 2 |]
  end in
  let module X = Product_lattice.Make (Sx) in
  let domain =
    Array.to_list
      (Array.init
         (2 * 3 * 2)
         (fun idx ->
           let a0 = idx mod 2 in
           let a1 = idx / 2 mod 3 in
           let a2 = idx / 6 mod 2 in
           X.encode ~levels:[| a0; a1; a2 |]))
  in
  List.iter
    (fun va ->
      List.iter
        (fun vb ->
          let vc = X.co_sub va vb in
          let la = X.decode va in
          let lb = X.decode vb in
          let lc = X.decode vc in
          for i = 0 to Array.length la - 1 do
            let expected = if la.(i) <= lb.(i) then 0 else la.(i) in
            assert (lc.(i) = expected)
          done;
          (* Check a <= b \/ (a \ b) *)
          assert (X.leq va (X.join vb vc)))
        domain)
    domain;
  (* Residuation equivalence and distribution properties *)
  List.iter
    (fun va ->
      List.iter
        (fun vb ->
          let co = X.co_sub va vb in
          List.iter
            (fun vx ->
              let lhs = X.leq co vx in
              let rhs = X.leq va (X.join vb vx) in
              assert (lhs = rhs))
            domain;
          (* Distributions: (a \/ b) \\ c = (a \\ c) \/ (b \\ c) *)
          List.iter
            (fun vb2 ->
              let left = X.co_sub (X.join va vb2) vb in
              let right = X.join (X.co_sub va vb) (X.co_sub vb2 vb) in
              assert (left = right))
            domain;
          (* a \\ (b \/ c) = (a \\ b) /\ (a \\ c) *)
          List.iter
            (fun vc2 ->
              let left = X.co_sub va (X.join vb vc2) in
              let right = X.meet (X.co_sub va vb) (X.co_sub va vc2) in
              assert (left = right))
            domain)
        domain)
    domain;
  print_endline "✓ exhaustive co_sub properties ok";

  (* pretty print exact check *)
  let pp_str = L.pp ~axis_names:[| "A"; "B"; "C"; "D" |] v1 in
  assert_eq "pp" pp_str "[1,2,0,1]";
  print_endline "✓ pp ok";

  print_endline "All product_lattice tests passed"
