open Jkinds_lib

module Poly = struct
  module C = Product_lattice.Make(struct let axis_sizes = [|3;2|] end)
  module V = struct type t = string let compare = String.compare end
  module P = Lattice_polynomial.Make(C)(V)

  let axis_names = [| "a"; "b" |]
  let pp_coeff (x:C.t) = C.pp ~axis_names x

  let encode a b = C.encode ~levels:[|a;b|]

  let gen_var () = match Random.int 4 with 0->"x" | 1->"y" | 2->"z" | _->"w"

  let gen_vset () =
    let s = ref P.VarSet.empty in
    for _=0 to Random.int 2 do s := P.VarSet.add (gen_var ()) !s done;
    !s

  let gen_coeff () = encode (Random.int 3) (Random.int 2)

  let gen_term () = (gen_vset (), gen_coeff ())

  let gen_poly () : P.t =
    let n = 1 + Random.int 3 in
    let ts = Array.init n (fun _ -> gen_term ()) |> Array.to_list in
    P.join (P.of_list ts) P.bot

  let gen_poly_n n : P.t =
    let ts = Array.init n (fun _ -> gen_term ()) |> Array.to_list in
    P.join (P.of_list ts) P.bot

  (* Test-side constructor using only public ops: build from list of (vars, coeff) *)
  let of_list (ts:(P.vars * C.t) list) : P.t =
    let term_poly (s:P.vars) (c:C.t) : P.t =
      let pv = P.VarSet.fold (fun v acc -> P.meet acc (P.var v)) s (P.const C.top) in
      P.meet pv (P.const c)
    in
    List.fold_left (fun acc (s,c) -> P.join acc (term_poly s c)) P.bot ts

  let gen_env () : (V.t -> C.t) =
    let x = gen_coeff () and y = gen_coeff () and z = gen_coeff () and w = gen_coeff () in
    fun v -> match v with "x"->x | "y"->y | "z"->z | "w"->w | _->C.bot

  (* Evaluate a raw list of terms without going through P. *)
  let eval_raw (rho : V.t -> C.t) (ts:(P.vars * C.t) list) : C.t =
    let eval_set s = P.VarSet.fold (fun v acc -> C.meet acc (rho v)) s C.top in
    List.fold_left (fun acc (s,c) -> C.join acc (C.meet c (eval_set s))) C.bot ts
end


