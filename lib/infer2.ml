(* Convert Type_syntax.t into a lattice-solver polynomial where variables are
   either constructor atoms (like C.0, C.1, ...) or type variables ('a1, ...).

   Coefficients live in the concrete product lattice (Axis_lattice). *)

module VarLabel = struct
  type t = Atom of Modality.atom | TyVar of int

  let compare (a : t) (b : t) : int =
    match (a, b) with
    | Atom a1, Atom a2 -> (
      match String.compare a1.Modality.ctor a2.Modality.ctor with
      | 0 -> Int.compare a1.index a2.index
      | c -> c)
    | TyVar x, TyVar y -> Int.compare x y
    | Atom _, TyVar _ -> -1
    | TyVar _, Atom _ -> 1
end

module S = Lattice_solver.Make (Axis_lattice) (VarLabel)

module VarMap = Map.Make (struct
  type t = VarLabel.t

  let compare = VarLabel.compare
end)

let var_env : S.var VarMap.t ref = ref VarMap.empty

let get_var (lbl : VarLabel.t) : S.var =
  match VarMap.find_opt lbl !var_env with
  | Some v -> v
  | None ->
    let v = S.new_var lbl in
    var_env := VarMap.add lbl v !var_env;
    v

let const_levels (levels : int array) : S.poly =
  S.const (Axis_lattice.encode ~levels)

let rec to_poly (t : Type_syntax.t) : S.poly =
  match t with
  | Type_syntax.Unit -> S.const Axis_lattice.bot
  | Type_syntax.Pair (a, b) | Type_syntax.Sum (a, b) ->
    S.join (to_poly a) (to_poly b)
  | Type_syntax.Mod_annot (t', levels) ->
    S.meet (const_levels levels) (to_poly t')
  | Type_syntax.Mod_const levels -> const_levels levels
  | Type_syntax.Var v -> S.var (get_var (VarLabel.TyVar v))
  | Type_syntax.C (name, args) ->
    let base =
      S.var (get_var (VarLabel.Atom { Modality.ctor = name; index = 0 }))
    in
    let step (acc : S.poly) (arg : Type_syntax.t) (i : int) : S.poly =
      let atom_i = VarLabel.Atom { Modality.ctor = name; index = i } in
      let vi = S.var (get_var atom_i) in
      S.join acc (S.meet vi (to_poly arg))
    in
    List.mapi (fun i t -> (i + 1, t)) args
    |> List.fold_left (fun acc (i, t) -> step acc t i) base

(* Expose types for convenience to downstream callers. *)
type var_label = VarLabel.t
type poly = S.poly
type var = S.var

let pp_poly (p : S.poly) : string =
  let pp_coeff x =
    let levels = Axis_lattice.decode x |> Array.to_list in
    let parts = levels |> List.map string_of_int |> String.concat "," in
    Printf.sprintf "[%s]" parts
  in
  let pp_var = function
    | VarLabel.Atom a -> Printf.sprintf "%s.%d" a.Modality.ctor a.index
    | VarLabel.TyVar v -> Printf.sprintf "'a%d" v
  in
  S.pp ~pp_var ~pp_coeff p

(* Decompose a polynomial by type variables 'a1..'aarity. Returns base (key=[]),
   coefficients array of length [arity] where index i corresponds to 'a{i+1},
   and a list of mixed (non-linear) entries where key has length >= 2. *)
let decompose_by_tyvars ~(arity : int) (p : S.poly) :
    S.poly * S.poly array * (VarLabel.t list * S.poly) list =
  let universe =
    let rec loop i acc =
      if i > arity then List.rev acc else loop (i + 1) (VarLabel.TyVar i :: acc)
    in
    loop 1 []
  in
  let groups = S.decompose_by ~universe p in
  let base = ref S.bot in
  let coeffs = Array.init arity (fun _ -> S.bot) in
  let mixed = ref [] in
  let () =
    List.iter
      (fun (key, poly) ->
        match key with
        | [] -> base := S.join !base poly
        | [ VarLabel.TyVar i ] when i >= 1 && i <= arity ->
          coeffs.(i - 1) <- S.join coeffs.(i - 1) poly
        | _ -> mixed := (key, poly) :: !mixed)
      groups
  in
  (!base, coeffs, List.rev !mixed)
