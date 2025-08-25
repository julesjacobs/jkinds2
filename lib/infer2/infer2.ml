(* Convert Type_syntax.t into a lattice-solver polynomial where variables are
   either constructor atoms (like C.0, C.1, ...) or type variables ('a1, ...).

   Coefficients live in the concrete product lattice (Axis_lattice). *)

module VarLabel = struct
  type t = Atom of Modality.atom | TyVar of int | TyRec of int

  let compare (a : t) (b : t) : int =
    match (a, b) with
    | Atom a1, Atom a2 -> (
      match String.compare a1.Modality.ctor a2.Modality.ctor with
      | 0 -> Int.compare a1.index a2.index
      | c -> c)
    | TyVar x, TyVar y -> Int.compare x y
    | TyRec x, TyRec y -> Int.compare x y
    | Atom _, (TyVar _ | TyRec _) -> -1
    | TyVar _, Atom _ -> 1
    | TyRec _, Atom _ -> 1
    | TyVar _, TyRec _ -> -1
    | TyRec _, TyVar _ -> 1
end

module S = Lattice_solver.Make (Axis_lattice) (VarLabel)

module VarMap = Map.Make (struct
  type t = VarLabel.t

  let compare = VarLabel.compare
end)

let var_env : S.var VarMap.t ref = ref VarMap.empty

let get_var (lbl : VarLabel.t) : S.var =
  match lbl with
  | VarLabel.TyRec _ -> S.new_var lbl (* do not cache recursive-type vars *)
  | _ -> (
    match VarMap.find_opt lbl !var_env with
    | Some v -> v
    | None ->
      let v = S.new_var lbl in
      var_env := VarMap.add lbl v !var_env;
      v)

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

(* ---- Cyclic translation support ---- *)
let to_poly_cyclic (root : Type_parser.cyclic) : S.poly =
  (* Allocate one solver variable per MuLink ref. *)
  let table : (Type_parser.cyclic ref, S.var) Hashtbl.t = Hashtbl.create 64 in

  let rec translate (n : Type_parser.cyclic) : S.poly =
    match n with
    | Type_parser.MuLink r -> translate_link r
    | Type_parser.CUnit -> S.const Axis_lattice.bot
    | Type_parser.CVar i -> S.var (get_var (VarLabel.TyVar i))
    | Type_parser.CMod_const lv -> const_levels lv
    | Type_parser.CMod_annot (t, lv) -> S.meet (const_levels lv) (translate t)
    | Type_parser.CPair (a, b) | Type_parser.CSum (a, b) ->
      S.join (translate a) (translate b)
    | Type_parser.CCtor (name, args) ->
      let base =
        S.var (get_var (VarLabel.Atom { Modality.ctor = name; index = 0 }))
      in
      let step acc (i, t) =
        let vi =
          S.var (get_var (VarLabel.Atom { Modality.ctor = name; index = i }))
        in
        S.join acc (S.meet vi (translate t))
      in
      List.mapi (fun i t -> (i + 1, t)) args |> List.fold_left step base
  and translate_link (r : Type_parser.cyclic ref) : S.poly =
    match Hashtbl.find_opt table r with
    | Some v -> S.var v
    | None ->
      let v = get_var (VarLabel.TyRec (Hashtbl.length table)) in
      Hashtbl.add table r v;
      let rhs = translate !r in
      S.solve_lfp v rhs;
      S.bound v
  in
  translate root

let to_poly_mu_raw (m : Type_parser.mu_raw) : S.poly =
  let cyc = Type_parser.to_cyclic m in
  to_poly_cyclic cyc

let to_poly_decl_rhs (it : Decl_parser.decl_item) : S.poly =
  (* Always compile from the canonical mu_raw form to ensure parity. *)
  to_poly_mu_raw it.rhs_mu_raw

let pp_poly (p : S.poly) : string =
  let pp_coeff x =
    let levels = Axis_lattice.decode x |> Array.to_list in
    let parts = levels |> List.map string_of_int |> String.concat "," in
    Printf.sprintf "[%s]" parts
  in
  let pp_var = function
    | VarLabel.Atom a -> Printf.sprintf "%s.%d" a.Modality.ctor a.index
    | VarLabel.TyVar v -> Printf.sprintf "'a%d" v
    | VarLabel.TyRec i -> Printf.sprintf "μb%d" i
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
  let base, singles, mixed = S.decompose_linear ~universe p in
  let coeffs = Array.init arity (fun _ -> S.bot) in
  List.iter
    (fun (v, poly) ->
      match v with
      | VarLabel.TyVar i when i >= 1 && i <= arity ->
        coeffs.(i - 1) <- S.join coeffs.(i - 1) poly
      | _ -> ())
    singles;
  (base, coeffs, mixed)

let pp_varlabel : VarLabel.t -> string = function
  | VarLabel.Atom a -> Printf.sprintf "%s.%d" a.Modality.ctor a.index
  | VarLabel.TyVar v -> Printf.sprintf "'a%d" v
  | VarLabel.TyRec i -> Printf.sprintf "μb%d" i

let pp_state_line (v : S.var) : string =
  let pp_coeff x =
    let levels = Axis_lattice.decode x |> Array.to_list in
    let parts = levels |> List.map string_of_int |> String.concat "," in
    Printf.sprintf "[%s]" parts
  in
  S.pp_state_line ~pp_var:pp_varlabel ~pp_coeff v

let get_atom_var ~(ctor : string) ~(index : int) : S.var =
  get_var (VarLabel.Atom { Modality.ctor; index })

type linear_decomp = {
  it : Decl_parser.decl_item;
  base : S.poly;
  coeffs : S.poly array;
}

let compute_linear_decomps (prog : Decl_parser.program) : linear_decomp list =
  let decomp_of_it (it : Decl_parser.decl_item) : linear_decomp =
    let p = to_poly_decl_rhs it in
    let base, coeffs, mixed = decompose_by_tyvars ~arity:it.arity p in
    (if mixed <> [] then
       let parts =
         mixed
         |> List.map (fun (k, poly) ->
                let key =
                  "{" ^ (k |> List.map pp_varlabel |> String.concat ", ") ^ "}"
                in
                Printf.sprintf "%s: %s" key (pp_poly poly))
         |> String.concat "; "
       in
       let msg =
         Printf.sprintf "infer2: non-linear terms for %s: %s" it.name parts
       in
       failwith msg);
    { it; base; coeffs }
  in
  List.map decomp_of_it prog

let solve_linear_for_program (prog : Decl_parser.program) : unit =
  let decs = compute_linear_decomps prog in
  (* Phase 1: solve concretes completely *)
  List.iter
    (fun { it; base; coeffs } ->
      if not it.abstract then (
        let v0 = get_atom_var ~ctor:it.name ~index:0 in
        S.solve_lfp v0 base;
        for i = 1 to it.arity do
          let vi = get_atom_var ~ctor:it.name ~index:i in
          S.solve_lfp vi coeffs.(i - 1)
        done))
    decs;
  (* Phase 2: assert abstracts as ≤ constraints, using base ∨ coeff_i *)
  List.iter
    (fun { it; base; coeffs } ->
      if it.abstract then (
        let v0 = get_atom_var ~ctor:it.name ~index:0 in
        S.assert_leq v0 base;
        for i = 1 to it.arity do
          let vi = get_atom_var ~ctor:it.name ~index:i in
          let rhs = S.join base coeffs.(i - 1) in
          S.assert_leq vi rhs
        done))
    decs

let atom_state_lines_for_program (prog : Decl_parser.program) : string list =
  let lines = ref [] in
  List.iter
    (fun (it : Decl_parser.decl_item) ->
      for i = 0 to it.arity do
        let v = get_atom_var ~ctor:it.name ~index:i in
        lines := pp_state_line v :: !lines
      done)
    prog;
  List.rev !lines

let atom_bound_poly ~(ctor : string) ~(index : int) : S.poly =
  let v = get_atom_var ~ctor ~index in
  S.bound v

let normalized_kind_for_decl (it : Decl_parser.decl_item) : (int * S.poly) list
    =
  let rec loop i acc =
    if i > it.arity then List.rev acc
    else
      let p = atom_bound_poly ~ctor:it.name ~index:i in
      let p' =
        if i = 0 then p
        else
          let u = [ VarLabel.Atom { Modality.ctor = it.name; index = 0 } ] in
          let groups = S.decompose_by ~universe:u p in
          (* Keep only terms not containing C.0 (key = []) *)
          groups
          |> List.filter (fun (k, _) -> k = [])
          |> List.fold_left (fun acc (_, poly) -> S.join acc poly) S.bot
      in
      loop (i + 1) ((i, p') :: acc)
  in
  loop 0 []

