[@@@warning "-32-27"]

module Make
    (Lat : Lattice_intf.LATTICE)
    (Ty : sig
      type t

      val compare_ty : t -> t -> int

      val to_string : t -> string
    end)
    (Constr : sig
      type t

      val compare : t -> t -> int

      val to_string : t -> string
    end) : sig
  type ty = Ty.t
  type constr = Constr.t
  type lat = Lat.t
  type kind

  type ops = {
    const : lat -> kind;
    join : kind list -> kind;
    modality : lat -> kind -> kind;
    constr : constr -> kind list -> kind;
    kind_of : ty -> kind;
  }

  type ckind = ops -> kind
  type constr_decl = { args : ty list; kind : ckind; abstract : bool }
  type env = { kind_of : ty -> ckind; lookup : constr -> constr_decl }
  type atom = { constr : constr; arg_index : int }

  val normalize : env -> ckind -> (lat * atom list) list
  val leq : env -> ckind -> ckind -> bool
  val round_up : env -> ckind -> lat
  val atom_terms : env -> constr -> int -> (lat * atom list) list
end = struct
  type ty = Ty.t
  type constr = Constr.t
  type lat = Lat.t
  type atom = { constr : constr; arg_index : int }

  module RigidName = struct
    type t = Atom of atom | Ty of ty
  
    let compare (a : t) (b : t) : int =
      match (a, b) with
      | Atom a1, Atom a2 -> (
          match Constr.compare a1.constr a2.constr with
          | 0 -> Int.compare a1.arg_index a2.arg_index
          | c -> c)
      | Ty x, Ty y -> Ty.compare_ty x y
      | Atom _, Ty _ -> -1
      | Ty _, Atom _ -> 1
  
    let to_string (a : t) : string =
      match a with
      | Atom a -> Printf.sprintf "%s.%d" (Constr.to_string a.constr) a.arg_index
      | Ty v -> Printf.sprintf "<%s>" (Ty.to_string v)

    let atomic constr arg_index = Atom { constr; arg_index }
  end
  
  module LSolver = Lattice_fixpoint_solver.Make (Lat) (RigidName)

  type kind = LSolver.poly

  type ops = {
    const : lat -> kind;
    join : kind list -> kind;
    modality : lat -> kind -> kind;
    constr : constr -> kind list -> kind;
    kind_of : ty -> kind;
  }

  type ckind = ops -> kind
  type constr_decl = { args : ty list; kind : ckind; abstract : bool }
  type env = { kind_of : ty -> ckind; lookup : constr -> constr_decl }

  (* Persist tables across calls for a single process run, so that multiple
     normalize/leq calls share the same constructor solutions. *)
  type state = {
    constr_to_coeffs : (constr, (LSolver.var * LSolver.var list)) Hashtbl.t;
  }
  let current_state : state option ref = ref None

  let make_ops (env : env) : ops =
    (* Define all the trivial ops *)
    let const l = LSolver.const l in
    let join ks = List.fold_left LSolver.join LSolver.bot ks in
    let modality l k = LSolver.meet (LSolver.const l) k in

    (* Create hash table mapping ty to kind for memoization *)
    let ty_to_kind = Hashtbl.create 64 in
    let constr_to_coeffs =
      match !current_state with
      | Some s -> s.constr_to_coeffs
      | None ->
        let c = Hashtbl.create 0 in
        current_state := Some { constr_to_coeffs = c };
        c
    in

    (* Define kind_of and constr ops *)
    let rec kind_of (t : ty) : kind =
      match Hashtbl.find_opt ty_to_kind t with
      | Some k -> k
      | None ->
        let v = LSolver.new_var () in
        Hashtbl.add ty_to_kind t (LSolver.var v);
        let rhs = env.kind_of t ops in
        LSolver.solve_lfp v rhs;
        rhs
    and constr c ks =
      let debug = match Sys.getenv_opt "JK_DEBUG" with Some s -> s <> "0" && s <> "false" && s <> "" | None -> false in
      let pp = LSolver.pp ~pp_var:RigidName.to_string ~pp_coeff:Lat.to_string in
      (* Todo: need to explore the constructor definition using lookup to
         contrain the coeffs and base *)
      let base, coeffs =
        match Hashtbl.find_opt constr_to_coeffs c with
        | Some base_and_coeffs -> base_and_coeffs
        | None ->
          (* Lookup declared arity to size coefficient vars *)
          let {args; kind; abstract} = env.lookup c in
          let arity = List.length args in
          let base = LSolver.new_var () in
          let coeffs =
            let rec gen i acc = if i = 0 then acc else gen (i-1) (LSolver.new_var () :: acc) in
            gen arity []
          in
          Hashtbl.add constr_to_coeffs c (base, coeffs);
          (* Recursively compute the kind of the body under a fresh Ty scope *)
          let snapshot : (ty * kind) list = Hashtbl.fold (fun k v acc -> (k,v) :: acc) ty_to_kind [] in
          Hashtbl.reset ty_to_kind;
          List.iter (fun ty -> Hashtbl.replace ty_to_kind ty (LSolver.rigid (RigidName.Ty ty))) args;
          (* Compute body kind *)
          let kind' = kind ops in
          (* Extract coeffs' from kind' *)
          let universe = List.map (fun ty -> RigidName.Ty ty) args in
          let (base',coeffs',rest) = LSolver.decompose_linear ~universe kind' in
          (match rest with
           | [] -> ()
           | _ ->
             let parts =
               List.map (fun (ks, p) ->
                 let key = ks |> List.map RigidName.to_string |> String.concat ", " in
                 Printf.sprintf "{%s}: _" key) rest
               |> String.concat "; "
             in
             failwith ("jkind_solver: non-linear terms: " ^ parts));
          (* Align singles to arity positions via ordered merge against universe *)
          let coeff_arr = Array.make (List.length args) (LSolver.bot) in
          if debug then (
            Printf.eprintf "[JK] constr %s singles:\n" (Constr.to_string c);
            List.iter (fun (v,p) -> Printf.eprintf "  key=%s, poly=%s\n" (RigidName.to_string v) (pp p)) coeffs');
          let rec fill i singles =
            match i, singles with
            | n, _ when n >= Array.length coeff_arr -> ()
            | n, (v,p) :: rest ->
                if RigidName.compare v (List.nth universe n) = 0 then (
                  coeff_arr.(n) <- LSolver.join coeff_arr.(n) p;
                  fill (n+1) rest)
                else (
                  (* advance universe slot if no matching single *)
                  fill (n+1) singles)
            | _n, [] -> ()
          in
          fill 0 coeffs';
          if debug then (
            let singles_s =
              let buf = Buffer.create 128 in
              Array.iteri (fun i p -> Buffer.add_string buf (Printf.sprintf "  coeff[%d] = %s\n" (i+1) (pp p))) coeff_arr;
              Buffer.contents buf
            in
            Printf.eprintf "[JK] constr %s: base'=%s\n%s%!" (Constr.to_string c) (pp base') singles_s);
          if abstract then begin
            (* We need to assert that kind' is less than or equal to the base *)
            LSolver.enqueue_gfp base (LSolver.meet (LSolver.rigid (RigidName.atomic c 0)) base');
            List.iteri (fun i coeff ->
              let rhs = LSolver.join base' coeff_arr.(i) in
              let bound = LSolver.meet (LSolver.rigid (RigidName.atomic c (i + 1))) rhs in
              LSolver.enqueue_gfp coeff bound
            ) coeffs;
          end else begin
            (* We need to solve for the coeffs *)
            LSolver.solve_lfp base base';
            List.iteri (fun i coeff -> LSolver.solve_lfp coeff coeff_arr.(i)) coeffs
          end;
          (* Restore Ty scope *)
          Hashtbl.reset ty_to_kind;
          List.iter (fun (k,v) -> Hashtbl.add ty_to_kind k v) snapshot;
          (base, coeffs)
      in
      (* Meet each arg with the corresponding coeff (truncate/pad as needed) *)
      let ks' =
        let rec pairs xs ys acc =
          match (xs, ys) with
          | x :: xs', y :: ys' -> pairs xs' ys' ((x, y) :: acc)
          | _ -> List.rev acc
        in
        pairs ks coeffs []
        |> List.map (fun (k, coeff) -> LSolver.meet k (LSolver.var coeff))
      in
      (* Join all the ks'' plus the base *)
      let k' = List.fold_left LSolver.join (LSolver.var base) ks' in
      (* Return that kind *)
      k'
    and ops = { const; join; modality; constr; kind_of } in
    ops

  (* let norm (env : env) (k : ckind) : kind =
     let ops = make_ops env in
     k ops *)

  let normalize (env : env) (k : ckind) : (lat * atom list) list =
    let ops = make_ops env in
    let p = k ops in
    let terms = LSolver.normalize p in
    let to_atoms (names : RigidName.t list) : atom list =
      names
      |> List.filter_map (function RigidName.Atom a -> Some a | _ -> None)
    in
    List.map (fun (coeff, names) -> (coeff, to_atoms names)) terms

  let leq (env : env) (k1 : ckind) (k2 : ckind) : bool =
    let ops = make_ops env in
    let k1' = k1 ops in
    let k2' = k2 ops in
    LSolver.leq k1' k2'

  let round_up (env : env) (k : ckind) : lat =
    let terms = normalize env k in
    let open Lat in
    let base_coeffs = terms |> List.filter_map (fun (c, atoms) -> if atoms = [] then Some c else None) in
    match base_coeffs with
    | [] -> bot
    | x :: xs -> List.fold_left join x xs

  let atom_terms (env : env) (c : constr) (idx : int) : (lat * atom list) list =
    (* Ensure constructor vars are initialized with correct arity. *)
    let { args; _ } = env.lookup c in
    let arity = List.length args in
    let base, coeffs =
      match !current_state with
      | None ->
          let _ = make_ops env in
          (* Fall through to lookup again *)
          (match !current_state with Some s -> s | None -> failwith "jkind_solver: state not initialized")
          |> fun s -> (match Hashtbl.find_opt s.constr_to_coeffs c with Some x -> x | None ->
                let ops = make_ops env in
                let args0 = List.init arity (fun _ -> ops.const Lat.bot) in
                ignore (ops.constr c args0);
                match !current_state with Some s2 -> (match Hashtbl.find_opt s2.constr_to_coeffs c with Some x -> x | None -> failwith "jkind_solver: failed to init constr") | None -> failwith "jkind_solver: no state")
      | Some s ->
          (match Hashtbl.find_opt s.constr_to_coeffs c with
          | Some x -> x
          | None ->
              let ops = make_ops env in
              let args0 = List.init arity (fun _ -> ops.const Lat.bot) in
              ignore (ops.constr c args0);
              match Hashtbl.find_opt s.constr_to_coeffs c with Some x -> x | None -> failwith "jkind_solver: failed to init constr")
    in
    (* Ensure expected arity of coeffs; if mismatch, reinit explicitly. *)
    let base, coeffs =
      let n = List.length coeffs in
      if n = arity then (base, coeffs)
      else
        let ops = make_ops env in
        let args0 = List.init arity (fun _ -> ops.const Lat.bot) in
        ignore (ops.constr c args0);
        match !current_state with
        | Some s -> (match Hashtbl.find_opt s.constr_to_coeffs c with Some x -> x | None -> (base, coeffs))
        | None -> (base, coeffs)
    in
    let v =
      if idx = 0 then base
      else
        let n = List.length coeffs in
        if idx - 1 < 0 || idx - 1 >= n then failwith (Printf.sprintf "atom_terms: index %d out of bounds (arity=%d, have=%d) for constr %s" idx arity n (Constr.to_string c))
        else List.nth coeffs (idx - 1)
    in
    let to_atoms names =
      names |> List.filter_map (function RigidName.Atom a -> Some a | _ -> None)
    in
    let poly =
      if idx = 0 then LSolver.var v
      else
        let groups = LSolver.decompose_by ~universe:[RigidName.atomic c 0] (LSolver.var v) in
        let empty_groups = groups |> List.filter (fun (k, _) -> k = []) |> List.map snd in
        List.fold_left LSolver.join LSolver.bot empty_groups
    in
    LSolver.normalize poly |> List.map (fun (c0, names) -> (c0, to_atoms names))
end
