module type LATTICE = sig
  type t

  val bot : t
  val top : t
  val join : t -> t -> t
  val meet : t -> t -> t
  val co_sub : t -> t -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val hash : t -> int
end

module type ORDERED = sig
  type t

  val compare : t -> t -> int
  val to_string : t -> string
end

module Make (C : LATTICE) (V : ORDERED) : sig
  type node
  type var

  (* Constructors *)
  val bot : node
  val top : node
  val const : C.t -> node
  val rigid : V.t -> var
  val new_var : unit -> var
  val var : var -> node

  (* Boolean algebra over nodes *)
  val join : node -> node -> node
  val meet : node -> node -> node
  val sub_subsets : node -> node -> node

  (* Solving interface *)
  val solve_lfp : var -> node -> unit
  val solve_gfp : var -> node -> unit
  val enqueue_lfp : var -> node -> unit
  val enqueue_gfp : var -> node -> unit
  val solve_pending : unit -> unit

  (* Linear decomposition/composition helpers *)
  val decompose_linear : universe:var list -> node -> node * node list

  (* Normalization and inspection *)
  val normalize : node -> node
  val to_list : node -> (C.t * V.t list) list
  val to_named_terms : node -> (C.t * string list) list
  val to_named_terms_with : (var -> string) -> node -> (C.t * string list) list

  (* Pretty printers and checks *)
  val pp : node -> string
  val pp_debug : node -> string
  val check_var_order : node -> bool
end
