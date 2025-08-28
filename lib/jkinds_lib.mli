module Type_syntax = Type_syntax
module Modality = Modality
module Kind = Kind
module Type_parser : module type of Type_parser
module Decl_parser : module type of Decl_parser

module Global_counters : sig
  val inc : string -> unit
  val counters : unit -> (string * int) list
end

module Product_lattice : module type of Product_lattice
module Lattice_polynomial : module type of Lattice_polynomial
module Lattice_solver : module type of Lattice_solver
module Axis_lattice : module type of Axis_lattice
module Ldd : module type of Ldd
module Type_menhir_driver : module type of Type_menhir_driver

(* Expose minimal cyclic parse/desugar API from Type_parser *)
(* We keep extended mu API internal for now; Infer2 will import from Type_parser directly *)

module Infer2 : sig
  module VarLabel : sig
    type t = Atom of Modality.atom | TyVar of int | TyRec of int
  end

  type var_label = VarLabel.t
  type poly
  type var

  val to_poly : Type_syntax.t -> poly
  val to_poly_mu_raw : Type_parser.mu_raw -> poly
  val to_poly_decl_rhs : Decl_parser.decl_item -> poly
  val pp_poly : poly -> string

  val decompose_by_tyvars :
    arity:int -> poly -> poly * poly array * (var_label list * poly) list

  val solve_linear_for_program : Decl_parser.program -> unit
  val atom_state_lines_for_program : Decl_parser.program -> string list
  val pp_varlabel : var_label -> string
  val run_program : Decl_parser.program -> string
end

(* Classic run wrapper for normalized kinds *)
module Infer : sig
  include module type of Infer

  val run_program : Decl_parser.program -> max_iters:int -> string
end

module Infer4 : sig
  module RigidName : sig
    type t = Atom of Modality.atom | TyVar of int
  end

  type poly

  val to_poly : Type_syntax.t -> poly
  val to_poly_mu_raw : Type_parser.mu_raw -> poly
  val to_poly_decl_rhs : Decl_parser.decl_item -> poly
  val pp_poly : poly -> string

  val decompose_by_tyvars :
    arity:int -> poly -> poly * poly array * (RigidName.t list * poly) list

  val solve_linear_for_program : Decl_parser.program -> unit
  val atom_state_lines_for_program : Decl_parser.program -> string list
  val pp_varlabel : RigidName.t -> string
  val run_program : Decl_parser.program -> string
end

module Infer5 : sig
  val run_program : Decl_parser.program -> string
end

module Infer6 : sig
  val run_program : Decl_parser.program -> string
  val debug_constr : Decl_parser.program -> constr:string -> string
end
