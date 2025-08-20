module Type_syntax = Type_syntax
module Modality = Modality
module Kind = Kind
module Infer : module type of Infer
module Type_parser : module type of Type_parser
module Decl_parser : module type of Decl_parser
module Product_lattice : module type of Product_lattice
module Lattice_polynomial : module type of Lattice_polynomial
module Lattice_solver : module type of Lattice_solver
module Axis_lattice : module type of Axis_lattice

module Infer2 : sig
  module VarLabel : sig
    type t = Atom of Modality.atom | TyVar of int
  end

  type var_label = VarLabel.t
  type poly
  type var

  val to_poly : Type_syntax.t -> poly
  val pp_poly : poly -> string

  val decompose_by_tyvars :
    arity:int -> poly -> poly * poly array * (var_label list * poly) list

  val solve_linear_for_program : Decl_parser.program -> unit
  val atom_state_lines_for_program : Decl_parser.program -> string list
  val normalized_kind_for_decl : Decl_parser.decl_item -> (int * poly) list
end
