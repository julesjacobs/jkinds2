(* Solver for (in)equalities over multivariate lattice polynomials
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   The solver interface is parameterized by: - C: the coefficient lattice,
   providing joins/meets and related ops - V: the ordered variable identifiers
   (e.g., strings) *)

module type LATTICE = Lattice_intf.LATTICE

module type ORDERED = sig
  type t

  val compare : t -> t -> int
end

module Make (C : LATTICE) (V : ORDERED) : sig
  type var
  type lat = C.t
  type poly

  (* Create a fresh solver variable labeled by an external identifier *)
  val new_var : V.t -> var

  (* Variables and constants are leaves of polys *)
  val var : var -> poly
  val const : lat -> poly

  (* Lattice structure on polys *)
  val top : poly
  val bot : poly
  val join : poly -> poly -> poly
  val meet : poly -> poly -> poly

  (* Assert a constraint var ≤ poly(var, other vars); must have a variable on
     the left *)
  val assert_leq : var -> poly -> unit

  (* Solve the least fixpoint for var = poly(var, other vars); variable is
     eliminated after this (can't assert_leq or solve_lfp again). Throws an
     exception if the equation is inconsistent with existing (in)equalities *)
  val solve_lfp : var -> poly -> unit

  (* Pure check: forall xs, poly1(xs) ≤ poly2(xs) *)
  val leq : poly -> poly -> bool

  (* Get the normal form for a poly under the current hypotheses *)
  val normalize : poly -> (lat * V.t list) list

  (* Debug/introspection helpers (useful for tests/printing) *)
  val is_eliminated : var -> bool
  val bound : var -> poly
  val name : var -> V.t

  (* Pretty-print a polynomial with customizable printers *)
  val pp :
    ?pp_var:(V.t -> string) -> ?pp_coeff:(lat -> string) -> poly -> string

  (* Pretty-print one state line for a variable: "x ≤ p" or "x = p" if
     eliminated *)
  val pp_state_line :
    ?pp_var:(V.t -> string) -> ?pp_coeff:(lat -> string) -> var -> string
end
