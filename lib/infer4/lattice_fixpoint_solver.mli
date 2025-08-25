(* A fixpoint-oriented solver for (in)equalities over multivariate lattice
   polynomials with an explicit separation between rigid variables and solver
   variables.

   Key ideas (from NOTES.md):
   - Only solve solver variables (each exactly once via LFP or GFP).
   - Rigid variables are named external symbols; they are never solved.
   - Force before use: substitute solved solver vars; treat unsolved solver vars
     as their paired rigid variable; public normalization returns a list of
     (lat * V.t list) with only rigid variable names.
   - LFPs are solved before any GFP (attempting GFP first should be rejected).
*)

module type LATTICE = Lattice_intf.LATTICE

module type ORDERED = sig
  type t

  val compare : t -> t -> int
  val to_string : t -> string
end

module Make (C : LATTICE) (V : ORDERED) : sig
  type lat = C.t

  (* Anonymous solver variables, each solved exactly once (lfp/gfp). *)
  type var

  (* Polynomial type over the lattice [lat], built from rigid variable names
     (V.t) and solver variables. *)
  type poly

  (* Create a fresh solver variable. *)
  val new_var : unit -> var

  (* Inject variables/constants into polynomials. [rigid] inserts a named rigid
     variable; [var] inserts a solver variable. *)
  val var : var -> poly
  val rigid : V.t -> poly
  val const : lat -> poly

  (* Lattice structure on polynomials *)
  val top : poly
  val bot : poly
  val join : poly -> poly -> poly
  val meet : poly -> poly -> poly

  (* Solve a least- or greatest-fixpoint equation for a solver variable. After
     solving, occurrences of the variable are substituted by its solution by an
     internal forcing pass and will be eliminated in [normalize]. *)
  val solve_lfp : var -> poly -> unit
  val enqueue_gfp : var -> poly -> unit
  (* Enter query phase: solves any pending GFPs and prevents further LFP/GFP. *)
  val enter_query_phase : unit -> unit

  (* Pure check and normalization under the current solutions. [leq a b] holds
     iff force(a) ≤ force(b). *)
  val leq : poly -> poly -> bool

  (* Normalize to a list of terms (coeff, vars), where [vars] are rigid names
     (sorted) and all solver variables have been eliminated via forcing. *)
  val normalize : poly -> (lat * V.t list) list

  (* Decompose/group by designated rigid variables' names in [universe]. Works
     on the forced form. *)
  val decompose_by : universe:V.t list -> poly -> (V.t list * poly) list

  (* Linear decomposition specialized from [decompose_by]. Works on the forced
     form. *)
  val decompose_linear :
    universe:V.t list ->
    poly ->
    poly * (V.t * poly) list * (V.t list * poly) list

  (* Pretty-print a polynomial with customizable printers. *)
  val pp :
    ?pp_var:(V.t -> string) -> ?pp_coeff:(lat -> string) -> poly -> string

  (* Configure pretty-printers used only for internal tracing/logging. If not
     set, logs print rigid vars as "_" and coefficients as "⊤". *)
  val set_log_printers :
    ?pp_var:(V.t -> string) -> ?pp_coeff:(lat -> string) -> unit -> unit
end
