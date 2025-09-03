(* UDD: wrapper module re-exporting LDD backend under a new name. *)

module type LATTICE = Ldd.LATTICE
module type ORDERED = Ldd.ORDERED

module Make = Ldd.Make
