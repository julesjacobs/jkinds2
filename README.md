jkinds
======

This is a prototype for jkinds
Eventually we will have:
- Normalization
- Subsumption checking
- Rounding up to the nearest jkind without with-bounds

Before we get there, we need to set up the basics.

# The basics

We have the following types:

t : Type ::= 
    | C(t1, ..., tk)     -- type constructors with name C
    | a                  -- type variable
k : Kind ::= {a -> M1, b -> M2, ...}
  a kind is a map from type variables to modalities

A modality is an n-ary max of n-ary compositions of named modalities, which refer to a type constructor+argument position. For example C.1 refers to the first argument of C. Since modalities commute and are idempotent, we represent an n-ary composition as a set. So the modality itself is in the end represented as a set of sets.
Note, the identity is {{}}, the singleton set of the empty composition,
and zero is the empty set {}.

Operations on kinds:
- max(k1,k2) -- does max pointwise over l and the modalities
- M(k) where M is a modality, applies M to all components

We can compute a Kind for a Type:
kindof : Type -> Kind
    C(t1,...,tk) => max({a0 -> C.0},C.1(kindof(t1)), ..., C.k(kindof(tk))) 
        where "a0" is a global special type variable for administrative purposes
    a => {a -> id}



----------

## Notes.

join = ⊔ = max
meet = ⊓ = min


