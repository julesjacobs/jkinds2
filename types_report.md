# jkinds Types Report

Generated: 2025-09-01 19:08:47 UTC

## abstracts.types

```
type none() : [2,1]
type some('a1) : [2,1]
type leaf() : [2,1]
type node('a1,'a2) : [2,1]

type foo() : unit
type bar() : [0,1]
type baz() : [2,1] 

type one() : two() @@ [0,1]
type two() = one()

# Additional abstract examples
type maybe('a1) : none() + some('a1)
type wrap('a1) : 'a1 @@ [1,0]
type wrap2('a1) : [2,1] * ('a1 @@ [1,0])
type pairish('a1,'a2) : ('a1 * 'a2) @@ [1,0]
type treeA('a1) : leaf() + node('a1, treeA('a1))

# Mutually recursive abstracts
type H('a1) : F('a1) + 'a1
type F('a1) : H('a1) @@ [0,1]

# Test concrete
type G('a1) = [2,1] * 'a1
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
none: {0 ↦ none.0}
some: {0 ↦ some.0, 1 ↦ some.1}
leaf: {0 ↦ leaf.0}
node: {0 ↦ node.0, 1 ↦ node.1, 2 ↦ node.2}
foo: {0 ↦ ⊥}
bar: {0 ↦ [0,1] ⊓ bar.0}
baz: {0 ↦ baz.0}
one: {0 ↦ [0,1] ⊓ one.0}
two: {0 ↦ [0,1] ⊓ one.0}
maybe: {0 ↦ (maybe.0 ⊓ none.0) ⊔ (maybe.0 ⊓ some.0), 1 ↦ (maybe.1 ⊓ none.0) ⊔ (maybe.1 ⊓ some.0) ⊔ (maybe.1 ⊓ some.1)}
wrap: {0 ↦ ⊥, 1 ↦ [1,0] ⊓ wrap.1}
wrap2: {0 ↦ wrap2.0, 1 ↦ wrap2.1}
pairish: {0 ↦ ⊥, 1 ↦ [1,0] ⊓ pairish.1, 2 ↦ [1,0] ⊓ pairish.2}
treeA: {0 ↦ (leaf.0 ⊓ treeA.0) ⊔ (node.0 ⊓ treeA.0) ⊔ (node.2 ⊓ treeA.0), 1 ↦ (leaf.0 ⊓ treeA.1) ⊔ (node.0 ⊓ treeA.1) ⊔ (node.1 ⊓ treeA.1) ⊔ (node.2 ⊓ treeA.1)}
H: {0 ↦ [0,1] ⊓ F.0 ⊓ H.0, 1 ↦ H.1}
F: {0 ↦ [0,1] ⊓ F.0 ⊓ H.0, 1 ↦ [0,1] ⊓ F.1 ⊓ H.1}
G: {0 ↦ ⊤, 1 ↦ ⊥}

Timing: Infer2: 0.358 ms, Infer4: 0.382 ms, Infer5: 0.418 ms, Infer6: 0.166 ms
```

## benjamin.types

```
type t1('a1) : [2,1]
type t2('a1) : [2,1]

type foo1('a1) = t2(t1('a1)) * t1(unit)
type foo2('a1) = t1(t2('a1)) * t2(unit)

type bar('a1) = bar(t1('a1)) + bar(t2('a1)) + 'a1
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
t1: {0 ↦ t1.0, 1 ↦ t1.1}
t2: {0 ↦ t2.0, 1 ↦ t2.1}
foo1: {0 ↦ t1.0 ⊔ t2.0, 1 ↦ t1.1 ⊓ t2.1}
foo2: {0 ↦ t1.0 ⊔ t2.0, 1 ↦ t1.1 ⊓ t2.1}
bar: {0 ↦ t1.0 ⊔ t2.0, 1 ↦ ⊤}

Timing: Infer2: 0.138 ms, Infer4: 0.096 ms, Infer5: 0.147 ms, Infer6: 0.061 ms
```

## btree.types

```
type leaf('a1) : [2,1]
type node('a1,'a2) : [2,1]

type btree('a1) = (leaf('a1) + node(btree('a1), btree('a1)))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
leaf: {0 ↦ leaf.0, 1 ↦ leaf.1}
node: {0 ↦ node.0, 1 ↦ node.1, 2 ↦ node.2}
btree: {0 ↦ leaf.0 ⊔ node.0, 1 ↦ leaf.1}

Timing: Infer2: 0.080 ms, Infer4: 0.060 ms, Infer5: 0.078 ms, Infer6: 0.034 ms
```

## cn2.types

```
# Minimal repro for CN2 discrepancy

type A('a1) : B('a1) @@ [1,0] + 'a1
type B('a1) : C('a1) @@ [0,1]
type C('a1) : A('a1)

type K('a1,'a2) = A('a1) * (B('a2) + 'a1)
type M2('a1) = [0,1] * 'a1

type CN2('a1,'a2) = K(A('a1),'a2) + M2(B('a2))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
A: {0 ↦ ⊥, 1 ↦ A.1}
B: {0 ↦ ⊥, 1 ↦ [0,1] ⊓ A.1 ⊓ B.1 ⊓ C.1}
C: {0 ↦ ⊥, 1 ↦ A.1 ⊓ C.1}
K: {0 ↦ ⊥, 1 ↦ ⊤, 2 ↦ [0,1] ⊓ A.1 ⊓ B.1 ⊓ C.1}
M2: {0 ↦ [0,1], 1 ↦ [2,0]}
CN2: {0 ↦ [0,1], 1 ↦ [2,0] ⊓ A.1, 2 ↦ ⊥}

Timing: Infer2: 0.191 ms, Infer4: 0.148 ms, Infer5: 0.156 ms, Infer6: 0.066 ms
```

## cn2_min1.types

```
# Stepwise simplification of CN2 mismatch

type A('a1) : 'a1
type B('a1) : 'a1
type C('a1) : 'a1

type K('a1,'a2) = A('a1) * (B('a2) + 'a1)
type M2('a1) = [0,1] * 'a1

type CN2A('a1,'a2) = K(A('a1),'a2)
type CN2B('a1,'a2) = M2(B('a2))
type CN2('a1,'a2) = CN2A('a1,'a2) + CN2B('a1,'a2)
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
A: {0 ↦ ⊥, 1 ↦ A.1}
B: {0 ↦ ⊥, 1 ↦ B.1}
C: {0 ↦ ⊥, 1 ↦ C.1}
K: {0 ↦ ⊥, 1 ↦ ⊤, 2 ↦ B.1}
M2: {0 ↦ [0,1], 1 ↦ [2,0]}
CN2A: {0 ↦ ⊥, 1 ↦ A.1, 2 ↦ B.1}
CN2B: {0 ↦ [0,1], 1 ↦ ⊥, 2 ↦ [2,0] ⊓ B.1}
CN2: {0 ↦ [0,1], 1 ↦ [2,0] ⊓ A.1, 2 ↦ [2,0] ⊓ B.1}

Timing: Infer2: 0.182 ms, Infer4: 0.151 ms, Infer5: 0.148 ms, Infer6: 0.064 ms
```

## cn2_min2.types

```
# Simpler repro: remove K, just A('a1) + M2(B('a2))

type A('a1) : 'a1
type B('a1) : 'a1
type M2('a1) = [0,1] * 'a1

type CN('a1,'a2) = A('a1) + M2(B('a2))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
A: {0 ↦ ⊥, 1 ↦ A.1}
B: {0 ↦ ⊥, 1 ↦ B.1}
M2: {0 ↦ [0,1], 1 ↦ [2,0]}
CN: {0 ↦ [0,1], 1 ↦ [2,0] ⊓ A.1, 2 ↦ [2,0] ⊓ B.1}

Timing: Infer2: 0.082 ms, Infer4: 0.058 ms, Infer5: 0.057 ms, Infer6: 0.028 ms
```

## cn_chain.types

```
type A('a1) : 'a1
type B('a1) : 'a1
type M2('a1) = [0,1] * 'a1

type CN3('a1,'a2) = M2(A('a1) + B('a2))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
A: {0 ↦ ⊥, 1 ↦ A.1}
B: {0 ↦ ⊥, 1 ↦ B.1}
M2: {0 ↦ [0,1], 1 ↦ [2,0]}
CN3: {0 ↦ [0,1], 1 ↦ [2,0] ⊓ A.1, 2 ↦ [2,0] ⊓ B.1}

Timing: Infer2: 0.086 ms, Infer4: 0.064 ms, Infer5: 0.071 ms, Infer6: 0.034 ms
```

## cn_mix.types

```
type A('a1) : 'a1
type My('a1) = [0,1] * 'a1

type CNmix('a1,'a2) = A('a1) + My('a2)
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
A: {0 ↦ ⊥, 1 ↦ A.1}
My: {0 ↦ [0,1], 1 ↦ [2,0]}
CNmix: {0 ↦ [0,1], 1 ↦ [2,0] ⊓ A.1, 2 ↦ [2,0]}

Timing: Infer2: 0.056 ms, Infer4: 0.042 ms, Infer5: 0.045 ms, Infer6: 0.023 ms
```

## cn_novar.types

```
type A('a1) : 'a1
type M2('a1) = [0,1] * 'a1

type CN0('a1,'a2) = A('a1) + M2(unit)
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
A: {0 ↦ ⊥, 1 ↦ A.1}
M2: {0 ↦ [0,1], 1 ↦ [2,0]}
CN0: {0 ↦ [0,1], 1 ↦ [2,0] ⊓ A.1, 2 ↦ ⊥}

Timing: Infer2: 0.052 ms, Infer4: 0.038 ms, Infer5: 0.035 ms, Infer6: 0.022 ms
```

## cn_simple.types

```
type A('a1) = 'a1
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
A: {0 ↦ ⊥, 1 ↦ ⊤}

Timing: Infer2: 0.016 ms, Infer4: 0.011 ms, Infer5: 0.008 ms, Infer6: 0.014 ms
```

## cyclic.types

```
type L('a1) : mu 'b1. ('a1 + 'b1)

type Nested('a1) : mu 'b1. ((mu 'b2. ('b1 + 'b2)) + 'a1)

type Annot('a1) : mu 'b1. (('a1 @@ [1,0]) + ('b1 @@ [0,1]))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
L: {0 ↦ ⊥, 1 ↦ L.1}
Nested: {0 ↦ ⊥, 1 ↦ Nested.1}
Annot: {0 ↦ ⊥, 1 ↦ [1,0] ⊓ Annot.1}

Timing: Infer2: 0.052 ms, Infer4: 0.038 ms, Infer5: 0.035 ms, Infer6: 0.023 ms
```

## discrepancy_hunt.types

```
# Discrepancy hunt: stress cases for abstract/concrete, arities, and annotations

# 1) Simple abstract chain with alternating axes
type A('a1) : B('a1) @@ [1,0] + 'a1
type B('a1) : C('a1) @@ [0,1]
type C('a1) : A('a1)

# 2) Mutual recursion with two parameters and cross-position usage
type D('a1,'a2) : E('a2,'a1) + ('a1 @@ [1,0])
type E('a1,'a2) : (D('a1,'a2) @@ [0,1]) + ('a2 @@ [0,1])

# 3) Abstract self with duplicated parameter influence under different axes
type R('a1) : R('a1) @@ [1,0] + ('a1 @@ [0,1])

# 4) Concrete referencing abstract and mixing product/sum
type K('a1,'a2) = A('a1) * (B('a2) + 'a1)

# 5) Three-way mutual with arity mismatch and both axes
type U1('a1,'a2) : U2('a2) @@ [1,0] + ('a1 @@ [0,1])
type U2('a1) : U3('a1,'a1) @@ [0,1]
type U3('a1,'a2) : U1('a1,'a2) @@ [1,1]

# 6) Concrete two-step LFP chain with constants
type M1('a1) = [1,0] + M2('a1)
type M2('a1) = [0,1] * 'a1

# 7) Abstract depending on two concretes via different axes
type AC('a1) : M1('a1) @@ [0,1] + M2('a1) @@ [1,0]

# 8) Phantom parameter (not used on RHS)
type PH('a1,'a2) : 'a1

# 9) Crossed concrete/abstract mutual with ordering sensitivity
type X1('a1) = 'a1 + X2('a1)
type X2('a1) : X1('a1) @@ [0,1]

# 10) Abstract split through two auxiliaries with joins of axes
type S('a1) : T('a1) @@ [1,0] + U('a1) @@ [0,1]
type T('a1) : S('a1)
type U('a1) : S('a1) @@ [0,1]

# 11) Two-parameter abstract where both params feed same atom index
type Z1('a1,'a2) : ('a1 @@ [1,0]) + ('a2 @@ [1,0]) + Z1('a1,'a2) @@ [0,1]

# 12) Concrete that duplicates parameter through product, plus constant
type DUP('a1) = ('a1 * 'a1) + [1,1]

# 13) Abstract that funnels through concrete with swap
type SW('a1,'a2) : DUP('a2) @@ [0,1] + DUP('a1) @@ [1,0]

# 14) Nested applications of abstract constructors
type ND1('a1) : C(D('a1,'a1)) + 'a1
type ND2('a1) : (B(C('a1)) @@ [1,0]) + (A(D('a1,'a1)) @@ [0,1])
type ND3('a1,'a2) : (U3(B('a1), D('a2,'a2)) @@ [1,1]) + (X2(C('a1)) @@ [0,1])

# 15) Nested concretes composed with abstracts
type CN1('a1) = DUP(C('a1)) * M1(D('a1,'a1))
type CN2('a1,'a2) = K(A('a1),'a2) + M2(B('a2))

# 16) Deep nesting through S/T/U chain
type NestSTU('a1) : T(U(S('a1)))

# 17) Annotated nesting at multiple levels
type AnnNest('a1) : ((B(C('a1)) @@ [1,0]) @@ [0,1]) + ('a1 @@ [1,1])

# 18) Cross-nested mutual recursion
type RecNest1('a1) : RecNest2(C('a1)) + 'a1
type RecNest2('a1) : RecNest1(D('a1,'a1)) @@ [0,1]

# 19) Mixed two-parameter nesting with join against abstract
type MixNest('a1,'a2) : U3(B('a1), D('a2,'a2)) + (Z1(C('a1),'a2) @@ [1,0])

# 20) Zero-arity mutual recursion
type A0() : B0() @@ [1,0]
type B0() : A0()

# 21) Three-arity with permutations and mixed axes
type Z3('a1,'a2,'a3) : ('a2 @@ [1,0]) + ('a1 @@ [0,1]) + (Z3('a3,'a1,'a2) @@ [0,1])
type C3('a1,'a2,'a3) = Z3('a2,'a3,'a1)

# 22) Duplicate self references to test idempotence of joins
type DupSelf('a1) : S('a1) + (S('a1) @@ [0,1])

# 23) Polymorphic recursion within a single mutually recursive abstract group
# Nested self/group applications across the SCC
type PRF('a1) : ('a1 @@ [1,0]) + (PRD(PRF('a1)) @@ [0,1])
type PRE('a1) : (PRF(PRE('a1)) @@ [0,1]) + 'a1
type PRC('a1) : (PRD(PRC(PRE(PRC('a1)))) @@ [1,0]) + ('a1 @@ [0,1])
type PRD('a1) : PRC('a1) @@ [0,1]

# 24) Concrete polymorphic recursion across a mutual group
type PCX('a1) = PCY(PCX('a1)) + ('a1 @@ [1,0])
type PCY('a1) = 'a1 + (PCZ(PCX('a1)) @@ [0,1])
type PCZ('a1) = 'a1

# 25) Parameter-swapping polymorphic recursion (abstract)
# Fix: PRH is arity-2; pass both args, swapping via PRG and threading 'a2.
type PRG('a1,'a2) : (PRH(PRG('a2,'a1), 'a2) @@ [1,0]) + 'a1
type PRH('a1,'a2) : (PRG(PRH('a1,'a2),'a1) @@ [0,1]) + 'a2

# 26) Concrete–abstract mutual recursion (simple, 1-arity)
type CA('a1) : CC('a1) @@ [0,1] + 'a1
type CC('a1) = CA('a1) + ('a1 @@ [1,0])

# 27) Concrete–abstract mutual recursion with param swap (2-arity)
type CA2('a1,'a2) : CC2('a2,'a1) @@ [1,0] + ('a1 @@ [0,1])
type CC2('a1,'a2) = CA2('a1,'a2) + ('a2 @@ [1,0])

# 28) Zero-arity concrete–abstract mutual recursion
type CA0() : CC0() @@ [0,1]
type CC0() = CA0()

# 29) Three-node cycle with one concrete and two abstracts
type CAX('a1) : CBX('a1) + ('a1 @@ [0,1])
type CBX('a1) = CCX(CAX('a1)) + 'a1
type CCX('a1) : CAX('a1) @@ [1,0]
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
A: {0 ↦ ⊥, 1 ↦ A.1}
B: {0 ↦ ⊥, 1 ↦ [0,1] ⊓ A.1 ⊓ B.1 ⊓ C.1}
C: {0 ↦ ⊥, 1 ↦ A.1 ⊓ C.1}
D: {0 ↦ [0,1] ⊓ D.0 ⊓ E.0, 1 ↦ ([0,1] ⊓ D.1 ⊓ E.2) ⊔ ([1,0] ⊓ D.1), 2 ↦ [0,1] ⊓ D.1 ⊓ D.2 ⊓ E.1 ⊓ E.2}
E: {0 ↦ [0,1] ⊓ D.0 ⊓ E.0, 1 ↦ [0,1] ⊓ D.1 ⊓ E.1 ⊓ E.2, 2 ↦ [0,1] ⊓ E.2}
R: {0 ↦ [1,0] ⊓ R.0, 1 ↦ [1,1] ⊓ R.1}
K: {0 ↦ ⊥, 1 ↦ ⊤, 2 ↦ [0,1] ⊓ A.1 ⊓ B.1 ⊓ C.1}
U1: {0 ↦ ⊥, 1 ↦ [0,1] ⊓ U1.1, 2 ↦ ⊥}
U2: {0 ↦ ⊥, 1 ↦ [0,1] ⊓ U1.1 ⊓ U2.1 ⊓ U3.1}
U3: {0 ↦ ⊥, 1 ↦ [0,1] ⊓ U1.1 ⊓ U3.1, 2 ↦ ⊥}
M1: {0 ↦ [1,1], 1 ↦ [2,0]}
M2: {0 ↦ [0,1], 1 ↦ [2,0]}
AC: {0 ↦ [0,1] ⊓ AC.0, 1 ↦ [1,1] ⊓ AC.1}
PH: {0 ↦ ⊥, 1 ↦ PH.1, 2 ↦ ⊥}
X1: {0 ↦ [0,1] ⊓ X2.0, 1 ↦ ⊤}
X2: {0 ↦ [0,1] ⊓ X2.0, 1 ↦ [0,1] ⊓ X2.1}
S: {0 ↦ ([0,1] ⊓ S.0 ⊓ U.0) ⊔ ([1,0] ⊓ S.0 ⊓ T.0), 1 ↦ ([0,1] ⊓ S.1 ⊓ U.1) ⊔ ([1,0] ⊓ S.1 ⊓ T.1)}
T: {0 ↦ ([0,1] ⊓ S.0 ⊓ T.0 ⊓ U.0) ⊔ ([1,0] ⊓ S.0 ⊓ T.0), 1 ↦ ([0,1] ⊓ S.0 ⊓ T.1 ⊓ U.0) ⊔ ([0,1] ⊓ S.1 ⊓ T.1 ⊓ U.1) ⊔ ([1,0] ⊓ S.1 ⊓ T.1)}
U: {0 ↦ [0,1] ⊓ S.0 ⊓ U.0, 1 ↦ [0,1] ⊓ S.1 ⊓ U.1}
Z1: {0 ↦ [0,1] ⊓ Z1.0, 1 ↦ [1,1] ⊓ Z1.1, 2 ↦ [1,1] ⊓ Z1.2}
DUP: {0 ↦ [1,1], 1 ↦ [2,0]}
SW: {0 ↦ [1,1] ⊓ SW.0, 1 ↦ [1,1] ⊓ SW.1, 2 ↦ [1,1] ⊓ SW.2}
ND1: {0 ↦ [0,1] ⊓ A.1 ⊓ C.1 ⊓ D.0 ⊓ E.0 ⊓ ND1.0, 1 ↦ ND1.1}
ND2: {0 ↦ [0,1] ⊓ A.1 ⊓ D.0 ⊓ E.0 ⊓ ND2.0, 1 ↦ ([0,1] ⊓ A.1 ⊓ D.0 ⊓ E.0 ⊓ ND2.1) ⊔ ([0,1] ⊓ A.1 ⊓ D.1 ⊓ E.2 ⊓ ND2.1)}
ND3: {0 ↦ [0,1] ⊓ ND3.0 ⊓ X2.0, 1 ↦ ([0,1] ⊓ A.1 ⊓ B.1 ⊓ C.1 ⊓ ND3.1 ⊓ U1.1 ⊓ U3.1) ⊔ ([0,1] ⊓ A.1 ⊓ C.1 ⊓ ND3.1 ⊓ X2.1) ⊔ ([0,1] ⊓ ND3.1 ⊓ X2.0), 2 ↦ [0,1] ⊓ ND3.2 ⊓ X2.0}
CN1: {0 ↦ [1,1], 1 ↦ [2,0] ⊓ A.1 ⊓ C.1}
CN2: {0 ↦ [0,1], 1 ↦ [2,0] ⊓ A.1, 2 ↦ ⊥}
NestSTU: {0 ↦ ([0,1] ⊓ NestSTU.0 ⊓ S.0 ⊓ T.0 ⊓ U.0) ⊔ ([0,1] ⊓ NestSTU.0 ⊓ S.0 ⊓ T.1 ⊓ U.0) ⊔ ([1,0] ⊓ NestSTU.0 ⊓ S.0 ⊓ T.0), 1 ↦ ([0,1] ⊓ NestSTU.1 ⊓ S.0 ⊓ T.0 ⊓ U.0) ⊔ ([0,1] ⊓ NestSTU.1 ⊓ S.0 ⊓ T.1 ⊓ U.0) ⊔ ([0,1] ⊓ NestSTU.1 ⊓ S.1 ⊓ T.1 ⊓ U.1) ⊔ ([1,0] ⊓ NestSTU.1 ⊓ S.0 ⊓ T.0)}
AnnNest: {0 ↦ ⊥, 1 ↦ [1,1] ⊓ AnnNest.1}
RecNest1: {0 ↦ [0,1] ⊓ RecNest1.0 ⊓ RecNest2.0, 1 ↦ RecNest1.1}
RecNest2: {0 ↦ ([0,1] ⊓ D.0 ⊓ E.0 ⊓ RecNest1.1 ⊓ RecNest2.0) ⊔ ([0,1] ⊓ RecNest1.0 ⊓ RecNest2.0), 1 ↦ ([0,1] ⊓ D.0 ⊓ E.0 ⊓ RecNest1.1 ⊓ RecNest2.1) ⊔ ([0,1] ⊓ D.1 ⊓ E.2 ⊓ RecNest1.1 ⊓ RecNest2.1)}
MixNest: {0 ↦ ⊥, 1 ↦ ([0,1] ⊓ A.1 ⊓ B.1 ⊓ C.1 ⊓ MixNest.1 ⊓ U1.1 ⊓ U3.1) ⊔ ([1,0] ⊓ A.1 ⊓ C.1 ⊓ MixNest.1 ⊓ Z1.1), 2 ↦ [1,0] ⊓ MixNest.2 ⊓ Z1.2}
A0: {0 ↦ [1,0] ⊓ A0.0 ⊓ B0.0}
B0: {0 ↦ [1,0] ⊓ A0.0 ⊓ B0.0}
Z3: {0 ↦ [0,1] ⊓ Z3.0, 1 ↦ [0,1] ⊓ Z3.1, 2 ↦ ([0,1] ⊓ Z3.1 ⊓ Z3.2 ⊓ Z3.3) ⊔ ([1,0] ⊓ Z3.2), 3 ↦ [0,1] ⊓ Z3.1 ⊓ Z3.3}
C3: {0 ↦ [0,1] ⊓ Z3.0, 1 ↦ [0,1] ⊓ Z3.1 ⊓ Z3.3, 2 ↦ [0,1] ⊓ Z3.1, 3 ↦ ([0,1] ⊓ Z3.1 ⊓ Z3.2 ⊓ Z3.3) ⊔ ([1,0] ⊓ Z3.2)}
DupSelf: {0 ↦ ([0,1] ⊓ DupSelf.0 ⊓ S.0 ⊓ U.0) ⊔ ([1,0] ⊓ DupSelf.0 ⊓ S.0 ⊓ T.0), 1 ↦ ([0,1] ⊓ DupSelf.1 ⊓ S.0 ⊓ U.0) ⊔ ([0,1] ⊓ DupSelf.1 ⊓ S.1 ⊓ U.1) ⊔ ([1,0] ⊓ DupSelf.1 ⊓ S.0 ⊓ T.0) ⊔ ([1,0] ⊓ DupSelf.1 ⊓ S.1 ⊓ T.1)}
PRF: {0 ↦ [0,1] ⊓ PRC.1 ⊓ PRD.1 ⊓ PRF.0, 1 ↦ ([0,1] ⊓ PRC.1 ⊓ PRD.1 ⊓ PRF.1) ⊔ ([1,0] ⊓ PRF.1)}
PRE: {0 ↦ ([0,1] ⊓ PRC.1 ⊓ PRD.1 ⊓ PRE.0 ⊓ PRF.0) ⊔ ([0,1] ⊓ PRC.1 ⊓ PRD.1 ⊓ PRE.0 ⊓ PRF.1), 1 ↦ PRE.1}
PRC: {0 ↦ ⊥, 1 ↦ [0,1] ⊓ PRC.1}
PRD: {0 ↦ ⊥, 1 ↦ [0,1] ⊓ PRC.1 ⊓ PRD.1}
PCX: {0 ↦ ⊥, 1 ↦ [1,0]}
PCY: {0 ↦ ⊥, 1 ↦ ⊤}
PCZ: {0 ↦ ⊥, 1 ↦ ⊤}
PRG: {0 ↦ ⊥, 1 ↦ PRG.1, 2 ↦ [1,0] ⊓ PRG.2 ⊓ PRH.2}
PRH: {0 ↦ [0,1] ⊓ PRG.1 ⊓ PRH.0, 1 ↦ [0,1] ⊓ PRG.1 ⊓ PRH.1, 2 ↦ PRH.2}
CA: {0 ↦ [0,1] ⊓ CA.0, 1 ↦ CA.1}
CC: {0 ↦ [0,1] ⊓ CA.0, 1 ↦ CA.1 ⊔ [1,0]}
CA2: {0 ↦ [1,0] ⊓ CA2.0, 1 ↦ [1,1] ⊓ CA2.1, 2 ↦ [1,0] ⊓ CA2.1 ⊓ CA2.2}
CC2: {0 ↦ [1,0] ⊓ CA2.0, 1 ↦ [1,1] ⊓ CA2.1, 2 ↦ [1,0]}
CA0: {0 ↦ [0,1] ⊓ CA0.0}
CC0: {0 ↦ [0,1] ⊓ CA0.0}
CAX: {0 ↦ ([1,0] ⊓ CAX.0 ⊓ CCX.0) ⊔ ([1,0] ⊓ CAX.0 ⊓ CCX.1), 1 ↦ CAX.1}
CBX: {0 ↦ ([1,0] ⊓ CAX.0 ⊓ CCX.0) ⊔ ([1,0] ⊓ CAX.0 ⊓ CCX.1), 1 ↦ ⊤}
CCX: {0 ↦ [1,0] ⊓ CAX.0 ⊓ CCX.0, 1 ↦ ([1,0] ⊓ CAX.0 ⊓ CCX.1) ⊔ ([1,0] ⊓ CAX.1 ⊓ CCX.1)}

Timing: Infer2: 1.773 ms, Infer4: 1.405 ms, Infer5: 1.687 ms, Infer6: 1.001 ms
```

## easy.types

```
type A('a1) = unit + 'a1
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
A: {0 ↦ ⊥, 1 ↦ ⊤}

Timing: Infer2: 0.018 ms, Infer4: 0.011 ms, Infer5: 0.008 ms, Infer6: 0.009 ms
```

## experiments.types

```
type list('a1) : 'a1

type rose('a1) = list('a1 * rose(list('a1)))
type lily('a1) = list(lily(list('a1)))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
list: {0 ↦ ⊥, 1 ↦ list.1}
rose: {0 ↦ ⊥, 1 ↦ list.1}
lily: {0 ↦ ⊥, 1 ↦ ⊥}

Timing: Infer2: 0.076 ms, Infer4: 0.052 ms, Infer5: 0.059 ms, Infer6: 0.027 ms
```

## experiments2.types

```
type list('a1) : [0,1] * 'a1

type rose('a1) = list('a1 * rose(list('a1)))
type lily('a1) = list(lily(list('a1)))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
list: {0 ↦ [0,1] ⊓ list.0, 1 ↦ list.1}
rose: {0 ↦ [0,1] ⊓ list.0, 1 ↦ list.1}
lily: {0 ↦ [0,1] ⊓ list.0, 1 ↦ ⊥}

Timing: Infer2: 0.074 ms, Infer4: 0.057 ms, Infer5: 0.067 ms, Infer6: 0.035 ms
```

## fh.types

```
type H('a1) : F('a1) + 'a1
type F('a1) : H('a1) @@ [0,1]

# Harder mutual recursion with arities flipped and mixed annotations
type H2('a1,'a2) : F2('a2,'a1) + 'a1
type F2('a1,'a2) : (H2('a1,'a2) @@ [0,1]) + ('a2 @@ [1,0])

# Three-way abstract cycle with cross-axis influence
type X('a1) : Y('a1) @@ [1,0] + Z('a1) @@ [0,1]
type Y('a1) : X('a1) + 'a1
type Z('a1) : Y('a1) @@ [0,1]

# Mixed arity recursion using both axes and parameter duplication
type P('a1,'a2) : (Q('a1) @@ [1,1]) + ('a2 @@ [0,1])
type Q('a1) : P('a1,'a1) @@ [1,0] + 'a1

# Direct self-dependence under annotation
type M('a1) : M('a1) @@ [0,1] + 'a1

# Abstract pair factoring via two auxiliaries
type U('a1,'a2) : V('a1) + W('a2)
type V('a1) : U('a1,'a1) @@ [1,0]
type W('a1) : U('a1,'a1) @@ [0,1]

# Concretes depending on abstracts to test ordering vs. display
type C1('a1) = H('a1) * 'a1
type C2('a1) = F('a1) + 'a1 @@ [1,0]
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
H: {0 ↦ [0,1] ⊓ F.0 ⊓ H.0, 1 ↦ H.1}
F: {0 ↦ [0,1] ⊓ F.0 ⊓ H.0, 1 ↦ [0,1] ⊓ F.1 ⊓ H.1}
H2: {0 ↦ [0,1] ⊓ F2.0 ⊓ H2.0, 1 ↦ H2.1, 2 ↦ [0,1] ⊓ F2.1 ⊓ H2.1 ⊓ H2.2}
F2: {0 ↦ [0,1] ⊓ F2.0 ⊓ H2.0, 1 ↦ [0,1] ⊓ F2.1 ⊓ H2.1, 2 ↦ ([0,1] ⊓ F2.1 ⊓ F2.2 ⊓ H2.1 ⊓ H2.2) ⊔ ([1,0] ⊓ F2.2)}
X: {0 ↦ ([0,1] ⊓ X.0 ⊓ Y.0 ⊓ Z.0) ⊔ ([1,0] ⊓ X.0 ⊓ Y.0), 1 ↦ ([0,1] ⊓ X.1 ⊓ Y.1 ⊓ Z.1) ⊔ ([1,0] ⊓ X.1 ⊓ Y.1)}
Y: {0 ↦ ([0,1] ⊓ X.0 ⊓ Y.0 ⊓ Z.0) ⊔ ([1,0] ⊓ X.0 ⊓ Y.0), 1 ↦ Y.1}
Z: {0 ↦ [0,1] ⊓ X.0 ⊓ Y.0 ⊓ Z.0, 1 ↦ [0,1] ⊓ Y.1 ⊓ Z.1}
P: {0 ↦ [1,0] ⊓ P.0 ⊓ Q.0, 1 ↦ [1,1] ⊓ P.1 ⊓ Q.1, 2 ↦ [0,1] ⊓ P.2}
Q: {0 ↦ [1,0] ⊓ P.0 ⊓ Q.0, 1 ↦ Q.1}
M: {0 ↦ [0,1] ⊓ M.0, 1 ↦ M.1}
U: {0 ↦ ([0,1] ⊓ U.0 ⊓ W.0) ⊔ ([1,0] ⊓ U.0 ⊓ V.0), 1 ↦ [1,0] ⊓ U.1 ⊓ V.1, 2 ↦ [0,1] ⊓ U.2 ⊓ W.1}
V: {0 ↦ [1,0] ⊓ U.0 ⊓ V.0, 1 ↦ [1,0] ⊓ U.1 ⊓ V.1}
W: {0 ↦ [0,1] ⊓ U.0 ⊓ W.0, 1 ↦ [0,1] ⊓ U.2 ⊓ W.1}
C1: {0 ↦ [0,1] ⊓ F.0 ⊓ H.0, 1 ↦ ⊤}
C2: {0 ↦ [0,1] ⊓ F.0 ⊓ H.0, 1 ↦ ([0,1] ⊓ F.1 ⊓ H.1) ⊔ [1,0]}

Timing: Infer2: 0.528 ms, Infer4: 0.380 ms, Infer5: 0.319 ms, Infer6: 0.207 ms
```

## hunt.types

```
type A('a1) = [0,1] * 'a1

type B('a1,'a2) = 'a1 + A('a2)
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
A: {0 ↦ [0,1], 1 ↦ [2,0]}
B: {0 ↦ [0,1], 1 ↦ [2,0], 2 ↦ [2,0]}

Timing: Infer2: 0.044 ms, Infer4: 0.035 ms, Infer5: 0.034 ms, Infer6: 0.019 ms
```

## list_sum_pair.types

```
type list('a1) = unit + 'a1 * list('a1)
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
list: {0 ↦ ⊥, 1 ↦ ⊤}

Timing: Infer2: 0.021 ms, Infer4: 0.015 ms, Infer5: 0.014 ms, Infer6: 0.012 ms
```

## modalities.types

```
type Node('a1,'a2) : [2,1]
type F('a1) : [2,1]
type G('a1) : [2,1]
type nil() : [2,1]
type cons('a1,'a2) : [2,1]

type id_annot('a1) = 'a1 @@ [2,1]

type pair_annot('a1,'a2) = ('a1 * 'a2) @@ [1,0]

type nested('a1) = (('a1 @@ [2,1]) + unit) @@ [1,1]

type tree('a1) = Node('a1, tree('a1)) @@ [0,1]

type both('a1) = ('a1 @@ [2,0]) + ('a1 @@ [1,1])

type id_bot('a1) = 'a1 @@ [0,0]
type mix_sum('a1) = ('a1 @@ [1,0]) + ('a1 @@ [0,1])
type mix_pair('a1,'a2) = ('a1 @@ [1,0]) * ('a2 @@ [0,1])
type outer_vs_inner('a1) = ('a1 * 'a1) @@ [1,0]
type inner_vs_outer('a1) = ('a1 @@ [1,0]) * ('a1 @@ [1,0])
type list_ann('a1) = (unit + 'a1 * list_ann('a1)) @@ [1,0]
type two_axes('a1) = F('a1) @@ [1,0] + G('a1) @@ [0,1]
type deeply('a1) = ((F('a1) @@ [1,0]) + (G('a1) @@ [0,1])) @@ [1,1]

(* Compare inner vs outer annotation for lists *)
type list('a1) = unit + 'a1 * list('a1)
type list_inner('a1) = list('a1 @@ [1,0])
type list_outer('a1) = list('a1) @@ [1,0]

(* Alternative list using explicit nil/cons constructors *)
type list2('a1) = nil() + cons('a1, list2('a1))
type list2_inner('a1) = list2('a1 @@ [1,0])
type list2_outer('a1) = list2('a1) @@ [1,0]


(* New: bare modality constants as types (modals) *)
type modal_plus('a1) = [1,0] + 'a1
type modal_pair('a1) = [1,0] * 'a1
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
Node: {0 ↦ Node.0, 1 ↦ Node.1, 2 ↦ Node.2}
F: {0 ↦ F.0, 1 ↦ F.1}
G: {0 ↦ G.0, 1 ↦ G.1}
nil: {0 ↦ nil.0}
cons: {0 ↦ cons.0, 1 ↦ cons.1, 2 ↦ cons.2}
id_annot: {0 ↦ ⊥, 1 ↦ ⊤}
pair_annot: {0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [1,0]}
nested: {0 ↦ ⊥, 1 ↦ [1,1]}
tree: {0 ↦ [0,1] ⊓ Node.0, 1 ↦ [0,1] ⊓ Node.1}
both: {0 ↦ ⊥, 1 ↦ ⊤}
id_bot: {0 ↦ ⊥, 1 ↦ ⊥}
mix_sum: {0 ↦ ⊥, 1 ↦ [1,1]}
mix_pair: {0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [0,1]}
outer_vs_inner: {0 ↦ ⊥, 1 ↦ [1,0]}
inner_vs_outer: {0 ↦ ⊥, 1 ↦ [1,0]}
list_ann: {0 ↦ ⊥, 1 ↦ [1,0]}
two_axes: {0 ↦ ([0,1] ⊓ G.0) ⊔ ([1,0] ⊓ F.0), 1 ↦ ([0,1] ⊓ G.1) ⊔ ([1,0] ⊓ F.1)}
deeply: {0 ↦ ([0,1] ⊓ G.0) ⊔ ([1,0] ⊓ F.0), 1 ↦ ([0,1] ⊓ G.1) ⊔ ([1,0] ⊓ F.1)}
list: {0 ↦ ⊥, 1 ↦ ⊤}
list_inner: {0 ↦ ⊥, 1 ↦ [1,0]}
list_outer: {0 ↦ ⊥, 1 ↦ [1,0]}
list2: {0 ↦ cons.0 ⊔ nil.0, 1 ↦ cons.1}
list2_inner: {0 ↦ cons.0 ⊔ nil.0, 1 ↦ [1,0] ⊓ cons.1}
list2_outer: {0 ↦ ([1,0] ⊓ cons.0) ⊔ ([1,0] ⊓ nil.0), 1 ↦ [1,0] ⊓ cons.1}
modal_plus: {0 ↦ [1,0], 1 ↦ ⊤}
modal_pair: {0 ↦ [1,0], 1 ↦ ⊤}

Timing: Infer2: 0.456 ms, Infer4: 0.416 ms, Infer5: 0.391 ms, Infer6: 0.187 ms
```

## modals.types

```
type foo() = [1,0] * [0,1]
type bar() = foo() @@ [1,0]
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
foo: {0 ↦ [1,1]}
bar: {0 ↦ [1,0]}

Timing: Infer2: 0.024 ms, Infer4: 0.012 ms, Infer5: 0.014 ms, Infer6: 0.012 ms
```

## mutual.types

```
type cons('a1) : [2,1]

type oddlist('a1) = unit + cons('a1, evenlist('a1))
type evenlist('a1) = unit + cons('a1, oddlist('a1))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
cons: {0 ↦ cons.0, 1 ↦ cons.1}
oddlist: {0 ↦ cons.0, 1 ↦ cons.1}
evenlist: {0 ↦ cons.0, 1 ↦ cons.1}

Timing: Infer2: 0.110 ms, Infer4: 0.063 ms, Infer5: 0.044 ms, Infer6: 0.030 ms
```

## portable.types

```
type portable('a1) : [2,1]

type list('a1) = unit + 'a1 * list('a1)
type lily('a1) = list(portable('a1) * lily(list('a1)))
type tulip('a1) = unit + 'a1 * tulip(portable('a1 * 'a1))
type orchid('a1) = unit + portable('a1) * orchid('a1 * 'a1)
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
portable: {0 ↦ portable.0, 1 ↦ portable.1}
list: {0 ↦ ⊥, 1 ↦ ⊤}
lily: {0 ↦ portable.0, 1 ↦ portable.1}
tulip: {0 ↦ portable.0, 1 ↦ ⊤}
orchid: {0 ↦ portable.0, 1 ↦ portable.1}

Timing: Infer2: 0.122 ms, Infer4: 0.090 ms, Infer5: 0.115 ms, Infer6: 0.050 ms
```

## ref.types

```
type portended('a1) : [2,1]
type ref('a1) : [2,1]
type foo('a1) = portended(ref('a1))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
portended: {0 ↦ portended.0, 1 ↦ portended.1}
ref: {0 ↦ ref.0, 1 ↦ ref.1}
foo: {0 ↦ portended.0 ⊔ (portended.1 ⊓ ref.0), 1 ↦ portended.1 ⊓ ref.1}

Timing: Infer2: 0.057 ms, Infer4: 0.040 ms, Infer5: 0.040 ms, Infer6: 0.021 ms
```

## rose_sum_pair.types

```
type list('a1) = unit + 'a1 * list('a1)
type rose('a1) = list(rose(list('a1)))
type lily('a1) = list('a1 * lily(list('a1)))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
list: {0 ↦ ⊥, 1 ↦ ⊤}
rose: {0 ↦ ⊥, 1 ↦ ⊥}
lily: {0 ↦ ⊥, 1 ↦ ⊤}

Timing: Infer2: 0.065 ms, Infer4: 0.052 ms, Infer5: 0.055 ms, Infer6: 0.027 ms
```

## simple_example.types

```
type foo() : [2,1]

type bar() = foo()
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
foo: {0 ↦ foo.0}
bar: {0 ↦ foo.0}

Timing: Infer2: 0.022 ms, Infer4: 0.011 ms, Infer5: 0.011 ms, Infer6: 0.008 ms
```

## zipper.types

```
type cons('a1,'a2) : [2,1]
type down('a1,'a2) : [2,1]

type list('a1) = unit + cons('a1, list('a1))
type ctx('a1) = unit + down('a1, ctx('a1))
type zipper('a1) = (ctx('a1) * list('a1))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 normalized kinds:
cons: {0 ↦ cons.0, 1 ↦ cons.1, 2 ↦ cons.2}
down: {0 ↦ down.0, 1 ↦ down.1, 2 ↦ down.2}
list: {0 ↦ cons.0, 1 ↦ cons.1}
ctx: {0 ↦ down.0, 1 ↦ down.1}
zipper: {0 ↦ cons.0 ⊔ down.0, 1 ↦ cons.1 ⊔ down.1}

Timing: Infer2: 0.130 ms, Infer4: 0.094 ms, Infer5: 0.115 ms, Infer6: 0.053 ms
```
