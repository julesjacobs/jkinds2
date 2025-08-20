# jkinds Types Report

Generated: 2025-08-20 21:43:45 UTC

## abstracts.types

```
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
Infer2: RHS as polys:
foo: ⊥
bar: [0,1]
baz: ⊤
one: ([0,1] ⊓ two.0)
two: one.0
maybe: (some.0 ⊔ none.0 ⊔ (some.1 ⊓ 'a1))
wrap: ([1,0] ⊓ 'a1)
wrap2: ⊤
pairish: (([1,0] ⊓ 'a1) ⊔ ([1,0] ⊓ 'a2))
treeA: (node.0 ⊔ leaf.0 ⊔ ('a1 ⊓ node.1) ⊔ (node.2 ⊓ treeA.0) ⊔ ('a1 ⊓ node.2 ⊓ treeA.1))
H: ('a1 ⊔ F.0)
F: (([0,1] ⊓ H.0) ⊔ ([0,1] ⊓ 'a1 ⊓ H.1))
G: ⊤

Infer2: linear decomposition (base + coeffs):
foo: base=⊥
bar: base=[0,1]
baz: base=⊤
one: base=([0,1] ⊓ two.0)
two: base=one.0
maybe: base=(some.0 ⊔ none.0), 'a1=some.1
wrap: base=⊥, 'a1=[1,0]
wrap2: base=⊤, 'a1=⊥
pairish: base=⊥, 'a1=[1,0], 'a2=[1,0]
treeA: base=(node.0 ⊔ leaf.0 ⊔ (node.2 ⊓ treeA.0)), 'a1=(node.1 ⊔ (node.2 ⊓ treeA.1))
H: base=F.0, 'a1=⊤
F: base=([0,1] ⊓ H.0), 'a1=([0,1] ⊓ H.1)
G: base=⊤, 'a1=⊥

Infer2: solving atoms:
foo.0 ≤ ⊥
bar.0 ≤ ([0,1] ⊓ bar.0)
baz.0 ≤ baz.0
one.0 ≤ ([0,1] ⊓ one.0)
two.0 = ([0,1] ⊓ one.0)
maybe.0 ≤ ((some.0 ⊓ maybe.0) ⊔ (none.0 ⊓ maybe.0))
maybe.1 ≤ ((some.0 ⊓ maybe.1) ⊔ (some.1 ⊓ maybe.1) ⊔ (none.0 ⊓ maybe.1))
wrap.0 ≤ ⊥
wrap.1 ≤ ([1,0] ⊓ wrap.1)
wrap2.0 ≤ wrap2.0
wrap2.1 ≤ wrap2.1
pairish.0 ≤ ⊥
pairish.1 ≤ ([1,0] ⊓ pairish.1)
pairish.2 ≤ ([1,0] ⊓ pairish.2)
treeA.0 ≤ ((node.0 ⊓ treeA.0) ⊔ (node.2 ⊓ treeA.0) ⊔ (treeA.0 ⊓ leaf.0))
treeA.1 ≤ ((node.0 ⊓ treeA.1) ⊔ (node.1 ⊓ treeA.1) ⊔ (node.2 ⊓ treeA.1) ⊔ (treeA.1 ⊓ leaf.0))
H.0 ≤ ([0,1] ⊓ F.0 ⊓ H.0)
H.1 ≤ H.1
F.0 ≤ ([0,1] ⊓ F.0 ⊓ H.0)
F.1 ≤ (([0,1] ⊓ F.1 ⊓ H.1) ⊔ ([0,1] ⊓ F.0 ⊓ F.1 ⊓ H.0))
G.0 = ⊤
G.1 = ⊥

Infer2: Normalized kinds:
foo: {0 ↦ ⊥}
bar: {0 ↦ ([0,1] ⊓ bar.0)}
baz: {0 ↦ baz.0}
one: {0 ↦ ([0,1] ⊓ one.0)}
two: {0 ↦ ([0,1] ⊓ one.0)}
maybe: {0 ↦ ((some.0 ⊓ maybe.0) ⊔ (none.0 ⊓ maybe.0)), 1 ↦ ((some.0 ⊓ maybe.1) ⊔ (some.1 ⊓ maybe.1) ⊔ (none.0 ⊓ maybe.1))}
wrap: {0 ↦ ⊥, 1 ↦ ([1,0] ⊓ wrap.1)}
wrap2: {0 ↦ wrap2.0, 1 ↦ wrap2.1}
pairish: {0 ↦ ⊥, 1 ↦ ([1,0] ⊓ pairish.1), 2 ↦ ([1,0] ⊓ pairish.2)}
treeA: {0 ↦ ((node.0 ⊓ treeA.0) ⊔ (node.2 ⊓ treeA.0) ⊔ (treeA.0 ⊓ leaf.0)), 1 ↦ ((node.0 ⊓ treeA.1) ⊔ (node.1 ⊓ treeA.1) ⊔ (node.2 ⊓ treeA.1) ⊔ (treeA.1 ⊓ leaf.0))}
H: {0 ↦ ([0,1] ⊓ F.0 ⊓ H.0), 1 ↦ H.1}
F: {0 ↦ ([0,1] ⊓ F.0 ⊓ H.0), 1 ↦ ([0,1] ⊓ F.1 ⊓ H.1)}
G: {0 ↦ ⊤, 1 ↦ ⊥}

Normalized kinds:
foo: {0 ↦ ⊥}
bar: {0 ↦ ([0,1] ⊓ bar.0)}
baz: {0 ↦ baz.0}
one: {0 ↦ ([0,1] ⊓ one.0)}
two: {0 ↦ ([0,1] ⊓ one.0)}
maybe: {0 ↦ ((maybe.0 ⊓ none.0) ⊔ (maybe.0 ⊓ some.0)), 1 ↦ ((maybe.1 ⊓ none.0) ⊔ (maybe.1 ⊓ some.0) ⊔ (maybe.1 ⊓ some.1))}
wrap: {0 ↦ ⊥, 1 ↦ ([1,0] ⊓ wrap.1)}
wrap2: {0 ↦ wrap2.0, 1 ↦ wrap2.1}
pairish: {0 ↦ ⊥, 1 ↦ ([1,0] ⊓ pairish.1), 2 ↦ ([1,0] ⊓ pairish.2)}
treeA: {0 ↦ ((leaf.0 ⊓ treeA.0) ⊔ (node.0 ⊓ treeA.0) ⊔ (node.2 ⊓ treeA.0)), 1 ↦ ((leaf.0 ⊓ treeA.1) ⊔ (node.0 ⊓ treeA.1) ⊔ (node.1 ⊓ treeA.1) ⊔ (node.2 ⊓ treeA.1))}
H: {0 ↦ ([0,1] ⊓ F.0 ⊓ H.0), 1 ↦ H.1}
F: {0 ↦ ([0,1] ⊓ F.0 ⊓ H.0), 1 ↦ ([0,1] ⊓ F.1 ⊓ H.1)}
G: {0 ↦ ⊤, 1 ↦ ⊥}

Ceil/Floor kinds:
foo: ceil={0 ↦ ⊥}, floor={0 ↦ ⊥}
bar: ceil={0 ↦ [0,1]}, floor={0 ↦ ⊥}
baz: ceil={0 ↦ ⊤}, floor={0 ↦ ⊥}
one: ceil={0 ↦ [0,1]}, floor={0 ↦ ⊥}
two: ceil={0 ↦ [0,1]}, floor={0 ↦ ⊥}
maybe: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
wrap: ceil={0 ↦ ⊥, 1 ↦ [1,0]}, floor={0 ↦ ⊥, 1 ↦ ⊥}
wrap2: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
pairish: ceil={0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [1,0]}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
treeA: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
H: ceil={0 ↦ [0,1], 1 ↦ [2,0]}, floor={0 ↦ ⊥, 1 ↦ ⊥}
F: ceil={0 ↦ [0,1], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
G: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊤, 1 ↦ ⊥}

LEQ relationships:
foo <= bar, baz, one, two
bar <= (none)
baz <= (none)
one <= two
two <= one
maybe <= G
wrap <= G
wrap2 <= G
pairish <= (none)
treeA <= G
H <= G
F <= G
G <= (none)
```

## benjamin.types

```
type foo1('a1) = t2(t1('a1)) * t1(unit)
type foo2('a1) = t1(t2('a1)) * t2(unit)

type bar('a1) = bar(t1('a1)) + bar(t2('a1)) + 'a1
```

Program output:
```
Infer2: RHS as polys:
foo1: (t1.0 ⊔ t2.0 ⊔ (t1.1 ⊓ t2.1 ⊓ 'a1))
foo2: (t1.0 ⊔ t2.0 ⊔ (t1.1 ⊓ t2.1 ⊓ 'a1))
bar: ('a1 ⊔ bar.0 ⊔ (t1.0 ⊓ bar.1) ⊔ (t2.0 ⊓ bar.1))

Infer2: linear decomposition (base + coeffs):
foo1: base=(t1.0 ⊔ t2.0), 'a1=(t1.1 ⊓ t2.1)
foo2: base=(t1.0 ⊔ t2.0), 'a1=(t1.1 ⊓ t2.1)
bar: base=(bar.0 ⊔ (t1.0 ⊓ bar.1) ⊔ (t2.0 ⊓ bar.1)), 'a1=⊤

Infer2: solving atoms:
foo1.0 = (t1.0 ⊔ t2.0)
foo1.1 = (t1.1 ⊓ t2.1)
foo2.0 = (t1.0 ⊔ t2.0)
foo2.1 = (t1.1 ⊓ t2.1)
bar.0 = (t1.0 ⊔ t2.0)
bar.1 = ⊤

Infer2: Normalized kinds:
foo1: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ (t1.1 ⊓ t2.1)}
foo2: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ (t1.1 ⊓ t2.1)}
bar: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ ⊤}

Normalized kinds:
foo1: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ (t1.1 ⊓ t2.1)}
foo2: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ (t1.1 ⊓ t2.1)}
bar: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ ⊤}

Ceil/Floor kinds:
foo1: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
foo2: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
bar: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊤}

LEQ relationships:
foo1 <= foo2, bar
foo2 <= foo1, bar
bar <= (none)
```

## btree.types

```
type btree('a1) = (leaf('a1) + node(btree('a1), btree('a1)))
```

Program output:
```
Infer2: RHS as polys:
btree: (node.0 ⊔ leaf.0 ⊔ (node.1 ⊓ btree.0) ⊔ (btree.0 ⊓ node.2) ⊔ ('a1 ⊓ leaf.1) ⊔ (node.1 ⊓ btree.1 ⊓ 'a1) ⊔ (btree.1 ⊓ 'a1 ⊓ node.2))

Infer2: linear decomposition (base + coeffs):
btree: base=(node.0 ⊔ leaf.0 ⊔ (node.1 ⊓ btree.0) ⊔ (btree.0 ⊓ node.2)), 'a1=(leaf.1 ⊔ (node.1 ⊓ btree.1) ⊔ (btree.1 ⊓ node.2))

Infer2: solving atoms:
btree.0 = (node.0 ⊔ leaf.0)
btree.1 = leaf.1

Infer2: Normalized kinds:
btree: {0 ↦ (node.0 ⊔ leaf.0), 1 ↦ leaf.1}

Normalized kinds:
btree: {0 ↦ (leaf.0 ⊔ node.0), 1 ↦ leaf.1}

Ceil/Floor kinds:
btree: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}

LEQ relationships:
btree <= (none)
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
type ND1('a1) : C(D('a1)) + 'a1
type ND2('a1) : (B(C('a1)) @@ [1,0]) + (A(D('a1)) @@ [0,1])
type ND3('a1,'a2) : (U3(B('a1), D('a2)) @@ [1,1]) + (X2(C('a1)) @@ [0,1])

# 15) Nested concretes composed with abstracts
type CN1('a1) = DUP(C('a1)) * M1(D('a1))
type CN2('a1,'a2) = K(A('a1),'a2) + M2(B('a2))

# 16) Deep nesting through S/T/U chain
type NestSTU('a1) : T(U(S('a1)))

# 17) Annotated nesting at multiple levels
type AnnNest('a1) : ((B(C('a1)) @@ [1,0]) @@ [0,1]) + ('a1 @@ [1,1])

# 18) Cross-nested mutual recursion
type RecNest1('a1) : RecNest2(C('a1)) + 'a1
type RecNest2('a1) : RecNest1(D('a1)) @@ [0,1]

# 19) Mixed two-parameter nesting with join against abstract
type MixNest('a1,'a2) : U3(B('a1), D('a2)) + (Z1(C('a1),'a2) @@ [1,0])

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
type PRG('a1,'a2) : (PRH(PRG('a2,'a1)) @@ [1,0]) + 'a1
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
Infer2: RHS as polys:
A: ('a1 ⊔ ([1,0] ⊓ B.0))
B: (([0,1] ⊓ C.0) ⊔ ([0,1] ⊓ 'a1 ⊓ C.1))
C: (A.0 ⊔ ('a1 ⊓ A.1))
D: (([1,0] ⊓ 'a1) ⊔ E.0 ⊔ ('a1 ⊓ E.2) ⊔ (E.1 ⊓ 'a2))
E: (([0,1] ⊓ 'a2) ⊔ ([0,1] ⊓ D.0) ⊔ ([0,1] ⊓ 'a1 ⊓ D.1))
R: (([0,1] ⊓ 'a1) ⊔ ([1,0] ⊓ R.0) ⊔ ([1,0] ⊓ 'a1 ⊓ R.1))
K: ('a1 ⊔ B.0 ⊔ A.0 ⊔ (B.1 ⊓ 'a2))
U1: (([0,1] ⊓ 'a1) ⊔ ([1,0] ⊓ U2.0) ⊔ ([1,0] ⊓ 'a2 ⊓ U2.1))
U2: (([0,1] ⊓ U3.0) ⊔ ([0,1] ⊓ 'a1 ⊓ U3.1) ⊔ ([0,1] ⊓ 'a1 ⊓ U3.2))
U3: (([1,1] ⊓ U1.0) ⊔ ([1,1] ⊓ 'a1 ⊓ U1.1) ⊔ ([1,1] ⊓ 'a2 ⊓ U1.2))
M1: ([1,0] ⊔ M2.0 ⊔ ('a1 ⊓ M2.1))
M2: ([0,1] ⊔ ([2,0] ⊓ 'a1))
AC: (([1,0] ⊓ M2.0) ⊔ ([1,0] ⊓ 'a1 ⊓ M2.1))
PH: 'a1
X1: ('a1 ⊔ X2.0)
X2: (([0,1] ⊓ X1.0) ⊔ ([0,1] ⊓ 'a1 ⊓ X1.1))
S: (([0,1] ⊓ U.0) ⊔ ([0,1] ⊓ 'a1 ⊓ U.1))
T: (S.0 ⊔ ('a1 ⊓ S.1))
U: (([0,1] ⊓ S.0) ⊔ ([0,1] ⊓ 'a1 ⊓ S.1))
Z1: (([0,1] ⊓ Z1.0) ⊔ ([0,1] ⊓ 'a1 ⊓ Z1.1) ⊔ ([0,1] ⊓ 'a2 ⊓ Z1.2))
DUP: ([1,1] ⊔ ([2,0] ⊓ 'a1))
SW: (([1,0] ⊓ DUP.0) ⊔ ([1,0] ⊓ 'a1 ⊓ DUP.1))
ND1: ('a1 ⊔ C.0 ⊔ (C.1 ⊓ D.0))
ND2: (([1,0] ⊓ B.0) ⊔ ([0,1] ⊓ A.0) ⊔ ([1,0] ⊓ B.1 ⊓ C.0) ⊔ ([0,1] ⊓ A.1 ⊓ D.0) ⊔ ([1,0] ⊓ 'a1 ⊓ B.1 ⊓ C.1) ⊔ ([0,1] ⊓ 'a1 ⊓ A.1 ⊓ D.1))
ND3: (([1,1] ⊓ U3.0) ⊔ ([0,1] ⊓ X2.0) ⊔ ([1,1] ⊓ B.0 ⊓ U3.1) ⊔ ([0,1] ⊓ C.0 ⊓ X2.1) ⊔ ([1,1] ⊓ D.0 ⊓ U3.2) ⊔ ([1,1] ⊓ 'a1 ⊓ B.1 ⊓ U3.1) ⊔ ([0,1] ⊓ 'a1 ⊓ C.1 ⊓ X2.1) ⊔ ([1,1] ⊓ 'a2 ⊓ D.1 ⊓ U3.2))
CN1: (M1.0 ⊔ DUP.0 ⊔ (C.0 ⊓ DUP.1) ⊔ (D.0 ⊓ M1.1) ⊔ ('a1 ⊓ C.1 ⊓ DUP.1) ⊔ ('a1 ⊓ D.1 ⊓ M1.1))
CN2: (M2.0 ⊔ K.0 ⊔ (B.0 ⊓ M2.1) ⊔ (A.0 ⊓ K.1) ⊔ ('a2 ⊓ K.2) ⊔ ('a1 ⊓ A.1 ⊓ K.1) ⊔ (B.1 ⊓ 'a2 ⊓ M2.1))
NestSTU: (T.0 ⊔ (U.0 ⊓ T.1) ⊔ (U.1 ⊓ T.1 ⊓ S.0) ⊔ ('a1 ⊓ U.1 ⊓ T.1 ⊓ S.1))
AnnNest: ([1,1] ⊓ 'a1)
RecNest1: ('a1 ⊔ RecNest2.0 ⊔ (C.0 ⊓ RecNest2.1))
RecNest2: (([0,1] ⊓ RecNest1.0) ⊔ ([0,1] ⊓ D.0 ⊓ RecNest1.1) ⊔ ([0,1] ⊓ 'a1 ⊓ D.1 ⊓ RecNest1.1))
MixNest: (U3.0 ⊔ ([1,0] ⊓ Z1.0) ⊔ (B.0 ⊓ U3.1) ⊔ ([1,0] ⊓ C.0 ⊓ Z1.1) ⊔ ([1,0] ⊓ 'a2 ⊓ Z1.2) ⊔ (D.0 ⊓ U3.2) ⊔ ('a1 ⊓ B.1 ⊓ U3.1) ⊔ ([1,0] ⊓ 'a1 ⊓ C.1 ⊓ Z1.1) ⊔ ('a2 ⊓ D.1 ⊓ U3.2))
A0: ([1,0] ⊓ B0.0)
B0: A0.0
Z3: (([0,1] ⊓ 'a1) ⊔ ([1,0] ⊓ 'a2) ⊔ ([0,1] ⊓ Z3.0) ⊔ ([0,1] ⊓ 'a2 ⊓ Z3.3) ⊔ ([0,1] ⊓ Z3.1 ⊓ 'a3))
C3: (Z3.0 ⊔ ('a1 ⊓ Z3.3) ⊔ ('a2 ⊓ Z3.1) ⊔ ('a3 ⊓ Z3.2))
DupSelf: (S.0 ⊔ ('a1 ⊓ S.1))
PRF: (([1,0] ⊓ 'a1) ⊔ ([0,1] ⊓ PRD.0) ⊔ ([0,1] ⊓ PRD.1 ⊓ PRF.0) ⊔ ([0,1] ⊓ 'a1 ⊓ PRD.1 ⊓ PRF.1))
PRE: ('a1 ⊔ ([0,1] ⊓ PRF.0) ⊔ ([0,1] ⊓ PRF.1 ⊓ PRE.0))
PRC: (([0,1] ⊓ 'a1) ⊔ ([1,0] ⊓ PRD.0) ⊔ ([1,0] ⊓ PRD.1 ⊓ PRC.0) ⊔ ([1,0] ⊓ PRD.1 ⊓ PRE.0 ⊓ PRC.1) ⊔ ([1,0] ⊓ 'a1 ⊓ PRD.1 ⊓ PRE.1 ⊓ PRC.1))
PRD: (([0,1] ⊓ PRC.0) ⊔ ([0,1] ⊓ 'a1 ⊓ PRC.1))
PCX: (([1,0] ⊓ 'a1) ⊔ PCY.0 ⊔ (PCY.1 ⊓ PCX.0) ⊔ ('a1 ⊓ PCY.1 ⊓ PCX.1))
PCY: ('a1 ⊔ ([0,1] ⊓ PCZ.0) ⊔ ([0,1] ⊓ PCX.0 ⊓ PCZ.1))
PCZ: 'a1
PRG: ('a1 ⊔ ([1,0] ⊓ PRH.0) ⊔ ([1,0] ⊓ PRH.1 ⊓ PRG.0) ⊔ ([1,0] ⊓ 'a2 ⊓ PRH.1 ⊓ PRG.1))
PRH: ('a2 ⊔ ([0,1] ⊓ PRG.0) ⊔ ([0,1] ⊓ 'a1 ⊓ PRG.2) ⊔ ([0,1] ⊓ PRH.0 ⊓ PRG.1) ⊔ ([0,1] ⊓ 'a1 ⊓ PRH.1 ⊓ PRG.1))
CA: ('a1 ⊔ ([0,1] ⊓ CC.0))
CC: (([1,0] ⊓ 'a1) ⊔ CA.0 ⊔ ('a1 ⊓ CA.1))
CA2: (([0,1] ⊓ 'a1) ⊔ ([1,0] ⊓ CC2.0) ⊔ ([1,0] ⊓ 'a1 ⊓ CC2.2) ⊔ ([1,0] ⊓ 'a2 ⊓ CC2.1))
CC2: (([1,0] ⊓ 'a2) ⊔ CA2.0 ⊔ ('a1 ⊓ CA2.1) ⊔ ('a2 ⊓ CA2.2))
CA0: ([0,1] ⊓ CC0.0)
CC0: CA0.0
CAX: (([0,1] ⊓ 'a1) ⊔ CBX.0 ⊔ ([2,0] ⊓ 'a1 ⊓ CBX.1))
CBX: ('a1 ⊔ CCX.0 ⊔ (CCX.1 ⊓ CAX.0))
CCX: (([1,0] ⊓ CAX.0) ⊔ ([1,0] ⊓ 'a1 ⊓ CAX.1))

Infer2: linear decomposition (base + coeffs):
A: base=([1,0] ⊓ B.0), 'a1=⊤
B: base=([0,1] ⊓ C.0), 'a1=([0,1] ⊓ C.1)
C: base=A.0, 'a1=A.1
D: base=E.0, 'a1=([1,0] ⊔ E.2), 'a2=E.1
E: base=([0,1] ⊓ D.0), 'a1=([0,1] ⊓ D.1), 'a2=[0,1]
R: base=([1,0] ⊓ R.0), 'a1=([0,1] ⊔ ([1,0] ⊓ R.1))
K: base=(B.0 ⊔ A.0), 'a1=⊤, 'a2=B.1
U1: base=([1,0] ⊓ U2.0), 'a1=[0,1], 'a2=([1,0] ⊓ U2.1)
U2: base=([0,1] ⊓ U3.0), 'a1=(([0,1] ⊓ U3.1) ⊔ ([0,1] ⊓ U3.2))
U3: base=([1,1] ⊓ U1.0), 'a1=([1,1] ⊓ U1.1), 'a2=([1,1] ⊓ U1.2)
M1: base=([1,0] ⊔ M2.0), 'a1=M2.1
M2: base=[0,1], 'a1=[2,0]
AC: base=([1,0] ⊓ M2.0), 'a1=([1,0] ⊓ M2.1)
PH: base=⊥, 'a1=⊤, 'a2=⊥
X1: base=X2.0, 'a1=⊤
X2: base=([0,1] ⊓ X1.0), 'a1=([0,1] ⊓ X1.1)
S: base=([0,1] ⊓ U.0), 'a1=([0,1] ⊓ U.1)
T: base=S.0, 'a1=S.1
U: base=([0,1] ⊓ S.0), 'a1=([0,1] ⊓ S.1)
Z1: base=([0,1] ⊓ Z1.0), 'a1=([0,1] ⊓ Z1.1), 'a2=([0,1] ⊓ Z1.2)
DUP: base=[1,1], 'a1=[2,0]
SW: base=([1,0] ⊓ DUP.0), 'a1=([1,0] ⊓ DUP.1), 'a2=⊥
ND1: base=(C.0 ⊔ (C.1 ⊓ D.0)), 'a1=⊤
ND2: base=(([1,0] ⊓ B.0) ⊔ ([0,1] ⊓ A.0) ⊔ ([1,0] ⊓ B.1 ⊓ C.0) ⊔ ([0,1] ⊓ A.1 ⊓ D.0)), 'a1=(([1,0] ⊓ B.1 ⊓ C.1) ⊔ ([0,1] ⊓ A.1 ⊓ D.1))
ND3: base=(([1,1] ⊓ U3.0) ⊔ ([0,1] ⊓ X2.0) ⊔ ([1,1] ⊓ B.0 ⊓ U3.1) ⊔ ([0,1] ⊓ C.0 ⊓ X2.1) ⊔ ([1,1] ⊓ D.0 ⊓ U3.2)), 'a1=(([1,1] ⊓ B.1 ⊓ U3.1) ⊔ ([0,1] ⊓ C.1 ⊓ X2.1)), 'a2=([1,1] ⊓ D.1 ⊓ U3.2)
CN1: base=(M1.0 ⊔ DUP.0 ⊔ (C.0 ⊓ DUP.1) ⊔ (D.0 ⊓ M1.1)), 'a1=((C.1 ⊓ DUP.1) ⊔ (D.1 ⊓ M1.1))
CN2: base=(M2.0 ⊔ K.0 ⊔ (B.0 ⊓ M2.1) ⊔ (A.0 ⊓ K.1)), 'a1=(A.1 ⊓ K.1), 'a2=(K.2 ⊔ (B.1 ⊓ M2.1))
NestSTU: base=(T.0 ⊔ (U.0 ⊓ T.1) ⊔ (U.1 ⊓ T.1 ⊓ S.0)), 'a1=(U.1 ⊓ T.1 ⊓ S.1)
AnnNest: base=⊥, 'a1=[1,1]
RecNest1: base=(RecNest2.0 ⊔ (C.0 ⊓ RecNest2.1)), 'a1=⊤
RecNest2: base=(([0,1] ⊓ RecNest1.0) ⊔ ([0,1] ⊓ D.0 ⊓ RecNest1.1)), 'a1=([0,1] ⊓ D.1 ⊓ RecNest1.1)
MixNest: base=(U3.0 ⊔ ([1,0] ⊓ Z1.0) ⊔ (B.0 ⊓ U3.1) ⊔ ([1,0] ⊓ C.0 ⊓ Z1.1) ⊔ (D.0 ⊓ U3.2)), 'a1=((B.1 ⊓ U3.1) ⊔ ([1,0] ⊓ C.1 ⊓ Z1.1)), 'a2=(([1,0] ⊓ Z1.2) ⊔ (D.1 ⊓ U3.2))
A0: base=([1,0] ⊓ B0.0)
B0: base=A0.0
Z3: base=([0,1] ⊓ Z3.0), 'a1=[0,1], 'a2=([1,0] ⊔ ([0,1] ⊓ Z3.3)), 'a3=([0,1] ⊓ Z3.1)
C3: base=Z3.0, 'a1=Z3.3, 'a2=Z3.1, 'a3=Z3.2
DupSelf: base=S.0, 'a1=S.1
PRF: base=(([0,1] ⊓ PRD.0) ⊔ ([0,1] ⊓ PRD.1 ⊓ PRF.0)), 'a1=([1,0] ⊔ ([0,1] ⊓ PRD.1 ⊓ PRF.1))
PRE: base=(([0,1] ⊓ PRF.0) ⊔ ([0,1] ⊓ PRF.1 ⊓ PRE.0)), 'a1=⊤
PRC: base=(([1,0] ⊓ PRD.0) ⊔ ([1,0] ⊓ PRD.1 ⊓ PRC.0) ⊔ ([1,0] ⊓ PRD.1 ⊓ PRE.0 ⊓ PRC.1)), 'a1=([0,1] ⊔ ([1,0] ⊓ PRD.1 ⊓ PRE.1 ⊓ PRC.1))
PRD: base=([0,1] ⊓ PRC.0), 'a1=([0,1] ⊓ PRC.1)
PCX: base=(PCY.0 ⊔ (PCY.1 ⊓ PCX.0)), 'a1=([1,0] ⊔ (PCY.1 ⊓ PCX.1))
PCY: base=(([0,1] ⊓ PCZ.0) ⊔ ([0,1] ⊓ PCX.0 ⊓ PCZ.1)), 'a1=⊤
PCZ: base=⊥, 'a1=⊤
PRG: base=(([1,0] ⊓ PRH.0) ⊔ ([1,0] ⊓ PRH.1 ⊓ PRG.0)), 'a1=⊤, 'a2=([1,0] ⊓ PRH.1 ⊓ PRG.1)
PRH: base=(([0,1] ⊓ PRG.0) ⊔ ([0,1] ⊓ PRH.0 ⊓ PRG.1)), 'a1=(([0,1] ⊓ PRG.2) ⊔ ([0,1] ⊓ PRH.1 ⊓ PRG.1)), 'a2=⊤
CA: base=([0,1] ⊓ CC.0), 'a1=⊤
CC: base=CA.0, 'a1=([1,0] ⊔ CA.1)
CA2: base=([1,0] ⊓ CC2.0), 'a1=([0,1] ⊔ ([1,0] ⊓ CC2.2)), 'a2=([1,0] ⊓ CC2.1)
CC2: base=CA2.0, 'a1=CA2.1, 'a2=([1,0] ⊔ CA2.2)
CA0: base=([0,1] ⊓ CC0.0)
CC0: base=CA0.0
CAX: base=CBX.0, 'a1=([0,1] ⊔ ([2,0] ⊓ CBX.1))
CBX: base=(CCX.0 ⊔ (CCX.1 ⊓ CAX.0)), 'a1=⊤
CCX: base=([1,0] ⊓ CAX.0), 'a1=([1,0] ⊓ CAX.1)

Infer2: solving atoms:
A.0 ≤ ⊥
A.1 ≤ A.1
B.0 ≤ ⊥
B.1 ≤ ([0,1] ⊓ B.1 ⊓ C.1 ⊓ A.1)
C.0 ≤ ⊥
C.1 ≤ (C.1 ⊓ A.1)
D.0 ≤ ([0,1] ⊓ E.0 ⊓ D.0)
D.1 ≤ (([1,0] ⊓ D.1) ⊔ ([0,1] ⊓ E.2 ⊓ D.1) ⊔ ([0,1] ⊓ E.0 ⊓ D.0 ⊓ D.1))
D.2 ≤ (([0,1] ⊓ E.0 ⊓ D.0 ⊓ D.2) ⊔ ([0,1] ⊓ E.1 ⊓ E.2 ⊓ D.1 ⊓ D.2))
E.0 ≤ ([0,1] ⊓ E.0 ⊓ D.0)
E.1 ≤ (([0,1] ⊓ E.0 ⊓ E.1 ⊓ D.0) ⊔ ([0,1] ⊓ E.1 ⊓ E.2 ⊓ D.1))
E.2 ≤ ([0,1] ⊓ E.2)
R.0 ≤ ([1,0] ⊓ R.0)
R.1 ≤ ([1,1] ⊓ R.1)
K.0 = ⊥
K.1 = ⊤
K.2 = ([0,1] ⊓ B.1 ⊓ C.1 ⊓ A.1)
U1.0 ≤ ⊥
U1.1 ≤ ([0,1] ⊓ U1.1)
U1.2 ≤ ⊥
U2.0 ≤ ⊥
U2.1 ≤ ([0,1] ⊓ U2.1 ⊓ U3.1 ⊓ U1.1)
U3.0 ≤ ⊥
U3.1 ≤ ([0,1] ⊓ U3.1 ⊓ U1.1)
U3.2 ≤ ⊥
M1.0 = [1,1]
M1.1 = [2,0]
M2.0 = [0,1]
M2.1 = [2,0]
AC.0 ≤ ⊥
AC.1 ≤ ([1,0] ⊓ AC.1)
PH.0 ≤ ⊥
PH.1 ≤ PH.1
PH.2 ≤ ⊥
X1.0 = ([0,1] ⊓ X2.0)
X1.1 = ⊤
X2.0 ≤ ([0,1] ⊓ X2.0)
X2.1 ≤ ([0,1] ⊓ X2.1)
S.0 ≤ ([0,1] ⊓ U.0 ⊓ S.0)
S.1 ≤ (([0,1] ⊓ U.1 ⊓ S.1) ⊔ ([0,1] ⊓ U.0 ⊓ S.0 ⊓ S.1))
T.0 ≤ ([0,1] ⊓ U.0 ⊓ T.0 ⊓ S.0)
T.1 ≤ (([0,1] ⊓ U.0 ⊓ T.1 ⊓ S.0) ⊔ ([0,1] ⊓ U.1 ⊓ T.1 ⊓ S.1))
U.0 ≤ ([0,1] ⊓ U.0 ⊓ S.0)
U.1 ≤ (([0,1] ⊓ U.1 ⊓ S.1) ⊔ ([0,1] ⊓ U.0 ⊓ U.1 ⊓ S.0))
Z1.0 ≤ ([0,1] ⊓ Z1.0)
Z1.1 ≤ ([0,1] ⊓ Z1.1)
Z1.2 ≤ ([0,1] ⊓ Z1.2)
DUP.0 = [1,1]
DUP.1 = [2,0]
SW.0 ≤ ([1,0] ⊓ SW.0)
SW.1 ≤ ([1,0] ⊓ SW.1)
SW.2 ≤ ([1,0] ⊓ SW.2)
ND1.0 ≤ ([0,1] ⊓ C.1 ⊓ A.1 ⊓ E.0 ⊓ D.0 ⊓ ND1.0)
ND1.1 ≤ ND1.1
ND2.0 ≤ ([0,1] ⊓ A.1 ⊓ E.0 ⊓ D.0 ⊓ ND2.0)
ND2.1 ≤ (([0,1] ⊓ A.1 ⊓ E.0 ⊓ D.0 ⊓ ND2.1) ⊔ ([0,1] ⊓ A.1 ⊓ E.2 ⊓ D.1 ⊓ ND2.1))
ND3.0 ≤ ([0,1] ⊓ X2.0 ⊓ ND3.0)
ND3.1 ≤ (([0,1] ⊓ X2.0 ⊓ ND3.1) ⊔ ([0,1] ⊓ C.1 ⊓ A.1 ⊓ X2.1 ⊓ ND3.1) ⊔ ([0,1] ⊓ B.1 ⊓ C.1 ⊓ A.1 ⊓ U3.1 ⊓ U1.1 ⊓ ND3.1))
ND3.2 ≤ ([0,1] ⊓ X2.0 ⊓ ND3.2)
CN1.0 = [1,1]
CN1.1 = (([1,0] ⊓ D.1) ⊔ ([2,0] ⊓ C.1 ⊓ A.1))
CN2.0 = [0,1]
CN2.1 = A.1
CN2.2 = ([0,1] ⊓ B.1 ⊓ C.1 ⊓ A.1)
NestSTU.0 ≤ (([0,1] ⊓ U.0 ⊓ T.0 ⊓ S.0 ⊓ NestSTU.0) ⊔ ([0,1] ⊓ U.0 ⊓ T.1 ⊓ S.0 ⊓ NestSTU.0))
NestSTU.1 ≤ (([0,1] ⊓ U.0 ⊓ T.0 ⊓ S.0 ⊓ NestSTU.1) ⊔ ([0,1] ⊓ U.0 ⊓ T.1 ⊓ S.0 ⊓ NestSTU.1) ⊔ ([0,1] ⊓ U.1 ⊓ T.1 ⊓ S.1 ⊓ NestSTU.1))
AnnNest.0 ≤ ⊥
AnnNest.1 ≤ ([1,1] ⊓ AnnNest.1)
RecNest1.0 ≤ ([0,1] ⊓ RecNest2.0 ⊓ RecNest1.0)
RecNest1.1 ≤ RecNest1.1
RecNest2.0 ≤ (([0,1] ⊓ RecNest2.0 ⊓ RecNest1.0) ⊔ ([0,1] ⊓ E.0 ⊓ D.0 ⊓ RecNest2.0 ⊓ RecNest1.1))
RecNest2.1 ≤ (([0,1] ⊓ RecNest2.0 ⊓ RecNest2.1 ⊓ RecNest1.0) ⊔ ([0,1] ⊓ E.0 ⊓ D.0 ⊓ RecNest2.1 ⊓ RecNest1.1) ⊔ ([0,1] ⊓ E.2 ⊓ D.1 ⊓ RecNest2.1 ⊓ RecNest1.1))
MixNest.0 ≤ ⊥
MixNest.1 ≤ ([0,1] ⊓ B.1 ⊓ C.1 ⊓ A.1 ⊓ U3.1 ⊓ U1.1 ⊓ MixNest.1)
MixNest.2 ≤ ⊥
A0.0 ≤ ([1,0] ⊓ B0.0 ⊓ A0.0)
B0.0 ≤ ([1,0] ⊓ B0.0 ⊓ A0.0)
Z3.0 ≤ ([0,1] ⊓ Z3.0)
Z3.1 ≤ ([0,1] ⊓ Z3.1)
Z3.2 ≤ (([1,0] ⊓ Z3.2) ⊔ ([0,1] ⊓ Z3.0 ⊓ Z3.2) ⊔ ([0,1] ⊓ Z3.1 ⊓ Z3.2 ⊓ Z3.3))
Z3.3 ≤ (([0,1] ⊓ Z3.0 ⊓ Z3.3) ⊔ ([0,1] ⊓ Z3.1 ⊓ Z3.3))
C3.0 = ([0,1] ⊓ Z3.0)
C3.1 = (([0,1] ⊓ Z3.0 ⊓ Z3.3) ⊔ ([0,1] ⊓ Z3.1 ⊓ Z3.3))
C3.2 = ([0,1] ⊓ Z3.1)
C3.3 = (([1,0] ⊓ Z3.2) ⊔ ([0,1] ⊓ Z3.0 ⊓ Z3.2) ⊔ ([0,1] ⊓ Z3.1 ⊓ Z3.2 ⊓ Z3.3))
DupSelf.0 ≤ ([0,1] ⊓ U.0 ⊓ S.0 ⊓ DupSelf.0)
DupSelf.1 ≤ (([0,1] ⊓ U.0 ⊓ S.0 ⊓ DupSelf.1) ⊔ ([0,1] ⊓ U.1 ⊓ S.1 ⊓ DupSelf.1))
PRF.0 ≤ ([0,1] ⊓ PRD.1 ⊓ PRF.0 ⊓ PRC.1)
PRF.1 ≤ (([1,0] ⊓ PRF.1) ⊔ ([0,1] ⊓ PRD.1 ⊓ PRF.1 ⊓ PRC.1))
PRE.0 ≤ (([0,1] ⊓ PRD.1 ⊓ PRF.0 ⊓ PRE.0 ⊓ PRC.1) ⊔ ([0,1] ⊓ PRD.1 ⊓ PRF.1 ⊓ PRE.0 ⊓ PRC.1))
PRE.1 ≤ PRE.1
PRC.0 ≤ ⊥
PRC.1 ≤ ([0,1] ⊓ PRC.1)
PRD.0 ≤ ⊥
PRD.1 ≤ ([0,1] ⊓ PRD.1 ⊓ PRC.1)
PCX.0 = ⊥
PCX.1 = [1,0]
PCY.0 = ⊥
PCY.1 = ⊤
PCZ.0 = ⊥
PCZ.1 = ⊤
PRG.0 ≤ ⊥
PRG.1 ≤ PRG.1
PRG.2 ≤ ⊥
PRH.0 ≤ ([0,1] ⊓ PRH.0 ⊓ PRG.1)
PRH.1 ≤ ([0,1] ⊓ PRH.1 ⊓ PRG.1)
PRH.2 ≤ PRH.2
CA.0 ≤ ([0,1] ⊓ CA.0)
CA.1 ≤ CA.1
CC.0 = ([0,1] ⊓ CA.0)
CC.1 = ([1,0] ⊔ CA.1)
CA2.0 ≤ ([1,0] ⊓ CA2.0)
CA2.1 ≤ ([1,1] ⊓ CA2.1)
CA2.2 ≤ (([1,0] ⊓ CA2.0 ⊓ CA2.2) ⊔ ([1,0] ⊓ CA2.1 ⊓ CA2.2))
CC2.0 = ([1,0] ⊓ CA2.0)
CC2.1 = ([1,1] ⊓ CA2.1)
CC2.2 = [1,0]
CA0.0 ≤ ([0,1] ⊓ CA0.0)
CC0.0 = ([0,1] ⊓ CA0.0)
CAX.0 ≤ (([1,0] ⊓ CCX.0 ⊓ CAX.0) ⊔ ([1,0] ⊓ CCX.1 ⊓ CAX.0))
CAX.1 ≤ CAX.1
CBX.0 = (([1,0] ⊓ CCX.0 ⊓ CAX.0) ⊔ ([1,0] ⊓ CCX.1 ⊓ CAX.0))
CBX.1 = ⊤
CCX.0 ≤ ([1,0] ⊓ CCX.0 ⊓ CAX.0)
CCX.1 ≤ (([1,0] ⊓ CCX.1 ⊓ CAX.0) ⊔ ([1,0] ⊓ CCX.1 ⊓ CAX.1))

Infer2: Normalized kinds:
A: {0 ↦ ⊥, 1 ↦ A.1}
B: {0 ↦ ⊥, 1 ↦ ([0,1] ⊓ B.1 ⊓ C.1 ⊓ A.1)}
C: {0 ↦ ⊥, 1 ↦ (C.1 ⊓ A.1)}
D: {0 ↦ ([0,1] ⊓ E.0 ⊓ D.0), 1 ↦ (([1,0] ⊓ D.1) ⊔ ([0,1] ⊓ E.2 ⊓ D.1)), 2 ↦ ([0,1] ⊓ E.1 ⊓ E.2 ⊓ D.1 ⊓ D.2)}
E: {0 ↦ ([0,1] ⊓ E.0 ⊓ D.0), 1 ↦ ([0,1] ⊓ E.1 ⊓ E.2 ⊓ D.1), 2 ↦ ([0,1] ⊓ E.2)}
R: {0 ↦ ([1,0] ⊓ R.0), 1 ↦ ([1,1] ⊓ R.1)}
K: {0 ↦ ⊥, 1 ↦ ⊤, 2 ↦ ([0,1] ⊓ B.1 ⊓ C.1 ⊓ A.1)}
U1: {0 ↦ ⊥, 1 ↦ ([0,1] ⊓ U1.1), 2 ↦ ⊥}
U2: {0 ↦ ⊥, 1 ↦ ([0,1] ⊓ U2.1 ⊓ U3.1 ⊓ U1.1)}
U3: {0 ↦ ⊥, 1 ↦ ([0,1] ⊓ U3.1 ⊓ U1.1), 2 ↦ ⊥}
M1: {0 ↦ [1,1], 1 ↦ [2,0]}
M2: {0 ↦ [0,1], 1 ↦ [2,0]}
AC: {0 ↦ ⊥, 1 ↦ ([1,0] ⊓ AC.1)}
PH: {0 ↦ ⊥, 1 ↦ PH.1, 2 ↦ ⊥}
X1: {0 ↦ ([0,1] ⊓ X2.0), 1 ↦ ⊤}
X2: {0 ↦ ([0,1] ⊓ X2.0), 1 ↦ ([0,1] ⊓ X2.1)}
S: {0 ↦ ([0,1] ⊓ U.0 ⊓ S.0), 1 ↦ ([0,1] ⊓ U.1 ⊓ S.1)}
T: {0 ↦ ([0,1] ⊓ U.0 ⊓ T.0 ⊓ S.0), 1 ↦ (([0,1] ⊓ U.0 ⊓ T.1 ⊓ S.0) ⊔ ([0,1] ⊓ U.1 ⊓ T.1 ⊓ S.1))}
U: {0 ↦ ([0,1] ⊓ U.0 ⊓ S.0), 1 ↦ ([0,1] ⊓ U.1 ⊓ S.1)}
Z1: {0 ↦ ([0,1] ⊓ Z1.0), 1 ↦ ([0,1] ⊓ Z1.1), 2 ↦ ([0,1] ⊓ Z1.2)}
DUP: {0 ↦ [1,1], 1 ↦ [2,0]}
SW: {0 ↦ ([1,0] ⊓ SW.0), 1 ↦ ([1,0] ⊓ SW.1), 2 ↦ ([1,0] ⊓ SW.2)}
ND1: {0 ↦ ([0,1] ⊓ C.1 ⊓ A.1 ⊓ E.0 ⊓ D.0 ⊓ ND1.0), 1 ↦ ND1.1}
ND2: {0 ↦ ([0,1] ⊓ A.1 ⊓ E.0 ⊓ D.0 ⊓ ND2.0), 1 ↦ (([0,1] ⊓ A.1 ⊓ E.0 ⊓ D.0 ⊓ ND2.1) ⊔ ([0,1] ⊓ A.1 ⊓ E.2 ⊓ D.1 ⊓ ND2.1))}
ND3: {0 ↦ ([0,1] ⊓ X2.0 ⊓ ND3.0), 1 ↦ (([0,1] ⊓ X2.0 ⊓ ND3.1) ⊔ ([0,1] ⊓ C.1 ⊓ A.1 ⊓ X2.1 ⊓ ND3.1) ⊔ ([0,1] ⊓ B.1 ⊓ C.1 ⊓ A.1 ⊓ U3.1 ⊓ U1.1 ⊓ ND3.1)), 2 ↦ ([0,1] ⊓ X2.0 ⊓ ND3.2)}
CN1: {0 ↦ [1,1], 1 ↦ (([1,0] ⊓ D.1) ⊔ ([2,0] ⊓ C.1 ⊓ A.1))}
CN2: {0 ↦ [0,1], 1 ↦ A.1, 2 ↦ ([0,1] ⊓ B.1 ⊓ C.1 ⊓ A.1)}
NestSTU: {0 ↦ (([0,1] ⊓ U.0 ⊓ T.0 ⊓ S.0 ⊓ NestSTU.0) ⊔ ([0,1] ⊓ U.0 ⊓ T.1 ⊓ S.0 ⊓ NestSTU.0)), 1 ↦ (([0,1] ⊓ U.0 ⊓ T.0 ⊓ S.0 ⊓ NestSTU.1) ⊔ ([0,1] ⊓ U.0 ⊓ T.1 ⊓ S.0 ⊓ NestSTU.1) ⊔ ([0,1] ⊓ U.1 ⊓ T.1 ⊓ S.1 ⊓ NestSTU.1))}
AnnNest: {0 ↦ ⊥, 1 ↦ ([1,1] ⊓ AnnNest.1)}
RecNest1: {0 ↦ ([0,1] ⊓ RecNest2.0 ⊓ RecNest1.0), 1 ↦ RecNest1.1}
RecNest2: {0 ↦ (([0,1] ⊓ RecNest2.0 ⊓ RecNest1.0) ⊔ ([0,1] ⊓ E.0 ⊓ D.0 ⊓ RecNest2.0 ⊓ RecNest1.1)), 1 ↦ (([0,1] ⊓ E.0 ⊓ D.0 ⊓ RecNest2.1 ⊓ RecNest1.1) ⊔ ([0,1] ⊓ E.2 ⊓ D.1 ⊓ RecNest2.1 ⊓ RecNest1.1))}
MixNest: {0 ↦ ⊥, 1 ↦ ([0,1] ⊓ B.1 ⊓ C.1 ⊓ A.1 ⊓ U3.1 ⊓ U1.1 ⊓ MixNest.1), 2 ↦ ⊥}
A0: {0 ↦ ([1,0] ⊓ B0.0 ⊓ A0.0)}
B0: {0 ↦ ([1,0] ⊓ B0.0 ⊓ A0.0)}
Z3: {0 ↦ ([0,1] ⊓ Z3.0), 1 ↦ ([0,1] ⊓ Z3.1), 2 ↦ (([1,0] ⊓ Z3.2) ⊔ ([0,1] ⊓ Z3.1 ⊓ Z3.2 ⊓ Z3.3)), 3 ↦ ([0,1] ⊓ Z3.1 ⊓ Z3.3)}
C3: {0 ↦ ([0,1] ⊓ Z3.0), 1 ↦ (([0,1] ⊓ Z3.0 ⊓ Z3.3) ⊔ ([0,1] ⊓ Z3.1 ⊓ Z3.3)), 2 ↦ ([0,1] ⊓ Z3.1), 3 ↦ (([1,0] ⊓ Z3.2) ⊔ ([0,1] ⊓ Z3.0 ⊓ Z3.2) ⊔ ([0,1] ⊓ Z3.1 ⊓ Z3.2 ⊓ Z3.3))}
DupSelf: {0 ↦ ([0,1] ⊓ U.0 ⊓ S.0 ⊓ DupSelf.0), 1 ↦ (([0,1] ⊓ U.0 ⊓ S.0 ⊓ DupSelf.1) ⊔ ([0,1] ⊓ U.1 ⊓ S.1 ⊓ DupSelf.1))}
PRF: {0 ↦ ([0,1] ⊓ PRD.1 ⊓ PRF.0 ⊓ PRC.1), 1 ↦ (([1,0] ⊓ PRF.1) ⊔ ([0,1] ⊓ PRD.1 ⊓ PRF.1 ⊓ PRC.1))}
PRE: {0 ↦ (([0,1] ⊓ PRD.1 ⊓ PRF.0 ⊓ PRE.0 ⊓ PRC.1) ⊔ ([0,1] ⊓ PRD.1 ⊓ PRF.1 ⊓ PRE.0 ⊓ PRC.1)), 1 ↦ PRE.1}
PRC: {0 ↦ ⊥, 1 ↦ ([0,1] ⊓ PRC.1)}
PRD: {0 ↦ ⊥, 1 ↦ ([0,1] ⊓ PRD.1 ⊓ PRC.1)}
PCX: {0 ↦ ⊥, 1 ↦ [1,0]}
PCY: {0 ↦ ⊥, 1 ↦ ⊤}
PCZ: {0 ↦ ⊥, 1 ↦ ⊤}
PRG: {0 ↦ ⊥, 1 ↦ PRG.1, 2 ↦ ⊥}
PRH: {0 ↦ ([0,1] ⊓ PRH.0 ⊓ PRG.1), 1 ↦ ([0,1] ⊓ PRH.1 ⊓ PRG.1), 2 ↦ PRH.2}
CA: {0 ↦ ([0,1] ⊓ CA.0), 1 ↦ CA.1}
CC: {0 ↦ ([0,1] ⊓ CA.0), 1 ↦ ([1,0] ⊔ CA.1)}
CA2: {0 ↦ ([1,0] ⊓ CA2.0), 1 ↦ ([1,1] ⊓ CA2.1), 2 ↦ ([1,0] ⊓ CA2.1 ⊓ CA2.2)}
CC2: {0 ↦ ([1,0] ⊓ CA2.0), 1 ↦ ([1,1] ⊓ CA2.1), 2 ↦ [1,0]}
CA0: {0 ↦ ([0,1] ⊓ CA0.0)}
CC0: {0 ↦ ([0,1] ⊓ CA0.0)}
CAX: {0 ↦ (([1,0] ⊓ CCX.0 ⊓ CAX.0) ⊔ ([1,0] ⊓ CCX.1 ⊓ CAX.0)), 1 ↦ CAX.1}
CBX: {0 ↦ (([1,0] ⊓ CCX.0 ⊓ CAX.0) ⊔ ([1,0] ⊓ CCX.1 ⊓ CAX.0)), 1 ↦ ⊤}
CCX: {0 ↦ ([1,0] ⊓ CCX.0 ⊓ CAX.0), 1 ↦ (([1,0] ⊓ CCX.1 ⊓ CAX.0) ⊔ ([1,0] ⊓ CCX.1 ⊓ CAX.1))}

Normalized kinds:
A: {0 ↦ ⊥, 1 ↦ A.1}
B: {0 ↦ ⊥, 1 ↦ ([0,1] ⊓ A.1 ⊓ B.1 ⊓ C.1)}
C: {0 ↦ ⊥, 1 ↦ (A.1 ⊓ C.1)}
D: {0 ↦ ([0,1] ⊓ D.0 ⊓ E.0), 1 ↦ (([1,0] ⊓ D.1) ⊔ ([0,1] ⊓ D.1 ⊓ E.2)), 2 ↦ ([0,1] ⊓ D.1 ⊓ D.2 ⊓ E.1 ⊓ E.2)}
E: {0 ↦ ([0,1] ⊓ D.0 ⊓ E.0), 1 ↦ ([0,1] ⊓ D.1 ⊓ E.1 ⊓ E.2), 2 ↦ ([0,1] ⊓ E.2)}
R: {0 ↦ ([1,0] ⊓ R.0), 1 ↦ ([1,1] ⊓ R.1)}
K: {0 ↦ ⊥, 1 ↦ ⊤, 2 ↦ ([0,1] ⊓ A.1 ⊓ B.1 ⊓ C.1)}
U1: {0 ↦ ⊥, 1 ↦ ([0,1] ⊓ U1.1), 2 ↦ ⊥}
U2: {0 ↦ ⊥, 1 ↦ ([0,1] ⊓ U1.1 ⊓ U2.1 ⊓ U3.1)}
U3: {0 ↦ ⊥, 1 ↦ ([0,1] ⊓ U1.1 ⊓ U3.1), 2 ↦ ⊥}
M1: {0 ↦ [1,1], 1 ↦ [2,0]}
M2: {0 ↦ [0,1], 1 ↦ [2,0]}
AC: {0 ↦ ⊥, 1 ↦ ([1,0] ⊓ AC.1)}
PH: {0 ↦ ⊥, 1 ↦ PH.1, 2 ↦ ⊥}
X1: {0 ↦ ([0,1] ⊓ X2.0), 1 ↦ ⊤}
X2: {0 ↦ ([0,1] ⊓ X2.0), 1 ↦ ([0,1] ⊓ X2.1)}
S: {0 ↦ ([0,1] ⊓ S.0 ⊓ U.0), 1 ↦ ([0,1] ⊓ S.1 ⊓ U.1)}
T: {0 ↦ ([0,1] ⊓ S.0 ⊓ T.0 ⊓ U.0), 1 ↦ (([0,1] ⊓ S.0 ⊓ T.1 ⊓ U.0) ⊔ ([0,1] ⊓ S.1 ⊓ T.1 ⊓ U.1))}
U: {0 ↦ ([0,1] ⊓ S.0 ⊓ U.0), 1 ↦ ([0,1] ⊓ S.1 ⊓ U.1)}
Z1: {0 ↦ ([0,1] ⊓ Z1.0), 1 ↦ ([0,1] ⊓ Z1.1), 2 ↦ ([0,1] ⊓ Z1.2)}
DUP: {0 ↦ [1,1], 1 ↦ [2,0]}
SW: {0 ↦ ([1,0] ⊓ SW.0), 1 ↦ ([1,0] ⊓ SW.1), 2 ↦ ([1,0] ⊓ SW.2)}
ND1: {0 ↦ ([0,1] ⊓ A.1 ⊓ C.1 ⊓ D.0 ⊓ E.0 ⊓ ND1.0), 1 ↦ ND1.1}
ND2: {0 ↦ ([0,1] ⊓ A.1 ⊓ D.0 ⊓ E.0 ⊓ ND2.0), 1 ↦ (([0,1] ⊓ A.1 ⊓ D.0 ⊓ E.0 ⊓ ND2.1) ⊔ ([0,1] ⊓ A.1 ⊓ D.1 ⊓ E.2 ⊓ ND2.1))}
ND3: {0 ↦ ([0,1] ⊓ ND3.0 ⊓ X2.0), 1 ↦ (([0,1] ⊓ ND3.1 ⊓ X2.0) ⊔ ([0,1] ⊓ A.1 ⊓ C.1 ⊓ ND3.1 ⊓ X2.1) ⊔ ([0,1] ⊓ A.1 ⊓ B.1 ⊓ C.1 ⊓ ND3.1 ⊓ U1.1 ⊓ U3.1)), 2 ↦ ([0,1] ⊓ ND3.2 ⊓ X2.0)}
CN1: {0 ↦ [1,1], 1 ↦ ([2,0] ⊓ A.1 ⊓ C.1)}
CN2: {0 ↦ ([0,1] ⊔ ([2,0] ⊓ A.0) ⊔ ([2,0] ⊓ B.0)), 1 ↦ ([2,0] ⊓ A.1), 2 ↦ ([2,0] ⊓ B.1)}
NestSTU: {0 ↦ (([0,1] ⊓ NestSTU.0 ⊓ S.0 ⊓ T.0 ⊓ U.0) ⊔ ([0,1] ⊓ NestSTU.0 ⊓ S.0 ⊓ T.1 ⊓ U.0)), 1 ↦ (([0,1] ⊓ NestSTU.1 ⊓ S.0 ⊓ T.0 ⊓ U.0) ⊔ ([0,1] ⊓ NestSTU.1 ⊓ S.0 ⊓ T.1 ⊓ U.0) ⊔ ([0,1] ⊓ NestSTU.1 ⊓ S.1 ⊓ T.1 ⊓ U.1))}
AnnNest: {0 ↦ ⊥, 1 ↦ ([1,1] ⊓ AnnNest.1)}
RecNest1: {0 ↦ ([0,1] ⊓ RecNest1.0 ⊓ RecNest2.0), 1 ↦ RecNest1.1}
RecNest2: {0 ↦ (([0,1] ⊓ RecNest1.0 ⊓ RecNest2.0) ⊔ ([0,1] ⊓ D.0 ⊓ E.0 ⊓ RecNest1.1 ⊓ RecNest2.0)), 1 ↦ (([0,1] ⊓ D.0 ⊓ E.0 ⊓ RecNest1.1 ⊓ RecNest2.1) ⊔ ([0,1] ⊓ D.1 ⊓ E.2 ⊓ RecNest1.1 ⊓ RecNest2.1))}
MixNest: {0 ↦ ⊥, 1 ↦ ([0,1] ⊓ A.1 ⊓ B.1 ⊓ C.1 ⊓ MixNest.1 ⊓ U1.1 ⊓ U3.1), 2 ↦ ⊥}
A0: {0 ↦ ([1,0] ⊓ A0.0 ⊓ B0.0)}
B0: {0 ↦ ([1,0] ⊓ A0.0 ⊓ B0.0)}
Z3: {0 ↦ ([0,1] ⊓ Z3.0), 1 ↦ ([0,1] ⊓ Z3.1), 2 ↦ (([1,0] ⊓ Z3.2) ⊔ ([0,1] ⊓ Z3.1 ⊓ Z3.2 ⊓ Z3.3)), 3 ↦ ([0,1] ⊓ Z3.1 ⊓ Z3.3)}
C3: {0 ↦ ([0,1] ⊓ Z3.0), 1 ↦ ([0,1] ⊓ Z3.1 ⊓ Z3.3), 2 ↦ ([0,1] ⊓ Z3.1), 3 ↦ (([1,0] ⊓ Z3.2) ⊔ ([0,1] ⊓ Z3.1 ⊓ Z3.2 ⊓ Z3.3))}
DupSelf: {0 ↦ ([0,1] ⊓ DupSelf.0 ⊓ S.0 ⊓ U.0), 1 ↦ (([0,1] ⊓ DupSelf.1 ⊓ S.0 ⊓ U.0) ⊔ ([0,1] ⊓ DupSelf.1 ⊓ S.1 ⊓ U.1))}
PRF: {0 ↦ ([0,1] ⊓ PRC.1 ⊓ PRD.1 ⊓ PRF.0), 1 ↦ (([1,0] ⊓ PRF.1) ⊔ ([0,1] ⊓ PRC.1 ⊓ PRD.1 ⊓ PRF.1))}
PRE: {0 ↦ (([0,1] ⊓ PRC.1 ⊓ PRD.1 ⊓ PRE.0 ⊓ PRF.0) ⊔ ([0,1] ⊓ PRC.1 ⊓ PRD.1 ⊓ PRE.0 ⊓ PRF.1)), 1 ↦ PRE.1}
PRC: {0 ↦ ⊥, 1 ↦ ([0,1] ⊓ PRC.1)}
PRD: {0 ↦ ⊥, 1 ↦ ([0,1] ⊓ PRC.1 ⊓ PRD.1)}
PCX: {0 ↦ ⊥, 1 ↦ [1,0]}
PCY: {0 ↦ ⊥, 1 ↦ ⊤}
PCZ: {0 ↦ ⊥, 1 ↦ ⊤}
PRG: {0 ↦ ⊥, 1 ↦ PRG.1, 2 ↦ ⊥}
PRH: {0 ↦ ([0,1] ⊓ PRG.1 ⊓ PRH.0), 1 ↦ ([0,1] ⊓ PRG.1 ⊓ PRH.1), 2 ↦ PRH.2}
CA: {0 ↦ ([0,1] ⊓ CA.0), 1 ↦ CA.1}
CC: {0 ↦ ([0,1] ⊓ CA.0), 1 ↦ ([1,0] ⊔ CA.1)}
CA2: {0 ↦ ([1,0] ⊓ CA2.0), 1 ↦ ([1,1] ⊓ CA2.1), 2 ↦ ([1,0] ⊓ CA2.1 ⊓ CA2.2)}
CC2: {0 ↦ ([1,0] ⊓ CA2.0), 1 ↦ ([1,1] ⊓ CA2.1), 2 ↦ [1,0]}
CA0: {0 ↦ ([0,1] ⊓ CA0.0)}
CC0: {0 ↦ ([0,1] ⊓ CA0.0)}
CAX: {0 ↦ ((CAX.0 ⊓ CCX.0) ⊔ (CAX.0 ⊓ CCX.1)), 1 ↦ CAX.1}
CBX: {0 ↦ (([1,0] ⊓ CAX.0 ⊓ CCX.0) ⊔ ([1,0] ⊓ CAX.0 ⊓ CCX.1)), 1 ↦ ⊤}
CCX: {0 ↦ ([1,0] ⊓ CAX.0 ⊓ CCX.0), 1 ↦ (([1,0] ⊓ CAX.0 ⊓ CCX.1) ⊔ ([1,0] ⊓ CAX.1 ⊓ CCX.1))}

Ceil/Floor kinds:
A: ceil={0 ↦ ⊥, 1 ↦ ⊤}, floor={0 ↦ ⊥, 1 ↦ ⊥}
B: ceil={0 ↦ ⊥, 1 ↦ [0,1]}, floor={0 ↦ ⊥, 1 ↦ ⊥}
C: ceil={0 ↦ ⊥, 1 ↦ ⊤}, floor={0 ↦ ⊥, 1 ↦ ⊥}
D: ceil={0 ↦ [0,1], 1 ↦ [1,0], 2 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
E: ceil={0 ↦ [0,1], 1 ↦ ⊥, 2 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
R: ceil={0 ↦ [1,0], 1 ↦ [0,1]}, floor={0 ↦ ⊥, 1 ↦ ⊥}
K: ceil={0 ↦ ⊥, 1 ↦ ⊤, 2 ↦ [0,1]}, floor={0 ↦ ⊥, 1 ↦ ⊤, 2 ↦ ⊥}
U1: ceil={0 ↦ ⊥, 1 ↦ [0,1], 2 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
U2: ceil={0 ↦ ⊥, 1 ↦ [0,1]}, floor={0 ↦ ⊥, 1 ↦ ⊥}
U3: ceil={0 ↦ ⊥, 1 ↦ [0,1], 2 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
M1: ceil={0 ↦ [1,1], 1 ↦ [2,0]}, floor={0 ↦ [1,1], 1 ↦ [2,0]}
M2: ceil={0 ↦ [0,1], 1 ↦ [2,0]}, floor={0 ↦ [0,1], 1 ↦ [2,0]}
AC: ceil={0 ↦ ⊥, 1 ↦ [1,0]}, floor={0 ↦ ⊥, 1 ↦ ⊥}
PH: ceil={0 ↦ ⊥, 1 ↦ ⊤, 2 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
X1: ceil={0 ↦ [0,1], 1 ↦ [2,0]}, floor={0 ↦ ⊥, 1 ↦ ⊤}
X2: ceil={0 ↦ [0,1], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
S: ceil={0 ↦ [0,1], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
T: ceil={0 ↦ [0,1], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
U: ceil={0 ↦ [0,1], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
Z1: ceil={0 ↦ [0,1], 1 ↦ ⊥, 2 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
DUP: ceil={0 ↦ [1,1], 1 ↦ [2,0]}, floor={0 ↦ [1,1], 1 ↦ [2,0]}
SW: ceil={0 ↦ [1,0], 1 ↦ ⊥, 2 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
ND1: ceil={0 ↦ [0,1], 1 ↦ [2,0]}, floor={0 ↦ ⊥, 1 ↦ ⊥}
ND2: ceil={0 ↦ [0,1], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
ND3: ceil={0 ↦ [0,1], 1 ↦ ⊥, 2 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
CN1: ceil={0 ↦ [1,1], 1 ↦ [2,0]}, floor={0 ↦ [1,1], 1 ↦ ⊥}
CN2: ceil={0 ↦ ⊤, 1 ↦ ⊥, 2 ↦ ⊥}, floor={0 ↦ [0,1], 1 ↦ ⊥, 2 ↦ ⊥}
NestSTU: ceil={0 ↦ [0,1], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
AnnNest: ceil={0 ↦ ⊥, 1 ↦ [1,1]}, floor={0 ↦ ⊥, 1 ↦ ⊥}
RecNest1: ceil={0 ↦ [0,1], 1 ↦ [2,0]}, floor={0 ↦ ⊥, 1 ↦ ⊥}
RecNest2: ceil={0 ↦ [0,1], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
MixNest: ceil={0 ↦ ⊥, 1 ↦ [0,1], 2 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
A0: ceil={0 ↦ [1,0]}, floor={0 ↦ ⊥}
B0: ceil={0 ↦ [1,0]}, floor={0 ↦ ⊥}
Z3: ceil={0 ↦ [0,1], 1 ↦ ⊥, 2 ↦ [1,0], 3 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥, 3 ↦ ⊥}
C3: ceil={0 ↦ [0,1], 1 ↦ ⊥, 2 ↦ ⊥, 3 ↦ [1,0]}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥, 3 ↦ ⊥}
DupSelf: ceil={0 ↦ [0,1], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
PRF: ceil={0 ↦ [0,1], 1 ↦ [1,0]}, floor={0 ↦ ⊥, 1 ↦ ⊥}
PRE: ceil={0 ↦ [0,1], 1 ↦ [2,0]}, floor={0 ↦ ⊥, 1 ↦ ⊥}
PRC: ceil={0 ↦ ⊥, 1 ↦ [0,1]}, floor={0 ↦ ⊥, 1 ↦ ⊥}
PRD: ceil={0 ↦ ⊥, 1 ↦ [0,1]}, floor={0 ↦ ⊥, 1 ↦ ⊥}
PCX: ceil={0 ↦ ⊥, 1 ↦ [1,0]}, floor={0 ↦ ⊥, 1 ↦ [1,0]}
PCY: ceil={0 ↦ ⊥, 1 ↦ ⊤}, floor={0 ↦ ⊥, 1 ↦ ⊤}
PCZ: ceil={0 ↦ ⊥, 1 ↦ ⊤}, floor={0 ↦ ⊥, 1 ↦ ⊤}
PRG: ceil={0 ↦ ⊥, 1 ↦ ⊤, 2 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
PRH: ceil={0 ↦ [0,1], 1 ↦ ⊥, 2 ↦ [2,0]}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
CA: ceil={0 ↦ [0,1], 1 ↦ [2,0]}, floor={0 ↦ ⊥, 1 ↦ ⊥}
CC: ceil={0 ↦ [0,1], 1 ↦ [2,0]}, floor={0 ↦ ⊥, 1 ↦ [1,0]}
CA2: ceil={0 ↦ [1,0], 1 ↦ [0,1], 2 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
CC2: ceil={0 ↦ [1,0], 1 ↦ [0,1], 2 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ [1,0]}
CA0: ceil={0 ↦ [0,1]}, floor={0 ↦ ⊥}
CC0: ceil={0 ↦ [0,1]}, floor={0 ↦ ⊥}
CAX: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
CBX: ceil={0 ↦ [1,0], 1 ↦ ⊤}, floor={0 ↦ ⊥, 1 ↦ ⊤}
CCX: ceil={0 ↦ [1,0], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}

LEQ relationships:
A <= M1, M2, X1, DUP, PCY, PCZ, CBX
B <= A, C, M1, M2, X1, DUP, CN1, PCY, PCZ, CBX
C <= A, M1, M2, X1, DUP, CN1, PCY, PCZ, CBX
D <= (none)
E <= CN2
R <= M1, DUP, CN1
K <= (none)
U1 <= K, CN2
U2 <= M1, M2, X1, DUP, CN1, PCY, PCZ, CBX
U3 <= K, U1, CN2
M1 <= DUP
M2 <= M1, DUP
AC <= M1, M2, X1, DUP, CN1, PCX, PCY, PCZ, CC, CBX
PH <= K
X1 <= M1, M2, DUP
X2 <= M1, M2, X1, DUP, CN1
S <= M1, M2, DUP, CN1
T <= M1, M2, DUP, CN1
U <= M1, M2, DUP, CN1
Z1 <= CN2
DUP <= M1
SW <= (none)
ND1 <= M1, M2, DUP
ND2 <= M1, M2, DUP, CN1
ND3 <= CN2
CN1 <= M1, DUP
CN2 <= (none)
NestSTU <= M1, M2, DUP, CN1
AnnNest <= M1, M2, X1, DUP, CN1, PCY, PCZ, CBX
RecNest1 <= M1, M2, DUP
RecNest2 <= M1, M2, DUP, CN1
MixNest <= K, U1, U3, CN2
A0 <= B0
B0 <= A0
Z3 <= (none)
C3 <= (none)
DupSelf <= M1, M2, DUP, CN1
PRF <= M1, M2, DUP, CN1
PRE <= M1, M2, DUP
PRC <= M1, M2, X1, DUP, CN1, PCY, PCZ, CBX
PRD <= M1, M2, X1, DUP, CN1, PRC, PCY, PCZ, CBX
PCX <= M1, M2, X1, DUP, CN1, PCY, PCZ, CC, CBX
PCY <= M1, M2, X1, DUP, PCZ, CBX
PCZ <= M1, M2, X1, DUP, PCY, CBX
PRG <= K
PRH <= (none)
CA <= M1, M2, DUP, CC
CC <= M1, M2, DUP
CA2 <= CC2
CC2 <= (none)
CA0 <= CC0
CC0 <= CA0
CAX <= (none)
CBX <= M1, DUP
CCX <= M1, DUP, CN1, CBX
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
Infer2: RHS as polys:
H: ('a1 ⊔ F.0)
F: (([0,1] ⊓ H.0) ⊔ ([0,1] ⊓ 'a1 ⊓ H.1))
H2: ('a1 ⊔ F2.0 ⊔ (F2.1 ⊓ 'a2))
F2: (([1,0] ⊓ 'a2) ⊔ ([0,1] ⊓ H2.0) ⊔ ([0,1] ⊓ 'a1 ⊓ H2.1) ⊔ ([0,1] ⊓ 'a2 ⊓ H2.2))
X: (([0,1] ⊓ Z.0) ⊔ ([0,1] ⊓ 'a1 ⊓ Z.1))
Y: ('a1 ⊔ X.0)
Z: (([0,1] ⊓ Y.0) ⊔ ([0,1] ⊓ 'a1 ⊓ Y.1))
P: (([0,1] ⊓ 'a2) ⊔ ([1,1] ⊓ Q.0) ⊔ ([1,1] ⊓ 'a1 ⊓ Q.1))
Q: ('a1 ⊔ ([1,0] ⊓ P.0))
M: ('a1 ⊔ ([0,1] ⊓ M.0))
U: (W.0 ⊔ V.0 ⊔ ('a1 ⊓ V.1) ⊔ ('a2 ⊓ W.1))
V: (([1,0] ⊓ U.0) ⊔ ([1,0] ⊓ 'a1 ⊓ U.1) ⊔ ([1,0] ⊓ 'a1 ⊓ U.2))
W: (([0,1] ⊓ U.0) ⊔ ([0,1] ⊓ 'a1 ⊓ U.1) ⊔ ([0,1] ⊓ 'a1 ⊓ U.2))
C1: ('a1 ⊔ H.0)
C2: (([1,0] ⊓ 'a1) ⊔ ([1,0] ⊓ F.0))

Infer2: linear decomposition (base + coeffs):
H: base=F.0, 'a1=⊤
F: base=([0,1] ⊓ H.0), 'a1=([0,1] ⊓ H.1)
H2: base=F2.0, 'a1=⊤, 'a2=F2.1
F2: base=([0,1] ⊓ H2.0), 'a1=([0,1] ⊓ H2.1), 'a2=([1,0] ⊔ ([0,1] ⊓ H2.2))
X: base=([0,1] ⊓ Z.0), 'a1=([0,1] ⊓ Z.1)
Y: base=X.0, 'a1=⊤
Z: base=([0,1] ⊓ Y.0), 'a1=([0,1] ⊓ Y.1)
P: base=([1,1] ⊓ Q.0), 'a1=([1,1] ⊓ Q.1), 'a2=[0,1]
Q: base=([1,0] ⊓ P.0), 'a1=⊤
M: base=([0,1] ⊓ M.0), 'a1=⊤
U: base=(W.0 ⊔ V.0), 'a1=V.1, 'a2=W.1
V: base=([1,0] ⊓ U.0), 'a1=(([1,0] ⊓ U.1) ⊔ ([1,0] ⊓ U.2))
W: base=([0,1] ⊓ U.0), 'a1=(([0,1] ⊓ U.1) ⊔ ([0,1] ⊓ U.2))
C1: base=H.0, 'a1=⊤
C2: base=([1,0] ⊓ F.0), 'a1=[1,0]

Infer2: solving atoms:
H.0 ≤ ([0,1] ⊓ F.0 ⊓ H.0)
H.1 ≤ H.1
F.0 ≤ ([0,1] ⊓ F.0 ⊓ H.0)
F.1 ≤ (([0,1] ⊓ F.1 ⊓ H.1) ⊔ ([0,1] ⊓ F.0 ⊓ F.1 ⊓ H.0))
H2.0 ≤ ([0,1] ⊓ F2.0 ⊓ H2.0)
H2.1 ≤ H2.1
H2.2 ≤ (([0,1] ⊓ F2.0 ⊓ H2.0 ⊓ H2.2) ⊔ ([0,1] ⊓ F2.1 ⊓ H2.1 ⊓ H2.2))
F2.0 ≤ ([0,1] ⊓ F2.0 ⊓ H2.0)
F2.1 ≤ (([0,1] ⊓ F2.1 ⊓ H2.1) ⊔ ([0,1] ⊓ F2.0 ⊓ F2.1 ⊓ H2.0))
F2.2 ≤ (([1,0] ⊓ F2.2) ⊔ ([0,1] ⊓ F2.0 ⊓ F2.2 ⊓ H2.0) ⊔ ([0,1] ⊓ F2.1 ⊓ F2.2 ⊓ H2.1 ⊓ H2.2))
X.0 ≤ ([0,1] ⊓ Z.0 ⊓ Y.0 ⊓ X.0)
X.1 ≤ (([0,1] ⊓ Z.1 ⊓ Y.1 ⊓ X.1) ⊔ ([0,1] ⊓ Z.0 ⊓ Y.0 ⊓ X.0 ⊓ X.1))
Y.0 ≤ ([0,1] ⊓ Z.0 ⊓ Y.0 ⊓ X.0)
Y.1 ≤ Y.1
Z.0 ≤ ([0,1] ⊓ Z.0 ⊓ Y.0 ⊓ X.0)
Z.1 ≤ (([0,1] ⊓ Z.1 ⊓ Y.1) ⊔ ([0,1] ⊓ Z.0 ⊓ Z.1 ⊓ Y.0 ⊓ X.0))
P.0 ≤ ([1,0] ⊓ Q.0 ⊓ P.0)
P.1 ≤ (([1,1] ⊓ Q.1 ⊓ P.1) ⊔ ([1,0] ⊓ Q.0 ⊓ P.0 ⊓ P.1))
P.2 ≤ (([0,1] ⊓ P.2) ⊔ ([1,0] ⊓ Q.0 ⊓ P.0 ⊓ P.2))
Q.0 ≤ ([1,0] ⊓ Q.0 ⊓ P.0)
Q.1 ≤ Q.1
M.0 ≤ ([0,1] ⊓ M.0)
M.1 ≤ M.1
U.0 ≤ (([0,1] ⊓ W.0 ⊓ U.0) ⊔ ([1,0] ⊓ V.0 ⊓ U.0))
U.1 ≤ (([1,0] ⊓ V.1 ⊓ U.1) ⊔ ([0,1] ⊓ W.0 ⊓ U.0 ⊓ U.1) ⊔ ([1,0] ⊓ V.0 ⊓ U.0 ⊓ U.1))
U.2 ≤ (([0,1] ⊓ W.1 ⊓ U.2) ⊔ ([0,1] ⊓ W.0 ⊓ U.0 ⊓ U.2) ⊔ ([1,0] ⊓ V.0 ⊓ U.0 ⊓ U.2))
V.0 ≤ ([1,0] ⊓ V.0 ⊓ U.0)
V.1 ≤ (([1,0] ⊓ V.1 ⊓ U.1) ⊔ ([1,0] ⊓ V.0 ⊓ V.1 ⊓ U.0))
W.0 ≤ ([0,1] ⊓ W.0 ⊓ U.0)
W.1 ≤ (([0,1] ⊓ W.1 ⊓ U.2) ⊔ ([0,1] ⊓ W.0 ⊓ W.1 ⊓ U.0))
C1.0 = ([0,1] ⊓ F.0 ⊓ H.0)
C1.1 = ⊤
C2.0 = ⊥
C2.1 = [1,0]

Infer2: Normalized kinds:
H: {0 ↦ ([0,1] ⊓ F.0 ⊓ H.0), 1 ↦ H.1}
F: {0 ↦ ([0,1] ⊓ F.0 ⊓ H.0), 1 ↦ ([0,1] ⊓ F.1 ⊓ H.1)}
H2: {0 ↦ ([0,1] ⊓ F2.0 ⊓ H2.0), 1 ↦ H2.1, 2 ↦ ([0,1] ⊓ F2.1 ⊓ H2.1 ⊓ H2.2)}
F2: {0 ↦ ([0,1] ⊓ F2.0 ⊓ H2.0), 1 ↦ ([0,1] ⊓ F2.1 ⊓ H2.1), 2 ↦ (([1,0] ⊓ F2.2) ⊔ ([0,1] ⊓ F2.1 ⊓ F2.2 ⊓ H2.1 ⊓ H2.2))}
X: {0 ↦ ([0,1] ⊓ Z.0 ⊓ Y.0 ⊓ X.0), 1 ↦ ([0,1] ⊓ Z.1 ⊓ Y.1 ⊓ X.1)}
Y: {0 ↦ ([0,1] ⊓ Z.0 ⊓ Y.0 ⊓ X.0), 1 ↦ Y.1}
Z: {0 ↦ ([0,1] ⊓ Z.0 ⊓ Y.0 ⊓ X.0), 1 ↦ ([0,1] ⊓ Z.1 ⊓ Y.1)}
P: {0 ↦ ([1,0] ⊓ Q.0 ⊓ P.0), 1 ↦ ([1,1] ⊓ Q.1 ⊓ P.1), 2 ↦ ([0,1] ⊓ P.2)}
Q: {0 ↦ ([1,0] ⊓ Q.0 ⊓ P.0), 1 ↦ Q.1}
M: {0 ↦ ([0,1] ⊓ M.0), 1 ↦ M.1}
U: {0 ↦ (([0,1] ⊓ W.0 ⊓ U.0) ⊔ ([1,0] ⊓ V.0 ⊓ U.0)), 1 ↦ ([1,0] ⊓ V.1 ⊓ U.1), 2 ↦ ([0,1] ⊓ W.1 ⊓ U.2)}
V: {0 ↦ ([1,0] ⊓ V.0 ⊓ U.0), 1 ↦ ([1,0] ⊓ V.1 ⊓ U.1)}
W: {0 ↦ ([0,1] ⊓ W.0 ⊓ U.0), 1 ↦ ([0,1] ⊓ W.1 ⊓ U.2)}
C1: {0 ↦ ([0,1] ⊓ F.0 ⊓ H.0), 1 ↦ ⊤}
C2: {0 ↦ ⊥, 1 ↦ [1,0]}

Normalized kinds:
H: {0 ↦ ([0,1] ⊓ F.0 ⊓ H.0), 1 ↦ H.1}
F: {0 ↦ ([0,1] ⊓ F.0 ⊓ H.0), 1 ↦ ([0,1] ⊓ F.1 ⊓ H.1)}
H2: {0 ↦ ([0,1] ⊓ F2.0 ⊓ H2.0), 1 ↦ H2.1, 2 ↦ ([0,1] ⊓ F2.1 ⊓ H2.1 ⊓ H2.2)}
F2: {0 ↦ ([0,1] ⊓ F2.0 ⊓ H2.0), 1 ↦ ([0,1] ⊓ F2.1 ⊓ H2.1), 2 ↦ (([1,0] ⊓ F2.2) ⊔ ([0,1] ⊓ F2.1 ⊓ F2.2 ⊓ H2.1 ⊓ H2.2))}
X: {0 ↦ ([0,1] ⊓ X.0 ⊓ Y.0 ⊓ Z.0), 1 ↦ ([0,1] ⊓ X.1 ⊓ Y.1 ⊓ Z.1)}
Y: {0 ↦ ([0,1] ⊓ X.0 ⊓ Y.0 ⊓ Z.0), 1 ↦ Y.1}
Z: {0 ↦ ([0,1] ⊓ X.0 ⊓ Y.0 ⊓ Z.0), 1 ↦ ([0,1] ⊓ Y.1 ⊓ Z.1)}
P: {0 ↦ ([1,0] ⊓ P.0 ⊓ Q.0), 1 ↦ ([1,1] ⊓ P.1 ⊓ Q.1), 2 ↦ ([0,1] ⊓ P.2)}
Q: {0 ↦ ([1,0] ⊓ P.0 ⊓ Q.0), 1 ↦ Q.1}
M: {0 ↦ ([0,1] ⊓ M.0), 1 ↦ M.1}
U: {0 ↦ (([1,0] ⊓ U.0 ⊓ V.0) ⊔ ([0,1] ⊓ U.0 ⊓ W.0)), 1 ↦ ([1,0] ⊓ U.1 ⊓ V.1), 2 ↦ ([0,1] ⊓ U.2 ⊓ W.1)}
V: {0 ↦ ([1,0] ⊓ U.0 ⊓ V.0), 1 ↦ ([1,0] ⊓ U.1 ⊓ V.1)}
W: {0 ↦ ([0,1] ⊓ U.0 ⊓ W.0), 1 ↦ ([0,1] ⊓ U.2 ⊓ W.1)}
C1: {0 ↦ ([0,1] ⊓ F.0 ⊓ H.0), 1 ↦ ⊤}
C2: {0 ↦ ⊥, 1 ↦ [1,0]}

Ceil/Floor kinds:
H: ceil={0 ↦ [0,1], 1 ↦ [2,0]}, floor={0 ↦ ⊥, 1 ↦ ⊥}
F: ceil={0 ↦ [0,1], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
H2: ceil={0 ↦ [0,1], 1 ↦ [2,0], 2 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
F2: ceil={0 ↦ [0,1], 1 ↦ ⊥, 2 ↦ [1,0]}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
X: ceil={0 ↦ [0,1], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
Y: ceil={0 ↦ [0,1], 1 ↦ [2,0]}, floor={0 ↦ ⊥, 1 ↦ ⊥}
Z: ceil={0 ↦ [0,1], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
P: ceil={0 ↦ [1,0], 1 ↦ [0,1], 2 ↦ [0,1]}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
Q: ceil={0 ↦ [1,0], 1 ↦ ⊤}, floor={0 ↦ ⊥, 1 ↦ ⊥}
M: ceil={0 ↦ [0,1], 1 ↦ [2,0]}, floor={0 ↦ ⊥, 1 ↦ ⊥}
U: ceil={0 ↦ [1,1], 1 ↦ ⊥, 2 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
V: ceil={0 ↦ [1,0], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
W: ceil={0 ↦ [0,1], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
C1: ceil={0 ↦ [0,1], 1 ↦ [2,0]}, floor={0 ↦ ⊥, 1 ↦ ⊤}
C2: ceil={0 ↦ ⊥, 1 ↦ [1,0]}, floor={0 ↦ ⊥, 1 ↦ [1,0]}

LEQ relationships:
H <= C1
F <= C1
H2 <= (none)
F2 <= (none)
X <= (none)
Y <= (none)
Z <= (none)
P <= (none)
Q <= (none)
M <= (none)
U <= (none)
V <= (none)
W <= (none)
C1 <= (none)
C2 <= C1
```

## list_sum_pair.types

```
type list('a1) = unit + 'a1 * list('a1)
```

Program output:
```
Infer2: RHS as polys:
list: (list.0 ⊔ 'a1)

Infer2: linear decomposition (base + coeffs):
list: base=list.0, 'a1=⊤

Infer2: solving atoms:
list.0 = ⊥
list.1 = ⊤

Infer2: Normalized kinds:
list: {0 ↦ ⊥, 1 ↦ ⊤}

Normalized kinds:
list: {0 ↦ ⊥, 1 ↦ ⊤}

Ceil/Floor kinds:
list: ceil={0 ↦ ⊥, 1 ↦ ⊤}, floor={0 ↦ ⊥, 1 ↦ ⊤}

LEQ relationships:
list <= (none)
```

## modalities.types

```
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
Infer2: RHS as polys:
id_annot: 'a1
pair_annot: (([1,0] ⊓ 'a1) ⊔ ([1,0] ⊓ 'a2))
nested: ([1,1] ⊓ 'a1)
tree: (([0,1] ⊓ Node.0) ⊔ ([0,1] ⊓ 'a1 ⊓ Node.1) ⊔ ([0,1] ⊓ Node.2 ⊓ tree.0) ⊔ ([0,1] ⊓ 'a1 ⊓ Node.2 ⊓ tree.1))
both: 'a1
id_bot: ⊥
mix_sum: ([1,1] ⊓ 'a1)
mix_pair: (([1,0] ⊓ 'a1) ⊔ ([0,1] ⊓ 'a2))
outer_vs_inner: ([1,0] ⊓ 'a1)
inner_vs_outer: ([1,0] ⊓ 'a1)
list_ann: (([1,0] ⊓ 'a1) ⊔ ([1,0] ⊓ list_ann.0))
two_axes: (([0,1] ⊓ G.0) ⊔ ([0,1] ⊓ 'a1 ⊓ G.1))
deeply: (([0,1] ⊓ G.0) ⊔ ([1,0] ⊓ F.0) ⊔ ([0,1] ⊓ 'a1 ⊓ G.1) ⊔ ([1,0] ⊓ 'a1 ⊓ F.1))
list: ('a1 ⊔ list.0)
list_inner: (list.0 ⊔ ([1,0] ⊓ 'a1 ⊓ list.1))
list_outer: (([1,0] ⊓ list.0) ⊔ ([1,0] ⊓ 'a1 ⊓ list.1))
list2: (cons.0 ⊔ nil.0 ⊔ ('a1 ⊓ cons.1) ⊔ (cons.2 ⊓ list2.0) ⊔ ('a1 ⊓ cons.2 ⊓ list2.1))
list2_inner: (list2.0 ⊔ ([1,0] ⊓ 'a1 ⊓ list2.1))
list2_outer: (([1,0] ⊓ list2.0) ⊔ ([1,0] ⊓ 'a1 ⊓ list2.1))
modal_plus: ([1,0] ⊔ 'a1)
modal_pair: ([1,0] ⊔ 'a1)

Infer2: linear decomposition (base + coeffs):
id_annot: base=⊥, 'a1=⊤
pair_annot: base=⊥, 'a1=[1,0], 'a2=[1,0]
nested: base=⊥, 'a1=[1,1]
tree: base=(([0,1] ⊓ Node.0) ⊔ ([0,1] ⊓ Node.2 ⊓ tree.0)), 'a1=(([0,1] ⊓ Node.1) ⊔ ([0,1] ⊓ Node.2 ⊓ tree.1))
both: base=⊥, 'a1=⊤
id_bot: base=⊥, 'a1=⊥
mix_sum: base=⊥, 'a1=[1,1]
mix_pair: base=⊥, 'a1=[1,0], 'a2=[0,1]
outer_vs_inner: base=⊥, 'a1=[1,0]
inner_vs_outer: base=⊥, 'a1=[1,0]
list_ann: base=([1,0] ⊓ list_ann.0), 'a1=[1,0]
two_axes: base=([0,1] ⊓ G.0), 'a1=([0,1] ⊓ G.1)
deeply: base=(([0,1] ⊓ G.0) ⊔ ([1,0] ⊓ F.0)), 'a1=(([0,1] ⊓ G.1) ⊔ ([1,0] ⊓ F.1))
list: base=list.0, 'a1=⊤
list_inner: base=list.0, 'a1=([1,0] ⊓ list.1)
list_outer: base=([1,0] ⊓ list.0), 'a1=([1,0] ⊓ list.1)
list2: base=(cons.0 ⊔ nil.0 ⊔ (cons.2 ⊓ list2.0)), 'a1=(cons.1 ⊔ (cons.2 ⊓ list2.1))
list2_inner: base=list2.0, 'a1=([1,0] ⊓ list2.1)
list2_outer: base=([1,0] ⊓ list2.0), 'a1=([1,0] ⊓ list2.1)
modal_plus: base=[1,0], 'a1=⊤
modal_pair: base=[1,0], 'a1=⊤

Infer2: solving atoms:
id_annot.0 = ⊥
id_annot.1 = ⊤
pair_annot.0 = ⊥
pair_annot.1 = [1,0]
pair_annot.2 = [1,0]
nested.0 = ⊥
nested.1 = [1,1]
tree.0 = ([0,1] ⊓ Node.0)
tree.1 = ([0,1] ⊓ Node.1)
both.0 = ⊥
both.1 = ⊤
id_bot.0 = ⊥
id_bot.1 = ⊥
mix_sum.0 = ⊥
mix_sum.1 = [1,1]
mix_pair.0 = ⊥
mix_pair.1 = [1,0]
mix_pair.2 = [0,1]
outer_vs_inner.0 = ⊥
outer_vs_inner.1 = [1,0]
inner_vs_outer.0 = ⊥
inner_vs_outer.1 = [1,0]
list_ann.0 = ⊥
list_ann.1 = [1,0]
two_axes.0 = ([0,1] ⊓ G.0)
two_axes.1 = ([0,1] ⊓ G.1)
deeply.0 = (([0,1] ⊓ G.0) ⊔ ([1,0] ⊓ F.0))
deeply.1 = (([0,1] ⊓ G.1) ⊔ ([1,0] ⊓ F.1))
list.0 = ⊥
list.1 = ⊤
list_inner.0 = ⊥
list_inner.1 = [1,0]
list_outer.0 = ⊥
list_outer.1 = [1,0]
list2.0 = (cons.0 ⊔ nil.0)
list2.1 = cons.1
list2_inner.0 = (cons.0 ⊔ nil.0)
list2_inner.1 = ([1,0] ⊓ cons.1)
list2_outer.0 = (([1,0] ⊓ cons.0) ⊔ ([1,0] ⊓ nil.0))
list2_outer.1 = ([1,0] ⊓ cons.1)
modal_plus.0 = [1,0]
modal_plus.1 = ⊤
modal_pair.0 = [1,0]
modal_pair.1 = ⊤

Infer2: Normalized kinds:
id_annot: {0 ↦ ⊥, 1 ↦ ⊤}
pair_annot: {0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [1,0]}
nested: {0 ↦ ⊥, 1 ↦ [1,1]}
tree: {0 ↦ ([0,1] ⊓ Node.0), 1 ↦ ([0,1] ⊓ Node.1)}
both: {0 ↦ ⊥, 1 ↦ ⊤}
id_bot: {0 ↦ ⊥, 1 ↦ ⊥}
mix_sum: {0 ↦ ⊥, 1 ↦ [1,1]}
mix_pair: {0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [0,1]}
outer_vs_inner: {0 ↦ ⊥, 1 ↦ [1,0]}
inner_vs_outer: {0 ↦ ⊥, 1 ↦ [1,0]}
list_ann: {0 ↦ ⊥, 1 ↦ [1,0]}
two_axes: {0 ↦ ([0,1] ⊓ G.0), 1 ↦ ([0,1] ⊓ G.1)}
deeply: {0 ↦ (([0,1] ⊓ G.0) ⊔ ([1,0] ⊓ F.0)), 1 ↦ (([0,1] ⊓ G.1) ⊔ ([1,0] ⊓ F.1))}
list: {0 ↦ ⊥, 1 ↦ ⊤}
list_inner: {0 ↦ ⊥, 1 ↦ [1,0]}
list_outer: {0 ↦ ⊥, 1 ↦ [1,0]}
list2: {0 ↦ (cons.0 ⊔ nil.0), 1 ↦ cons.1}
list2_inner: {0 ↦ (cons.0 ⊔ nil.0), 1 ↦ ([1,0] ⊓ cons.1)}
list2_outer: {0 ↦ (([1,0] ⊓ cons.0) ⊔ ([1,0] ⊓ nil.0)), 1 ↦ ([1,0] ⊓ cons.1)}
modal_plus: {0 ↦ [1,0], 1 ↦ ⊤}
modal_pair: {0 ↦ [1,0], 1 ↦ ⊤}

Normalized kinds:
id_annot: {0 ↦ ⊥, 1 ↦ ⊤}
pair_annot: {0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [1,0]}
nested: {0 ↦ ⊥, 1 ↦ [1,1]}
tree: {0 ↦ ([0,1] ⊓ Node.0), 1 ↦ ([0,1] ⊓ Node.1)}
both: {0 ↦ ⊥, 1 ↦ ⊤}
id_bot: {0 ↦ ⊥, 1 ↦ ⊥}
mix_sum: {0 ↦ ⊥, 1 ↦ [1,1]}
mix_pair: {0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [0,1]}
outer_vs_inner: {0 ↦ ⊥, 1 ↦ [1,0]}
inner_vs_outer: {0 ↦ ⊥, 1 ↦ [1,0]}
list_ann: {0 ↦ ⊥, 1 ↦ [1,0]}
two_axes: {0 ↦ ([0,1] ⊓ G.0), 1 ↦ ([0,1] ⊓ G.1)}
deeply: {0 ↦ (([1,0] ⊓ F.0) ⊔ ([0,1] ⊓ G.0)), 1 ↦ (([1,0] ⊓ F.1) ⊔ ([0,1] ⊓ G.1))}
list: {0 ↦ ⊥, 1 ↦ ⊤}
list_inner: {0 ↦ ⊥, 1 ↦ [1,0]}
list_outer: {0 ↦ ⊥, 1 ↦ [1,0]}
list2: {0 ↦ (cons.0 ⊔ nil.0), 1 ↦ cons.1}
list2_inner: {0 ↦ (cons.0 ⊔ nil.0), 1 ↦ ([1,0] ⊓ cons.1)}
list2_outer: {0 ↦ (([1,0] ⊓ cons.0) ⊔ ([1,0] ⊓ nil.0)), 1 ↦ ([1,0] ⊓ cons.1)}
modal_plus: {0 ↦ [1,0], 1 ↦ ⊤}
modal_pair: {0 ↦ [1,0], 1 ↦ ⊤}

Ceil/Floor kinds:
id_annot: ceil={0 ↦ ⊥, 1 ↦ ⊤}, floor={0 ↦ ⊥, 1 ↦ ⊤}
pair_annot: ceil={0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [1,0]}, floor={0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [1,0]}
nested: ceil={0 ↦ ⊥, 1 ↦ [1,1]}, floor={0 ↦ ⊥, 1 ↦ [1,1]}
tree: ceil={0 ↦ [0,1], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
both: ceil={0 ↦ ⊥, 1 ↦ ⊤}, floor={0 ↦ ⊥, 1 ↦ ⊤}
id_bot: ceil={0 ↦ ⊥, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
mix_sum: ceil={0 ↦ ⊥, 1 ↦ [1,1]}, floor={0 ↦ ⊥, 1 ↦ [1,1]}
mix_pair: ceil={0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [0,1]}, floor={0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [0,1]}
outer_vs_inner: ceil={0 ↦ ⊥, 1 ↦ [1,0]}, floor={0 ↦ ⊥, 1 ↦ [1,0]}
inner_vs_outer: ceil={0 ↦ ⊥, 1 ↦ [1,0]}, floor={0 ↦ ⊥, 1 ↦ [1,0]}
list_ann: ceil={0 ↦ ⊥, 1 ↦ [1,0]}, floor={0 ↦ ⊥, 1 ↦ [1,0]}
two_axes: ceil={0 ↦ [0,1], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
deeply: ceil={0 ↦ [1,1], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
list: ceil={0 ↦ ⊥, 1 ↦ ⊤}, floor={0 ↦ ⊥, 1 ↦ ⊤}
list_inner: ceil={0 ↦ ⊥, 1 ↦ [1,0]}, floor={0 ↦ ⊥, 1 ↦ [1,0]}
list_outer: ceil={0 ↦ ⊥, 1 ↦ [1,0]}, floor={0 ↦ ⊥, 1 ↦ [1,0]}
list2: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
list2_inner: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
list2_outer: ceil={0 ↦ [1,0], 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
modal_plus: ceil={0 ↦ [1,0], 1 ↦ ⊤}, floor={0 ↦ [1,0], 1 ↦ ⊤}
modal_pair: ceil={0 ↦ [1,0], 1 ↦ ⊤}, floor={0 ↦ [1,0], 1 ↦ ⊤}

LEQ relationships:
id_annot <= both, list, modal_plus, modal_pair
pair_annot <= (none)
nested <= id_annot, both, mix_sum, list, modal_plus, modal_pair
tree <= (none)
both <= id_annot, list, modal_plus, modal_pair
id_bot <= id_annot, nested, tree, both, mix_sum, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair
mix_sum <= id_annot, nested, both, list, modal_plus, modal_pair
mix_pair <= (none)
outer_vs_inner <= id_annot, nested, both, mix_sum, inner_vs_outer, list_ann, list, list_inner, list_outer, modal_plus, modal_pair
inner_vs_outer <= id_annot, nested, both, mix_sum, outer_vs_inner, list_ann, list, list_inner, list_outer, modal_plus, modal_pair
list_ann <= id_annot, nested, both, mix_sum, outer_vs_inner, inner_vs_outer, list, list_inner, list_outer, modal_plus, modal_pair
two_axes <= deeply
deeply <= (none)
list <= id_annot, both, modal_plus, modal_pair
list_inner <= id_annot, nested, both, mix_sum, outer_vs_inner, inner_vs_outer, list_ann, list, list_outer, modal_plus, modal_pair
list_outer <= id_annot, nested, both, mix_sum, outer_vs_inner, inner_vs_outer, list_ann, list, list_inner, modal_plus, modal_pair
list2 <= (none)
list2_inner <= list2
list2_outer <= list2, list2_inner, modal_plus, modal_pair
modal_plus <= modal_pair
modal_pair <= modal_plus
```

## modals.types

```
type foo() = [1,0] * [0,1]
type bar() = foo() @@ [1,0]
```

Program output:
```
Infer2: RHS as polys:
foo: [1,1]
bar: ([1,0] ⊓ foo.0)

Infer2: linear decomposition (base + coeffs):
foo: base=[1,1]
bar: base=([1,0] ⊓ foo.0)

Infer2: solving atoms:
foo.0 = [1,1]
bar.0 = [1,0]

Infer2: Normalized kinds:
foo: {0 ↦ [1,1]}
bar: {0 ↦ [1,0]}

Normalized kinds:
foo: {0 ↦ [1,1]}
bar: {0 ↦ [1,0]}

Ceil/Floor kinds:
foo: ceil={0 ↦ [1,1]}, floor={0 ↦ [1,1]}
bar: ceil={0 ↦ [1,0]}, floor={0 ↦ [1,0]}

LEQ relationships:
foo <= (none)
bar <= foo
```

## mutual.types

```
type oddlist('a1) = unit + cons('a1, evenlist('a1))
type evenlist('a1) = unit + cons('a1, oddlist('a1))
```

Program output:
```
Infer2: RHS as polys:
oddlist: (cons.0 ⊔ (cons.1 ⊓ 'a1) ⊔ (cons.2 ⊓ evenlist.0) ⊔ ('a1 ⊓ cons.2 ⊓ evenlist.1))
evenlist: (cons.0 ⊔ (cons.1 ⊓ 'a1) ⊔ (cons.2 ⊓ oddlist.0) ⊔ ('a1 ⊓ cons.2 ⊓ oddlist.1))

Infer2: linear decomposition (base + coeffs):
oddlist: base=(cons.0 ⊔ (cons.2 ⊓ evenlist.0)), 'a1=(cons.1 ⊔ (cons.2 ⊓ evenlist.1))
evenlist: base=(cons.0 ⊔ (cons.2 ⊓ oddlist.0)), 'a1=(cons.1 ⊔ (cons.2 ⊓ oddlist.1))

Infer2: solving atoms:
oddlist.0 = cons.0
oddlist.1 = cons.1
evenlist.0 = cons.0
evenlist.1 = cons.1

Infer2: Normalized kinds:
oddlist: {0 ↦ cons.0, 1 ↦ cons.1}
evenlist: {0 ↦ cons.0, 1 ↦ cons.1}

Normalized kinds:
oddlist: {0 ↦ cons.0, 1 ↦ cons.1}
evenlist: {0 ↦ cons.0, 1 ↦ cons.1}

Ceil/Floor kinds:
oddlist: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
evenlist: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}

LEQ relationships:
oddlist <= evenlist
evenlist <= oddlist
```

## portable.types

```
type list('a1) = unit + 'a1 * list('a1)
type lily('a1) = list(portable('a1) * lily(list('a1)))
type tulip('a1) = unit + 'a1 * tulip(portable('a1 * 'a1))
type orchid('a1) = unit + portable('a1) * orchid('a1 * 'a1)
```

Program output:
```
Infer2: RHS as polys:
list: (list.0 ⊔ 'a1)
lily: (list.0 ⊔ (list.1 ⊓ lily.0) ⊔ (list.1 ⊓ portable.0) ⊔ (list.1 ⊓ 'a1 ⊓ lily.1) ⊔ (list.1 ⊓ 'a1 ⊓ portable.1))
tulip: ('a1 ⊔ tulip.0 ⊔ (portable.0 ⊓ tulip.1))
orchid: (portable.0 ⊔ orchid.0 ⊔ ('a1 ⊓ portable.1) ⊔ ('a1 ⊓ orchid.1))

Infer2: linear decomposition (base + coeffs):
list: base=list.0, 'a1=⊤
lily: base=(list.0 ⊔ (list.1 ⊓ lily.0) ⊔ (list.1 ⊓ portable.0)), 'a1=((list.1 ⊓ lily.1) ⊔ (list.1 ⊓ portable.1))
tulip: base=(tulip.0 ⊔ (portable.0 ⊓ tulip.1)), 'a1=⊤
orchid: base=(portable.0 ⊔ orchid.0), 'a1=(portable.1 ⊔ orchid.1)

Infer2: solving atoms:
list.0 = ⊥
list.1 = ⊤
lily.0 = portable.0
lily.1 = portable.1
tulip.0 = portable.0
tulip.1 = ⊤
orchid.0 = portable.0
orchid.1 = portable.1

Infer2: Normalized kinds:
list: {0 ↦ ⊥, 1 ↦ ⊤}
lily: {0 ↦ portable.0, 1 ↦ portable.1}
tulip: {0 ↦ portable.0, 1 ↦ ⊤}
orchid: {0 ↦ portable.0, 1 ↦ portable.1}

Normalized kinds:
list: {0 ↦ ⊥, 1 ↦ ⊤}
lily: {0 ↦ portable.0, 1 ↦ portable.1}
tulip: {0 ↦ portable.0, 1 ↦ ⊤}
orchid: {0 ↦ portable.0, 1 ↦ portable.1}

Ceil/Floor kinds:
list: ceil={0 ↦ ⊥, 1 ↦ ⊤}, floor={0 ↦ ⊥, 1 ↦ ⊤}
lily: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
tulip: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊤}
orchid: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}

LEQ relationships:
list <= tulip
lily <= tulip, orchid
tulip <= (none)
orchid <= lily, tulip
```

## ref.types

```
type foo('a1) = portended(ref('a1))
```

Program output:
```
Infer2: RHS as polys:
foo: (portended.0 ⊔ (portended.1 ⊓ ref.0) ⊔ (portended.1 ⊓ ref.1 ⊓ 'a1))

Infer2: linear decomposition (base + coeffs):
foo: base=(portended.0 ⊔ (portended.1 ⊓ ref.0)), 'a1=(portended.1 ⊓ ref.1)

Infer2: solving atoms:
foo.0 = (portended.0 ⊔ (portended.1 ⊓ ref.0))
foo.1 = (portended.1 ⊓ ref.1)

Infer2: Normalized kinds:
foo: {0 ↦ (portended.0 ⊔ (portended.1 ⊓ ref.0)), 1 ↦ (portended.1 ⊓ ref.1)}

Normalized kinds:
foo: {0 ↦ (portended.0 ⊔ (portended.1 ⊓ ref.0)), 1 ↦ (portended.1 ⊓ ref.1)}

Ceil/Floor kinds:
foo: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}

LEQ relationships:
foo <= (none)
```

## rose_sum_pair.types

```
type list('a1) = unit + 'a1 * list('a1)
type rose('a1) = list(rose(list('a1)))
type lily('a1) = list('a1 * lily(list('a1)))
```

Program output:
```
Infer2: RHS as polys:
list: (list.0 ⊔ 'a1)
rose: (list.0 ⊔ (list.1 ⊓ rose.0) ⊔ (list.1 ⊓ 'a1 ⊓ rose.1))
lily: (list.0 ⊔ (list.1 ⊓ 'a1) ⊔ (list.1 ⊓ lily.0))

Infer2: linear decomposition (base + coeffs):
list: base=list.0, 'a1=⊤
rose: base=(list.0 ⊔ (list.1 ⊓ rose.0)), 'a1=(list.1 ⊓ rose.1)
lily: base=(list.0 ⊔ (list.1 ⊓ lily.0)), 'a1=list.1

Infer2: solving atoms:
list.0 = ⊥
list.1 = ⊤
rose.0 = ⊥
rose.1 = ⊥
lily.0 = ⊥
lily.1 = ⊤

Infer2: Normalized kinds:
list: {0 ↦ ⊥, 1 ↦ ⊤}
rose: {0 ↦ ⊥, 1 ↦ ⊥}
lily: {0 ↦ ⊥, 1 ↦ ⊤}

Normalized kinds:
list: {0 ↦ ⊥, 1 ↦ ⊤}
rose: {0 ↦ ⊥, 1 ↦ ⊥}
lily: {0 ↦ ⊥, 1 ↦ ⊤}

Ceil/Floor kinds:
list: ceil={0 ↦ ⊥, 1 ↦ ⊤}, floor={0 ↦ ⊥, 1 ↦ ⊤}
rose: ceil={0 ↦ ⊥, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
lily: ceil={0 ↦ ⊥, 1 ↦ ⊤}, floor={0 ↦ ⊥, 1 ↦ ⊤}

LEQ relationships:
list <= lily
rose <= list, lily
lily <= list
```

## zipper.types

```
type list('a1) = unit + cons('a1, list('a1))
type ctx('a1) = unit + down('a1, ctx('a1))
type zipper('a1) = (ctx('a1) * list('a1))
```

Program output:
```
Infer2: RHS as polys:
list: (cons.0 ⊔ (cons.1 ⊓ 'a1) ⊔ (cons.2 ⊓ list.0) ⊔ ('a1 ⊓ cons.2 ⊓ list.1))
ctx: (down.0 ⊔ ('a1 ⊓ down.1) ⊔ (down.2 ⊓ ctx.0) ⊔ ('a1 ⊓ down.2 ⊓ ctx.1))
zipper: (list.0 ⊔ ctx.0 ⊔ ('a1 ⊓ list.1) ⊔ ('a1 ⊓ ctx.1))

Infer2: linear decomposition (base + coeffs):
list: base=(cons.0 ⊔ (cons.2 ⊓ list.0)), 'a1=(cons.1 ⊔ (cons.2 ⊓ list.1))
ctx: base=(down.0 ⊔ (down.2 ⊓ ctx.0)), 'a1=(down.1 ⊔ (down.2 ⊓ ctx.1))
zipper: base=(list.0 ⊔ ctx.0), 'a1=(list.1 ⊔ ctx.1)

Infer2: solving atoms:
list.0 = cons.0
list.1 = cons.1
ctx.0 = down.0
ctx.1 = down.1
zipper.0 = (cons.0 ⊔ down.0)
zipper.1 = (cons.1 ⊔ down.1)

Infer2: Normalized kinds:
list: {0 ↦ cons.0, 1 ↦ cons.1}
ctx: {0 ↦ down.0, 1 ↦ down.1}
zipper: {0 ↦ (cons.0 ⊔ down.0), 1 ↦ (cons.1 ⊔ down.1)}

Normalized kinds:
list: {0 ↦ cons.0, 1 ↦ cons.1}
ctx: {0 ↦ down.0, 1 ↦ down.1}
zipper: {0 ↦ (cons.0 ⊔ down.0), 1 ↦ (cons.1 ⊔ down.1)}

Ceil/Floor kinds:
list: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
ctx: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}
zipper: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}

LEQ relationships:
list <= zipper
ctx <= zipper
zipper <= (none)
```
