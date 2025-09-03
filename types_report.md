# jkinds Types Report

Generated: 2025-09-03 17:27:29 UTC

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
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
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

Timing: Infer2: 0.258 ms, Infer4: 0.245 ms, Infer5: 0.294 ms, Infer6: 0.149 ms, Infer8: 0.122 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
none <=: [none, two, G]
some <=: [some, two, G]
leaf <=: [leaf, two, G]
node <=: [node, two, G]
foo <=: [foo, two, G]
bar <=: [bar, two, G]
baz <=: [baz, two, G]
one <=: [one, two, G]
two <=: [two, G]
maybe <=: [two, maybe, G]
wrap <=: [two, wrap, G]
wrap2 <=: [two, wrap2, G]
pairish <=: [two, pairish, G]
treeA <=: [two, treeA, G]
H <=: [two, H, G]
F <=: [two, F, G]
G <=: [two, G]
ROUND_UP (Infer6 & Infer8)
none: [2,1]
some: [2,1]
leaf: [2,1]
node: [2,1]
foo: [2,1]
bar: [2,1]
baz: [2,1]
one: [2,1]
two: [0,0]
maybe: [2,1]
wrap: [2,1]
wrap2: [2,1]
pairish: [2,1]
treeA: [2,1]
H: [2,1]
F: [2,1]
G: [0,0]
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
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
t1: {0 ↦ t1.0, 1 ↦ t1.1}
t2: {0 ↦ t2.0, 1 ↦ t2.1}
foo1: {0 ↦ t1.0 ⊔ t2.0, 1 ↦ t1.1 ⊓ t2.1}
foo2: {0 ↦ t1.0 ⊔ t2.0, 1 ↦ t1.1 ⊓ t2.1}
bar: {0 ↦ t1.0 ⊔ t2.0, 1 ↦ ⊤}

Timing: Infer2: 0.154 ms, Infer4: 0.089 ms, Infer5: 0.144 ms, Infer6: 0.074 ms, Infer8: 0.077 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
t1 <=: [t1, foo1, foo2, bar]
t2 <=: [t2, foo1, foo2, bar]
foo1 <=: [foo1, foo2, bar]
foo2 <=: [foo1, foo2, bar]
bar <=: [foo1, foo2, bar]
ROUND_UP (Infer6 & Infer8)
t1: [2,1]
t2: [2,1]
foo1: [0,0]
foo2: [0,0]
bar: [0,0]
```

## btree.types

```
type leaf('a1) : [2,1]
type node('a1,'a2) : [2,1]

type btree('a1) = (leaf('a1) + node(btree('a1), btree('a1)))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
leaf: {0 ↦ leaf.0, 1 ↦ leaf.1}
node: {0 ↦ node.0, 1 ↦ node.1, 2 ↦ node.2}
btree: {0 ↦ leaf.0 ⊔ node.0, 1 ↦ leaf.1}

Timing: Infer2: 0.099 ms, Infer4: 0.071 ms, Infer5: 0.099 ms, Infer6: 0.055 ms, Infer8: 0.042 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
leaf <=: [leaf, btree]
node <=: [node, btree]
btree <=: [btree]
ROUND_UP (Infer6 & Infer8)
leaf: [2,1]
node: [2,1]
btree: [0,0]
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
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
A: {0 ↦ ⊥, 1 ↦ A.1}
B: {0 ↦ ⊥, 1 ↦ [0,1] ⊓ A.1 ⊓ B.1 ⊓ C.1}
C: {0 ↦ ⊥, 1 ↦ A.1 ⊓ C.1}
K: {0 ↦ ⊥, 1 ↦ ⊤, 2 ↦ [0,1] ⊓ A.1 ⊓ B.1 ⊓ C.1}
M2: {0 ↦ [0,1], 1 ↦ [2,0]}
CN2: {0 ↦ [0,1], 1 ↦ [2,0] ⊓ A.1, 2 ↦ ⊥}

Timing: Infer2: 0.192 ms, Infer4: 0.144 ms, Infer5: 0.169 ms, Infer6: 0.103 ms, Infer8: 0.087 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
A <=: [A, K, M2, CN2]
B <=: [B, K, M2, CN2]
C <=: [C, K, M2, CN2]
K <=: [K, M2, CN2]
M2 <=: [K, M2, CN2]
CN2 <=: [K, M2, CN2]
ROUND_UP (Infer6 & Infer8)
A: [2,1]
B: [2,1]
C: [2,1]
K: [0,0]
M2: [0,0]
CN2: [0,0]
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
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
A: {0 ↦ ⊥, 1 ↦ A.1}
B: {0 ↦ ⊥, 1 ↦ B.1}
C: {0 ↦ ⊥, 1 ↦ C.1}
K: {0 ↦ ⊥, 1 ↦ ⊤, 2 ↦ B.1}
M2: {0 ↦ [0,1], 1 ↦ [2,0]}
CN2A: {0 ↦ ⊥, 1 ↦ A.1, 2 ↦ B.1}
CN2B: {0 ↦ [0,1], 1 ↦ ⊥, 2 ↦ [2,0] ⊓ B.1}
CN2: {0 ↦ [0,1], 1 ↦ [2,0] ⊓ A.1, 2 ↦ [2,0] ⊓ B.1}

Timing: Infer2: 0.227 ms, Infer4: 0.161 ms, Infer5: 0.145 ms, Infer6: 0.089 ms, Infer8: 0.077 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
A <=: [A, K, M2, CN2A, CN2B, CN2]
B <=: [B, K, M2, CN2A, CN2B, CN2]
C <=: [C, K, M2, CN2A, CN2B, CN2]
K <=: [K, M2, CN2A, CN2B, CN2]
M2 <=: [K, M2, CN2A, CN2B, CN2]
CN2A <=: [K, M2, CN2A, CN2B, CN2]
CN2B <=: [K, M2, CN2A, CN2B, CN2]
CN2 <=: [K, M2, CN2A, CN2B, CN2]
ROUND_UP (Infer6 & Infer8)
A: [2,1]
B: [2,1]
C: [2,1]
K: [0,0]
M2: [0,0]
CN2A: [0,0]
CN2B: [0,0]
CN2: [0,0]
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
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
A: {0 ↦ ⊥, 1 ↦ A.1}
B: {0 ↦ ⊥, 1 ↦ B.1}
M2: {0 ↦ [0,1], 1 ↦ [2,0]}
CN: {0 ↦ [0,1], 1 ↦ [2,0] ⊓ A.1, 2 ↦ [2,0] ⊓ B.1}

Timing: Infer2: 0.089 ms, Infer4: 0.067 ms, Infer5: 0.066 ms, Infer6: 0.048 ms, Infer8: 0.039 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
A <=: [A, M2, CN]
B <=: [B, M2, CN]
M2 <=: [M2, CN]
CN <=: [M2, CN]
ROUND_UP (Infer6 & Infer8)
A: [2,1]
B: [2,1]
M2: [0,0]
CN: [0,0]
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
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
A: {0 ↦ ⊥, 1 ↦ A.1}
B: {0 ↦ ⊥, 1 ↦ B.1}
M2: {0 ↦ [0,1], 1 ↦ [2,0]}
CN3: {0 ↦ [0,1], 1 ↦ [2,0] ⊓ A.1, 2 ↦ [2,0] ⊓ B.1}

Timing: Infer2: 0.098 ms, Infer4: 0.073 ms, Infer5: 0.077 ms, Infer6: 0.050 ms, Infer8: 0.041 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
A <=: [A, M2, CN3]
B <=: [B, M2, CN3]
M2 <=: [M2, CN3]
CN3 <=: [M2, CN3]
ROUND_UP (Infer6 & Infer8)
A: [2,1]
B: [2,1]
M2: [0,0]
CN3: [0,0]
```

## cn_mix.types

```
type A('a1) : 'a1
type My('a1) = [0,1] * 'a1

type CNmix('a1,'a2) = A('a1) + My('a2)
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
A: {0 ↦ ⊥, 1 ↦ A.1}
My: {0 ↦ [0,1], 1 ↦ [2,0]}
CNmix: {0 ↦ [0,1], 1 ↦ [2,0] ⊓ A.1, 2 ↦ [2,0]}

Timing: Infer2: 0.063 ms, Infer4: 0.046 ms, Infer5: 0.044 ms, Infer6: 0.040 ms, Infer8: 0.035 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
A <=: [A, My, CNmix]
My <=: [My, CNmix]
CNmix <=: [My, CNmix]
ROUND_UP (Infer6 & Infer8)
A: [2,1]
My: [0,0]
CNmix: [0,0]
```

## cn_novar.types

```
type A('a1) : 'a1
type M2('a1) = [0,1] * 'a1

type CN0('a1,'a2) = A('a1) + M2(unit)
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
A: {0 ↦ ⊥, 1 ↦ A.1}
M2: {0 ↦ [0,1], 1 ↦ [2,0]}
CN0: {0 ↦ [0,1], 1 ↦ [2,0] ⊓ A.1, 2 ↦ ⊥}

Timing: Infer2: 0.054 ms, Infer4: 0.040 ms, Infer5: 0.038 ms, Infer6: 0.038 ms, Infer8: 0.024 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
A <=: [A, M2, CN0]
M2 <=: [M2, CN0]
CN0 <=: [M2, CN0]
ROUND_UP (Infer6 & Infer8)
A: [2,1]
M2: [0,0]
CN0: [0,0]
```

## cn_simple.types

```
type A('a1) = 'a1
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
A: {0 ↦ ⊥, 1 ↦ ⊤}

Timing: Infer2: 0.015 ms, Infer4: 0.010 ms, Infer5: 0.007 ms, Infer6: 0.011 ms, Infer8: 0.006 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
A <=: [A]
ROUND_UP (Infer6 & Infer8)
A: [2,1]
```

## cyclic.types

```
type L('a1) : mu 'b1. ('a1 + 'b1)

type Nested('a1) : mu 'b1. ((mu 'b2. ('b1 + 'b2)) + 'a1)

type Annot('a1) : mu 'b1. (('a1 @@ [1,0]) + ('b1 @@ [0,1]))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
L: {0 ↦ ⊥, 1 ↦ L.1}
Nested: {0 ↦ ⊥, 1 ↦ Nested.1}
Annot: {0 ↦ ⊥, 1 ↦ [1,0] ⊓ Annot.1}

Timing: Infer2: 0.062 ms, Infer4: 0.047 ms, Infer5: 0.040 ms, Infer6: 0.036 ms, Infer8: 0.027 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
L <=: [L]
Nested <=: [Nested]
Annot <=: [Annot]
ROUND_UP (Infer6 & Infer8)
L: [2,1]
Nested: [2,1]
Annot: [2,1]
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
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
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

Timing: Infer2: 1.982 ms, Infer4: 1.766 ms, Infer5: 1.763 ms, Infer6: 1.289 ms, Infer8: 1.837 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
A <=: [A, K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
B <=: [B, K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
C <=: [C, K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
D <=: [D, K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
E <=: [E, K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
R <=: [R, K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
K <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
U1 <=: [K, U1, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
U2 <=: [K, U2, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
U3 <=: [K, U3, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
M1 <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
M2 <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
AC <=: [K, M1, M2, AC, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
PH <=: [K, M1, M2, PH, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
X1 <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
X2 <=: [K, M1, M2, X1, X2, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
S <=: [K, M1, M2, X1, S, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
T <=: [K, M1, M2, X1, T, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
U <=: [K, M1, M2, X1, U, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
Z1 <=: [K, M1, M2, X1, Z1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
DUP <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
SW <=: [K, M1, M2, X1, DUP, SW, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
ND1 <=: [K, M1, M2, X1, DUP, ND1, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
ND2 <=: [K, M1, M2, X1, DUP, ND2, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
ND3 <=: [K, M1, M2, X1, DUP, ND3, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
CN1 <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
CN2 <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
NestSTU <=: [K, M1, M2, X1, DUP, CN1, CN2, NestSTU, C3, PCX, PCY, CC, CC2, CC0, CBX]
AnnNest <=: [K, M1, M2, X1, DUP, CN1, CN2, AnnNest, C3, PCX, PCY, CC, CC2, CC0, CBX]
RecNest1 <=: [K, M1, M2, X1, DUP, CN1, CN2, RecNest1, C3, PCX, PCY, CC, CC2, CC0, CBX]
RecNest2 <=: [K, M1, M2, X1, DUP, CN1, CN2, RecNest2, C3, PCX, PCY, CC, CC2, CC0, CBX]
MixNest <=: [K, M1, M2, X1, DUP, CN1, CN2, MixNest, C3, PCX, PCY, CC, CC2, CC0, CBX]
A0 <=: [K, M1, M2, X1, DUP, CN1, CN2, A0, C3, PCX, PCY, CC, CC2, CC0, CBX]
B0 <=: [K, M1, M2, X1, DUP, CN1, CN2, B0, C3, PCX, PCY, CC, CC2, CC0, CBX]
Z3 <=: [K, M1, M2, X1, DUP, CN1, CN2, Z3, C3, PCX, PCY, CC, CC2, CC0, CBX]
C3 <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
DupSelf <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, DupSelf, PCX, PCY, CC, CC2, CC0, CBX]
PRF <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PRF, PCX, PCY, CC, CC2, CC0, CBX]
PRE <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PRE, PCX, PCY, CC, CC2, CC0, CBX]
PRC <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PRC, PCX, PCY, CC, CC2, CC0, CBX]
PRD <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PRD, PCX, PCY, CC, CC2, CC0, CBX]
PCX <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
PCY <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
PCZ <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, PCZ, CC, CC2, CC0, CBX]
PRG <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, PRG, CC, CC2, CC0, CBX]
PRH <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, PRH, CC, CC2, CC0, CBX]
CA <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CA, CC, CC2, CC0, CBX]
CC <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
CA2 <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CA2, CC2, CC0, CBX]
CC2 <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
CA0 <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CA0, CC0, CBX]
CC0 <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
CAX <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CAX, CBX]
CBX <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX]
CCX <=: [K, M1, M2, X1, DUP, CN1, CN2, C3, PCX, PCY, CC, CC2, CC0, CBX, CCX]
ROUND_UP (Infer6 & Infer8)
A: [2,1]
B: [2,1]
C: [2,1]
D: [2,1]
E: [2,1]
R: [2,1]
K: [0,0]
U1: [2,1]
U2: [2,1]
U3: [2,1]
M1: [0,0]
M2: [0,0]
AC: [2,1]
PH: [2,1]
X1: [0,0]
X2: [2,1]
S: [2,1]
T: [2,1]
U: [2,1]
Z1: [2,1]
DUP: [0,0]
SW: [2,1]
ND1: [2,1]
ND2: [2,1]
ND3: [2,1]
CN1: [0,0]
CN2: [0,0]
NestSTU: [2,1]
AnnNest: [2,1]
RecNest1: [2,1]
RecNest2: [2,1]
MixNest: [2,1]
A0: [2,1]
B0: [2,1]
Z3: [2,1]
C3: [0,0]
DupSelf: [2,1]
PRF: [2,1]
PRE: [2,1]
PRC: [2,1]
PRD: [2,1]
PCX: [0,0]
PCY: [0,0]
PCZ: [2,1]
PRG: [2,1]
PRH: [2,1]
CA: [2,1]
CC: [0,0]
CA2: [2,1]
CC2: [0,0]
CA0: [2,1]
CC0: [0,0]
CAX: [2,1]
CBX: [0,0]
CCX: [2,1]
```

## easy.types

```
type A('a1) = unit + 'a1
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
A: {0 ↦ ⊥, 1 ↦ ⊤}

Timing: Infer2: 0.019 ms, Infer4: 0.012 ms, Infer5: 0.010 ms, Infer6: 0.013 ms, Infer8: 0.007 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
A <=: [A]
ROUND_UP (Infer6 & Infer8)
A: [0,0]
```

## experiments.types

```
type list('a1) : 'a1

type rose('a1) = list('a1 * rose(list('a1)))
type lily('a1) = list(lily(list('a1)))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
list: {0 ↦ ⊥, 1 ↦ list.1}
rose: {0 ↦ ⊥, 1 ↦ list.1}
lily: {0 ↦ ⊥, 1 ↦ ⊥}

Timing: Infer2: 0.083 ms, Infer4: 0.056 ms, Infer5: 0.070 ms, Infer6: 0.045 ms, Infer8: 0.036 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
list <=: [list, rose, lily]
rose <=: [rose, lily]
lily <=: [rose, lily]
ROUND_UP (Infer6 & Infer8)
list: [2,1]
rose: [0,0]
lily: [0,0]
```

## experiments2.types

```
type list('a1) : [0,1] * 'a1

type rose('a1) = list('a1 * rose(list('a1)))
type lily('a1) = list(lily(list('a1)))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
list: {0 ↦ [0,1] ⊓ list.0, 1 ↦ list.1}
rose: {0 ↦ [0,1] ⊓ list.0, 1 ↦ list.1}
lily: {0 ↦ [0,1] ⊓ list.0, 1 ↦ ⊥}

Timing: Infer2: 0.092 ms, Infer4: 0.090 ms, Infer5: 0.100 ms, Infer6: 0.072 ms, Infer8: 0.047 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
list <=: [list, rose, lily]
rose <=: [rose, lily]
lily <=: [rose, lily]
ROUND_UP (Infer6 & Infer8)
list: [2,1]
rose: [0,0]
lily: [0,0]
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
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
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

Timing: Infer2: 0.557 ms, Infer4: 0.410 ms, Infer5: 0.354 ms, Infer6: 0.271 ms, Infer8: 0.317 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
H <=: [H, C1, C2]
F <=: [F, C1, C2]
H2 <=: [H2, C1, C2]
F2 <=: [F2, C1, C2]
X <=: [X, C1, C2]
Y <=: [Y, C1, C2]
Z <=: [Z, C1, C2]
P <=: [P, C1, C2]
Q <=: [Q, C1, C2]
M <=: [M, C1, C2]
U <=: [U, C1, C2]
V <=: [V, C1, C2]
W <=: [W, C1, C2]
C1 <=: [C1, C2]
C2 <=: [C1, C2]
ROUND_UP (Infer6 & Infer8)
H: [2,1]
F: [2,1]
H2: [2,1]
F2: [2,1]
X: [2,1]
Y: [2,1]
Z: [2,1]
P: [2,1]
Q: [2,1]
M: [2,1]
U: [2,1]
V: [2,1]
W: [2,1]
C1: [0,0]
C2: [0,0]
```

## hunt.types

```
type A('a1) = [0,1] * 'a1

type B('a1,'a2) = 'a1 + A('a2)
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
A: {0 ↦ [0,1], 1 ↦ [2,0]}
B: {0 ↦ [0,1], 1 ↦ [2,0], 2 ↦ [2,0]}

Timing: Infer2: 0.046 ms, Infer4: 0.035 ms, Infer5: 0.035 ms, Infer6: 0.031 ms, Infer8: 0.022 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
A <=: [A, B]
B <=: [A, B]
ROUND_UP (Infer6 & Infer8)
A: [0,0]
B: [0,0]
```

## list_sum_pair.types

```
type list('a1) = unit + 'a1 * list('a1)
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
list: {0 ↦ ⊥, 1 ↦ ⊤}

Timing: Infer2: 0.021 ms, Infer4: 0.016 ms, Infer5: 0.014 ms, Infer6: 0.017 ms, Infer8: 0.010 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
list <=: [list]
ROUND_UP (Infer6 & Infer8)
list: [0,0]
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
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
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

Timing: Infer2: 0.406 ms, Infer4: 0.364 ms, Infer5: 0.345 ms, Infer6: 0.237 ms, Infer8: 0.220 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
Node <=: [Node, id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
F <=: [F, id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
G <=: [G, id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
nil <=: [nil, id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
cons <=: [cons, id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
id_annot <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
pair_annot <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
nested <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
tree <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
both <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
id_bot <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
mix_sum <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
mix_pair <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
outer_vs_inner <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
inner_vs_outer <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
list_ann <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
two_axes <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
deeply <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
list <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
list_inner <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
list_outer <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
list2 <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
list2_inner <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
list2_outer <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
modal_plus <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
modal_pair <=: [id_annot, pair_annot, nested, tree, both, id_bot, mix_sum, mix_pair, outer_vs_inner, inner_vs_outer, list_ann, two_axes, deeply, list, list_inner, list_outer, list2, list2_inner, list2_outer, modal_plus, modal_pair]
ROUND_UP (Infer6 & Infer8)
Node: [2,1]
F: [2,1]
G: [2,1]
nil: [2,1]
cons: [2,1]
id_annot: [0,0]
pair_annot: [0,0]
nested: [0,0]
tree: [0,0]
both: [0,0]
id_bot: [0,0]
mix_sum: [0,0]
mix_pair: [0,0]
outer_vs_inner: [0,0]
inner_vs_outer: [0,0]
list_ann: [0,0]
two_axes: [0,0]
deeply: [0,0]
list: [0,0]
list_inner: [0,0]
list_outer: [0,0]
list2: [0,0]
list2_inner: [0,0]
list2_outer: [0,0]
modal_plus: [0,0]
modal_pair: [0,0]
```

## modals.types

```
type foo() = [1,0] * [0,1]
type bar() = foo() @@ [1,0]
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
foo: {0 ↦ [1,1]}
bar: {0 ↦ [1,0]}

Timing: Infer2: 0.030 ms, Infer4: 0.028 ms, Infer5: 0.017 ms, Infer6: 0.030 ms, Infer8: 0.012 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
foo <=: [foo, bar]
bar <=: [foo, bar]
ROUND_UP (Infer6 & Infer8)
foo: [0,0]
bar: [0,0]
```

## mutual.types

```
type cons('a1) : [2,1]

type oddlist('a1) = unit + cons('a1, evenlist('a1))
type evenlist('a1) = unit + cons('a1, oddlist('a1))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
cons: {0 ↦ cons.0, 1 ↦ cons.1}
oddlist: {0 ↦ cons.0, 1 ↦ cons.1}
evenlist: {0 ↦ cons.0, 1 ↦ cons.1}

Timing: Infer2: 0.108 ms, Infer4: 0.133 ms, Infer5: 0.078 ms, Infer6: 0.061 ms, Infer8: 0.033 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
cons <=: [cons, oddlist, evenlist]
oddlist <=: [oddlist, evenlist]
evenlist <=: [oddlist, evenlist]
ROUND_UP (Infer6 & Infer8)
cons: [2,1]
oddlist: [0,0]
evenlist: [0,0]
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
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
portable: {0 ↦ portable.0, 1 ↦ portable.1}
list: {0 ↦ ⊥, 1 ↦ ⊤}
lily: {0 ↦ portable.0, 1 ↦ portable.1}
tulip: {0 ↦ portable.0, 1 ↦ ⊤}
orchid: {0 ↦ portable.0, 1 ↦ portable.1}

Timing: Infer2: 0.133 ms, Infer4: 0.107 ms, Infer5: 0.128 ms, Infer6: 0.080 ms, Infer8: 0.062 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
portable <=: [portable, list, lily, tulip, orchid]
list <=: [list, lily, tulip, orchid]
lily <=: [list, lily, tulip, orchid]
tulip <=: [list, lily, tulip, orchid]
orchid <=: [list, lily, tulip, orchid]
ROUND_UP (Infer6 & Infer8)
portable: [2,1]
list: [0,0]
lily: [0,0]
tulip: [0,0]
orchid: [0,0]
```

## ref.types

```
type portended('a1) : [2,1]
type ref('a1) : [2,1]
type foo('a1) = portended(ref('a1))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
portended: {0 ↦ portended.0, 1 ↦ portended.1}
ref: {0 ↦ ref.0, 1 ↦ ref.1}
foo: {0 ↦ portended.0 ⊔ (portended.1 ⊓ ref.0), 1 ↦ portended.1 ⊓ ref.1}

Timing: Infer2: 0.110 ms, Infer4: 0.087 ms, Infer5: 0.052 ms, Infer6: 0.052 ms, Infer8: 0.029 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
portended <=: [portended, foo]
ref <=: [ref, foo]
foo <=: [foo]
ROUND_UP (Infer6 & Infer8)
portended: [2,1]
ref: [2,1]
foo: [0,0]
```

## rose_sum_pair.types

```
type list('a1) = unit + 'a1 * list('a1)
type rose('a1) = list(rose(list('a1)))
type lily('a1) = list('a1 * lily(list('a1)))
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
list: {0 ↦ ⊥, 1 ↦ ⊤}
rose: {0 ↦ ⊥, 1 ↦ ⊥}
lily: {0 ↦ ⊥, 1 ↦ ⊤}

Timing: Infer2: 0.069 ms, Infer4: 0.056 ms, Infer5: 0.062 ms, Infer6: 0.040 ms, Infer8: 0.031 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
list <=: [list, rose, lily]
rose <=: [list, rose, lily]
lily <=: [list, rose, lily]
ROUND_UP (Infer6 & Infer8)
list: [0,0]
rose: [0,0]
lily: [0,0]
```

## simple_example.types

```
type foo() : [2,1]

type bar() = foo()
```

Program output:
```
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
foo: {0 ↦ foo.0}
bar: {0 ↦ foo.0}

Timing: Infer2: 0.043 ms, Infer4: 0.014 ms, Infer5: 0.015 ms, Infer6: 0.029 ms, Infer8: 0.010 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
foo <=: [foo, bar]
bar <=: [bar]
ROUND_UP (Infer6 & Infer8)
foo: [2,1]
bar: [0,0]
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
Infer2 & Infer4 & Infer5 & Infer6 & Infer8 normalized kinds:
cons: {0 ↦ cons.0, 1 ↦ cons.1, 2 ↦ cons.2}
down: {0 ↦ down.0, 1 ↦ down.1, 2 ↦ down.2}
list: {0 ↦ cons.0, 1 ↦ cons.1}
ctx: {0 ↦ down.0, 1 ↦ down.1}
zipper: {0 ↦ cons.0 ⊔ down.0, 1 ↦ cons.1 ⊔ down.1}

Timing: Infer2: 0.129 ms, Infer4: 0.112 ms, Infer5: 0.115 ms, Infer6: 0.069 ms, Infer8: 0.062 ms
```

Extras (Infer6/Infer8 leq and round_up):
```
LEQ (Infer6 & Infer8)
cons <=: [cons, list, ctx, zipper]
down <=: [down, list, ctx, zipper]
list <=: [list, ctx, zipper]
ctx <=: [list, ctx, zipper]
zipper <=: [list, ctx, zipper]
ROUND_UP (Infer6 & Infer8)
cons: [2,1]
down: [2,1]
list: [0,0]
ctx: [0,0]
zipper: [0,0]
```
