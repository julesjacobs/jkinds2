# jkinds Types Report

Generated: 2025-08-15 18:43:34 UTC

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
Kinds:
foo: {0 ↦ ⊥}
bar: {0 ↦ [0,1]}
baz: {0 ↦ ⊤}
one: {0 ↦ ([0,1] ⊓ two.0)}
two: {0 ↦ one.0}
maybe: {0 ↦ (none.0 ⊔ some.0), 1 ↦ some.1}
wrap: {0 ↦ ⊥, 1 ↦ [1,0]}
wrap2: {0 ↦ ⊤, 1 ↦ ⊥}
pairish: {0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [1,0]}
treeA: {0 ↦ (leaf.0 ⊔ node.0 ⊔ (node.2 ⊓ treeA.0)), 1 ↦ (node.1 ⊔ (node.2 ⊓ treeA.1))}
H: {0 ↦ F.0, 1 ↦ ⊤}
F: {0 ↦ ([0,1] ⊓ H.0), 1 ↦ ([0,1] ⊓ H.1)}
G: {0 ↦ ⊤, 1 ↦ ⊥}

Least fixpoint kinds:
[lfp] iter 0:
  two: {0 ↦ ⊥}
  G: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 1:
  two: {0 ↦ one.0}
  G: {0 ↦ ⊤, 1 ↦ ⊥}
[lfp] iter 0:
  foo: {0 ↦ foo.0}
  bar: {0 ↦ bar.0}
  baz: {0 ↦ baz.0}
  one: {0 ↦ one.0}
  maybe: {0 ↦ maybe.0, 1 ↦ maybe.1}
  wrap: {0 ↦ wrap.0, 1 ↦ wrap.1}
  wrap2: {0 ↦ wrap2.0, 1 ↦ wrap2.1}
  pairish: {0 ↦ pairish.0, 1 ↦ pairish.1, 2 ↦ pairish.2}
  treeA: {0 ↦ treeA.0, 1 ↦ treeA.1}
  H: {0 ↦ H.0, 1 ↦ H.1}
  F: {0 ↦ F.0, 1 ↦ F.1}
[lfp] iter 1:
  foo: {0 ↦ ⊥}
  bar: {0 ↦ ([0,1] ⊓ bar.0)}
  baz: {0 ↦ baz.0}
  one: {0 ↦ ([0,1] ⊓ one.0)}
  maybe: {0 ↦ ((maybe.0 ⊓ none.0) ⊔ (maybe.0 ⊓ some.0)), 1 ↦ ((maybe.1 ⊓ none.0) ⊔ (maybe.1 ⊓ some.0) ⊔ (maybe.1 ⊓ some.1))}
  wrap: {0 ↦ ⊥, 1 ↦ ([1,0] ⊓ wrap.1)}
  wrap2: {0 ↦ wrap2.0, 1 ↦ wrap2.1}
  pairish: {0 ↦ ⊥, 1 ↦ ([1,0] ⊓ pairish.1), 2 ↦ ([1,0] ⊓ pairish.2)}
  treeA: {0 ↦ ((leaf.0 ⊓ treeA.0) ⊔ (node.0 ⊓ treeA.0) ⊔ (node.2 ⊓ treeA.0)), 1 ↦ ((leaf.0 ⊓ treeA.1) ⊔ (node.0 ⊓ treeA.1) ⊔ (node.1 ⊓ treeA.1) ⊔ (node.2 ⊓ treeA.1))}
  H: {0 ↦ (F.0 ⊓ H.0), 1 ↦ H.1}
  F: {0 ↦ ([0,1] ⊓ F.0 ⊓ H.0), 1 ↦ (([0,1] ⊓ F.1 ⊓ H.0) ⊔ ([0,1] ⊓ F.1 ⊓ H.1))}
[lfp] iter 2:
  foo: {0 ↦ ⊥}
  bar: {0 ↦ ([0,1] ⊓ bar.0)}
  baz: {0 ↦ baz.0}
  one: {0 ↦ ([0,1] ⊓ one.0)}
  maybe: {0 ↦ ((maybe.0 ⊓ none.0) ⊔ (maybe.0 ⊓ some.0)), 1 ↦ ((maybe.1 ⊓ none.0) ⊔ (maybe.1 ⊓ some.0) ⊔ (maybe.1 ⊓ some.1))}
  wrap: {0 ↦ ⊥, 1 ↦ ([1,0] ⊓ wrap.1)}
  wrap2: {0 ↦ wrap2.0, 1 ↦ wrap2.1}
  pairish: {0 ↦ ⊥, 1 ↦ ([1,0] ⊓ pairish.1), 2 ↦ ([1,0] ⊓ pairish.2)}
  treeA: {0 ↦ ((leaf.0 ⊓ treeA.0) ⊔ (node.0 ⊓ treeA.0) ⊔ (node.2 ⊓ treeA.0)), 1 ↦ ((leaf.0 ⊓ treeA.1) ⊔ (node.0 ⊓ treeA.1) ⊔ (node.1 ⊓ treeA.1) ⊔ (node.2 ⊓ treeA.1))}
  H: {0 ↦ ([0,1] ⊓ F.0 ⊓ H.0), 1 ↦ H.1}
  F: {0 ↦ ([0,1] ⊓ F.0 ⊓ H.0), 1 ↦ ([0,1] ⊓ F.1 ⊓ H.1)}

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
Kinds:
foo1: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ (t1.1 ⊓ t2.1)}
foo2: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ (t1.1 ⊓ t2.1)}
bar: {0 ↦ (bar.0 ⊔ (bar.1 ⊓ t1.0) ⊔ (bar.1 ⊓ t2.0)), 1 ↦ ⊤}

Least fixpoint kinds:
[lfp] iter 0:
  foo1: {0 ↦ ⊥, 1 ↦ ⊥}
  foo2: {0 ↦ ⊥, 1 ↦ ⊥}
  bar: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 1:
  foo1: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ (t1.1 ⊓ t2.1)}
  foo2: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ (t1.1 ⊓ t2.1)}
  bar: {0 ↦ ⊥, 1 ↦ ⊤}
[lfp] iter 2:
  foo1: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ (t1.1 ⊓ t2.1)}
  foo2: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ (t1.1 ⊓ t2.1)}
  bar: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ ⊤}
[lfp] iter 0:


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
Kinds:
btree: {0 ↦ (leaf.0 ⊔ node.0 ⊔ (btree.0 ⊓ node.1) ⊔ (btree.0 ⊓ node.2)), 1 ↦ (leaf.1 ⊔ (btree.1 ⊓ node.1) ⊔ (btree.1 ⊓ node.2))}

Least fixpoint kinds:
[lfp] iter 0:
  btree: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 1:
  btree: {0 ↦ (leaf.0 ⊔ node.0), 1 ↦ leaf.1}
[lfp] iter 0:


Normalized kinds:
btree: {0 ↦ (leaf.0 ⊔ node.0), 1 ↦ leaf.1}

Ceil/Floor kinds:
btree: ceil={0 ↦ ⊤, 1 ↦ ⊥}, floor={0 ↦ ⊥, 1 ↦ ⊥}

LEQ relationships:
btree <= (none)
```

## list_sum_pair.types

```
type list('a1) = unit + 'a1 * list('a1)
```

Program output:
```
Kinds:
list: {0 ↦ list.0, 1 ↦ ⊤}

Least fixpoint kinds:
[lfp] iter 0:
  list: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 1:
  list: {0 ↦ ⊥, 1 ↦ ⊤}
[lfp] iter 0:


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
Kinds:
id_annot: {0 ↦ ⊥, 1 ↦ ⊤}
pair_annot: {0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [1,0]}
nested: {0 ↦ ⊥, 1 ↦ [1,1]}
tree: {0 ↦ (([0,1] ⊓ Node.0) ⊔ ([0,1] ⊓ Node.2 ⊓ tree.0)), 1 ↦ (([0,1] ⊓ Node.1) ⊔ ([0,1] ⊓ Node.2 ⊓ tree.1))}
both: {0 ↦ ⊥, 1 ↦ ⊤}
id_bot: {0 ↦ ⊥, 1 ↦ ⊥}
mix_sum: {0 ↦ ⊥, 1 ↦ [1,1]}
mix_pair: {0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [0,1]}
outer_vs_inner: {0 ↦ ⊥, 1 ↦ [1,0]}
inner_vs_outer: {0 ↦ ⊥, 1 ↦ [1,0]}
list_ann: {0 ↦ ([1,0] ⊓ list_ann.0), 1 ↦ [1,0]}
two_axes: {0 ↦ ([0,1] ⊓ G.0), 1 ↦ ([0,1] ⊓ G.1)}
deeply: {0 ↦ (([1,0] ⊓ F.0) ⊔ ([0,1] ⊓ G.0)), 1 ↦ (([1,0] ⊓ F.1) ⊔ ([0,1] ⊓ G.1))}
list: {0 ↦ list.0, 1 ↦ ⊤}
list_inner: {0 ↦ list.0, 1 ↦ ([1,0] ⊓ list.1)}
list_outer: {0 ↦ ([1,0] ⊓ list.0), 1 ↦ ([1,0] ⊓ list.1)}
list2: {0 ↦ (cons.0 ⊔ nil.0 ⊔ (cons.2 ⊓ list2.0)), 1 ↦ (cons.1 ⊔ (cons.2 ⊓ list2.1))}
list2_inner: {0 ↦ list2.0, 1 ↦ ([1,0] ⊓ list2.1)}
list2_outer: {0 ↦ ([1,0] ⊓ list2.0), 1 ↦ ([1,0] ⊓ list2.1)}
modal_plus: {0 ↦ [1,0], 1 ↦ ⊤}
modal_pair: {0 ↦ [1,0], 1 ↦ ⊤}

Least fixpoint kinds:
[lfp] iter 0:
  id_annot: {0 ↦ ⊥, 1 ↦ ⊥}
  pair_annot: {0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
  nested: {0 ↦ ⊥, 1 ↦ ⊥}
  tree: {0 ↦ ⊥, 1 ↦ ⊥}
  both: {0 ↦ ⊥, 1 ↦ ⊥}
  id_bot: {0 ↦ ⊥, 1 ↦ ⊥}
  mix_sum: {0 ↦ ⊥, 1 ↦ ⊥}
  mix_pair: {0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
  outer_vs_inner: {0 ↦ ⊥, 1 ↦ ⊥}
  inner_vs_outer: {0 ↦ ⊥, 1 ↦ ⊥}
  list_ann: {0 ↦ ⊥, 1 ↦ ⊥}
  two_axes: {0 ↦ ⊥, 1 ↦ ⊥}
  deeply: {0 ↦ ⊥, 1 ↦ ⊥}
  list: {0 ↦ ⊥, 1 ↦ ⊥}
  list_inner: {0 ↦ ⊥, 1 ↦ ⊥}
  list_outer: {0 ↦ ⊥, 1 ↦ ⊥}
  list2: {0 ↦ ⊥, 1 ↦ ⊥}
  list2_inner: {0 ↦ ⊥, 1 ↦ ⊥}
  list2_outer: {0 ↦ ⊥, 1 ↦ ⊥}
  modal_plus: {0 ↦ ⊥, 1 ↦ ⊥}
  modal_pair: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 1:
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
  list_inner: {0 ↦ ⊥, 1 ↦ ⊥}
  list_outer: {0 ↦ ⊥, 1 ↦ ⊥}
  list2: {0 ↦ (cons.0 ⊔ nil.0), 1 ↦ cons.1}
  list2_inner: {0 ↦ ⊥, 1 ↦ ⊥}
  list2_outer: {0 ↦ ⊥, 1 ↦ ⊥}
  modal_plus: {0 ↦ [1,0], 1 ↦ ⊤}
  modal_pair: {0 ↦ [1,0], 1 ↦ ⊤}
[lfp] iter 2:
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
[lfp] iter 0:


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
Kinds:
foo: {0 ↦ [1,1]}
bar: {0 ↦ ([1,0] ⊓ foo.0)}

Least fixpoint kinds:
[lfp] iter 0:
  foo: {0 ↦ ⊥}
  bar: {0 ↦ ⊥}
[lfp] iter 1:
  foo: {0 ↦ [1,1]}
  bar: {0 ↦ ⊥}
[lfp] iter 2:
  foo: {0 ↦ [1,1]}
  bar: {0 ↦ [1,0]}
[lfp] iter 0:


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
Kinds:
oddlist: {0 ↦ (cons.0 ⊔ (cons.2 ⊓ evenlist.0)), 1 ↦ (cons.1 ⊔ (cons.2 ⊓ evenlist.1))}
evenlist: {0 ↦ (cons.0 ⊔ (cons.2 ⊓ oddlist.0)), 1 ↦ (cons.1 ⊔ (cons.2 ⊓ oddlist.1))}

Least fixpoint kinds:
[lfp] iter 0:
  oddlist: {0 ↦ ⊥, 1 ↦ ⊥}
  evenlist: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 1:
  oddlist: {0 ↦ cons.0, 1 ↦ cons.1}
  evenlist: {0 ↦ cons.0, 1 ↦ cons.1}
[lfp] iter 0:


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
Kinds:
list: {0 ↦ list.0, 1 ↦ ⊤}
lily: {0 ↦ (list.0 ⊔ (lily.0 ⊓ list.1) ⊔ (list.1 ⊓ portable.0)), 1 ↦ ((lily.1 ⊓ list.1) ⊔ (list.1 ⊓ portable.1))}
tulip: {0 ↦ (tulip.0 ⊔ (portable.0 ⊓ tulip.1)), 1 ↦ ⊤}
orchid: {0 ↦ (orchid.0 ⊔ portable.0), 1 ↦ (orchid.1 ⊔ portable.1)}

Least fixpoint kinds:
[lfp] iter 0:
  list: {0 ↦ ⊥, 1 ↦ ⊥}
  lily: {0 ↦ ⊥, 1 ↦ ⊥}
  tulip: {0 ↦ ⊥, 1 ↦ ⊥}
  orchid: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 1:
  list: {0 ↦ ⊥, 1 ↦ ⊤}
  lily: {0 ↦ ⊥, 1 ↦ ⊥}
  tulip: {0 ↦ ⊥, 1 ↦ ⊤}
  orchid: {0 ↦ portable.0, 1 ↦ portable.1}
[lfp] iter 2:
  list: {0 ↦ ⊥, 1 ↦ ⊤}
  lily: {0 ↦ portable.0, 1 ↦ portable.1}
  tulip: {0 ↦ portable.0, 1 ↦ ⊤}
  orchid: {0 ↦ portable.0, 1 ↦ portable.1}
[lfp] iter 0:


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
Kinds:
foo: {0 ↦ (portended.0 ⊔ (portended.1 ⊓ ref.0)), 1 ↦ (portended.1 ⊓ ref.1)}

Least fixpoint kinds:
[lfp] iter 0:
  foo: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 1:
  foo: {0 ↦ (portended.0 ⊔ (portended.1 ⊓ ref.0)), 1 ↦ (portended.1 ⊓ ref.1)}
[lfp] iter 0:


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
Kinds:
list: {0 ↦ list.0, 1 ↦ ⊤}
rose: {0 ↦ (list.0 ⊔ (list.1 ⊓ rose.0)), 1 ↦ (list.1 ⊓ rose.1)}
lily: {0 ↦ (list.0 ⊔ (lily.0 ⊓ list.1)), 1 ↦ list.1}

Least fixpoint kinds:
[lfp] iter 0:
  list: {0 ↦ ⊥, 1 ↦ ⊥}
  rose: {0 ↦ ⊥, 1 ↦ ⊥}
  lily: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 1:
  list: {0 ↦ ⊥, 1 ↦ ⊤}
  rose: {0 ↦ ⊥, 1 ↦ ⊥}
  lily: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 2:
  list: {0 ↦ ⊥, 1 ↦ ⊤}
  rose: {0 ↦ ⊥, 1 ↦ ⊥}
  lily: {0 ↦ ⊥, 1 ↦ ⊤}
[lfp] iter 0:


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
Kinds:
list: {0 ↦ (cons.0 ⊔ (cons.2 ⊓ list.0)), 1 ↦ (cons.1 ⊔ (cons.2 ⊓ list.1))}
ctx: {0 ↦ (down.0 ⊔ (ctx.0 ⊓ down.2)), 1 ↦ (down.1 ⊔ (ctx.1 ⊓ down.2))}
zipper: {0 ↦ (ctx.0 ⊔ list.0), 1 ↦ (ctx.1 ⊔ list.1)}

Least fixpoint kinds:
[lfp] iter 0:
  list: {0 ↦ ⊥, 1 ↦ ⊥}
  ctx: {0 ↦ ⊥, 1 ↦ ⊥}
  zipper: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 1:
  list: {0 ↦ cons.0, 1 ↦ cons.1}
  ctx: {0 ↦ down.0, 1 ↦ down.1}
  zipper: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 2:
  list: {0 ↦ cons.0, 1 ↦ cons.1}
  ctx: {0 ↦ down.0, 1 ↦ down.1}
  zipper: {0 ↦ (cons.0 ⊔ down.0), 1 ↦ (cons.1 ⊔ down.1)}
[lfp] iter 0:


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
