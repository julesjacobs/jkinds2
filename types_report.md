# jkinds Types Report

Generated: 2025-08-14 17:42:33 UTC

## benjamin.types

```
type foo1('a1) = t2(t1('a1)) * t1(unit)
type foo2('a1) = t1(t2('a1)) * t2(unit)

type bar('a1) = bar(t1('a1)) + bar(t2('a1)) + 'a1
```

Program output:
```
Kinds:
bar: {0 ↦ (bar.0 ⊔ (bar.1 ⊓ t1.0) ⊔ (bar.1 ⊓ t2.0)), 1 ↦ ⊤}
foo1: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ {(t1.1 ⊓ t2.1)}}
foo2: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ {(t1.1 ⊓ t2.1)}}

Least fixpoint kinds:
[lfp] iter 0:
  bar: {0 ↦ ⊥, 1 ↦ ⊥}
  foo1: {0 ↦ ⊥, 1 ↦ ⊥}
  foo2: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 1:
  bar: {0 ↦ ⊥, 1 ↦ ⊤}
  foo1: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ {(t1.1 ⊓ t2.1)}}
  foo2: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ {(t1.1 ⊓ t2.1)}}
[lfp] iter 2:
  bar: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ ⊤}
  foo1: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ {(t1.1 ⊓ t2.1)}}
  foo2: {0 ↦ (t1.0 ⊔ t2.0), 1 ↦ {(t1.1 ⊓ t2.1)}}
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
both: {0 ↦ ⊥, 1 ↦ ⊤}
deeply: {0 ↦ (([1,0] ⊓ F.0) ⊔ ([0,1] ⊓ G.0)), 1 ↦ (([1,0] ⊓ F.1) ⊔ ([0,1] ⊓ G.1))}
id_annot: {0 ↦ ⊥, 1 ↦ ⊤}
id_bot: {0 ↦ ⊥, 1 ↦ ⊥}
inner_vs_outer: {0 ↦ ⊥, 1 ↦ [1,0]}
list: {0 ↦ list.0, 1 ↦ ⊤}
list2: {0 ↦ (cons.0 ⊔ nil.0 ⊔ (cons.2 ⊓ list2.0)), 1 ↦ (cons.1 ⊔ (cons.2 ⊓ list2.1))}
list2_inner: {0 ↦ list2.0, 1 ↦ {([1,0] ⊓ list2.1)}}
list2_outer: {0 ↦ {([1,0] ⊓ list2.0)}, 1 ↦ {([1,0] ⊓ list2.1)}}
list_ann: {0 ↦ {([1,0] ⊓ list_ann.0)}, 1 ↦ [1,0]}
list_inner: {0 ↦ list.0, 1 ↦ {([1,0] ⊓ list.1)}}
list_outer: {0 ↦ {([1,0] ⊓ list.0)}, 1 ↦ {([1,0] ⊓ list.1)}}
mix_pair: {0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [0,1]}
mix_sum: {0 ↦ ⊥, 1 ↦ [1,1]}
modal_pair: {0 ↦ [1,0], 1 ↦ ⊤}
modal_plus: {0 ↦ [1,0], 1 ↦ ⊤}
nested: {0 ↦ ⊥, 1 ↦ [1,1]}
outer_vs_inner: {0 ↦ ⊥, 1 ↦ [1,0]}
pair_annot: {0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [1,0]}
tree: {0 ↦ (([0,1] ⊓ Node.0) ⊔ ([0,1] ⊓ Node.2 ⊓ tree.0)), 1 ↦ (([0,1] ⊓ Node.1) ⊔ ([0,1] ⊓ Node.2 ⊓ tree.1))}
two_axes: {0 ↦ {([0,1] ⊓ G.0)}, 1 ↦ {([0,1] ⊓ G.1)}}

Least fixpoint kinds:
[lfp] iter 0:
  both: {0 ↦ ⊥, 1 ↦ ⊥}
  deeply: {0 ↦ ⊥, 1 ↦ ⊥}
  id_annot: {0 ↦ ⊥, 1 ↦ ⊥}
  id_bot: {0 ↦ ⊥, 1 ↦ ⊥}
  inner_vs_outer: {0 ↦ ⊥, 1 ↦ ⊥}
  list: {0 ↦ ⊥, 1 ↦ ⊥}
  list2: {0 ↦ ⊥, 1 ↦ ⊥}
  list2_inner: {0 ↦ ⊥, 1 ↦ ⊥}
  list2_outer: {0 ↦ ⊥, 1 ↦ ⊥}
  list_ann: {0 ↦ ⊥, 1 ↦ ⊥}
  list_inner: {0 ↦ ⊥, 1 ↦ ⊥}
  list_outer: {0 ↦ ⊥, 1 ↦ ⊥}
  mix_pair: {0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
  mix_sum: {0 ↦ ⊥, 1 ↦ ⊥}
  modal_pair: {0 ↦ ⊥, 1 ↦ ⊥}
  modal_plus: {0 ↦ ⊥, 1 ↦ ⊥}
  nested: {0 ↦ ⊥, 1 ↦ ⊥}
  outer_vs_inner: {0 ↦ ⊥, 1 ↦ ⊥}
  pair_annot: {0 ↦ ⊥, 1 ↦ ⊥, 2 ↦ ⊥}
  tree: {0 ↦ ⊥, 1 ↦ ⊥}
  two_axes: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 1:
  both: {0 ↦ ⊥, 1 ↦ ⊤}
  deeply: {0 ↦ (([1,0] ⊓ F.0) ⊔ ([0,1] ⊓ G.0)), 1 ↦ (([1,0] ⊓ F.1) ⊔ ([0,1] ⊓ G.1))}
  id_annot: {0 ↦ ⊥, 1 ↦ ⊤}
  id_bot: {0 ↦ ⊥, 1 ↦ ⊥}
  inner_vs_outer: {0 ↦ ⊥, 1 ↦ [1,0]}
  list: {0 ↦ ⊥, 1 ↦ ⊤}
  list2: {0 ↦ (cons.0 ⊔ nil.0), 1 ↦ cons.1}
  list2_inner: {0 ↦ ⊥, 1 ↦ ⊥}
  list2_outer: {0 ↦ ⊥, 1 ↦ ⊥}
  list_ann: {0 ↦ ⊥, 1 ↦ [1,0]}
  list_inner: {0 ↦ ⊥, 1 ↦ ⊥}
  list_outer: {0 ↦ ⊥, 1 ↦ ⊥}
  mix_pair: {0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [0,1]}
  mix_sum: {0 ↦ ⊥, 1 ↦ [1,1]}
  modal_pair: {0 ↦ [1,0], 1 ↦ ⊤}
  modal_plus: {0 ↦ [1,0], 1 ↦ ⊤}
  nested: {0 ↦ ⊥, 1 ↦ [1,1]}
  outer_vs_inner: {0 ↦ ⊥, 1 ↦ [1,0]}
  pair_annot: {0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [1,0]}
  tree: {0 ↦ {([0,1] ⊓ Node.0)}, 1 ↦ {([0,1] ⊓ Node.1)}}
  two_axes: {0 ↦ {([0,1] ⊓ G.0)}, 1 ↦ {([0,1] ⊓ G.1)}}
[lfp] iter 2:
  both: {0 ↦ ⊥, 1 ↦ ⊤}
  deeply: {0 ↦ (([1,0] ⊓ F.0) ⊔ ([0,1] ⊓ G.0)), 1 ↦ (([1,0] ⊓ F.1) ⊔ ([0,1] ⊓ G.1))}
  id_annot: {0 ↦ ⊥, 1 ↦ ⊤}
  id_bot: {0 ↦ ⊥, 1 ↦ ⊥}
  inner_vs_outer: {0 ↦ ⊥, 1 ↦ [1,0]}
  list: {0 ↦ ⊥, 1 ↦ ⊤}
  list2: {0 ↦ (cons.0 ⊔ nil.0), 1 ↦ cons.1}
  list2_inner: {0 ↦ (cons.0 ⊔ nil.0), 1 ↦ {([1,0] ⊓ cons.1)}}
  list2_outer: {0 ↦ (([1,0] ⊓ cons.0) ⊔ ([1,0] ⊓ nil.0)), 1 ↦ {([1,0] ⊓ cons.1)}}
  list_ann: {0 ↦ ⊥, 1 ↦ [1,0]}
  list_inner: {0 ↦ ⊥, 1 ↦ [1,0]}
  list_outer: {0 ↦ ⊥, 1 ↦ [1,0]}
  mix_pair: {0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [0,1]}
  mix_sum: {0 ↦ ⊥, 1 ↦ [1,1]}
  modal_pair: {0 ↦ [1,0], 1 ↦ ⊤}
  modal_plus: {0 ↦ [1,0], 1 ↦ ⊤}
  nested: {0 ↦ ⊥, 1 ↦ [1,1]}
  outer_vs_inner: {0 ↦ ⊥, 1 ↦ [1,0]}
  pair_annot: {0 ↦ ⊥, 1 ↦ [1,0], 2 ↦ [1,0]}
  tree: {0 ↦ {([0,1] ⊓ Node.0)}, 1 ↦ {([0,1] ⊓ Node.1)}}
  two_axes: {0 ↦ {([0,1] ⊓ G.0)}, 1 ↦ {([0,1] ⊓ G.1)}}
```

## modals.types

```
type foo() = [1,0] * [0,1]
type bar() = foo() @@ [1,0]
```

Program output:
```
Kinds:
bar: {0 ↦ {([1,0] ⊓ foo.0)}}
foo: {0 ↦ [1,1]}

Least fixpoint kinds:
[lfp] iter 0:
  bar: {0 ↦ ⊥}
  foo: {0 ↦ ⊥}
[lfp] iter 1:
  bar: {0 ↦ ⊥}
  foo: {0 ↦ [1,1]}
[lfp] iter 2:
  bar: {0 ↦ [1,0]}
  foo: {0 ↦ [1,1]}
```

## mutual.types

```
type oddlist('a1) = unit + cons('a1, evenlist('a1))
type evenlist('a1) = unit + cons('a1, oddlist('a1))
```

Program output:
```
Kinds:
evenlist: {0 ↦ (cons.0 ⊔ (cons.2 ⊓ oddlist.0)), 1 ↦ (cons.1 ⊔ (cons.2 ⊓ oddlist.1))}
oddlist: {0 ↦ (cons.0 ⊔ (cons.2 ⊓ evenlist.0)), 1 ↦ (cons.1 ⊔ (cons.2 ⊓ evenlist.1))}

Least fixpoint kinds:
[lfp] iter 0:
  evenlist: {0 ↦ ⊥, 1 ↦ ⊥}
  oddlist: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 1:
  evenlist: {0 ↦ cons.0, 1 ↦ cons.1}
  oddlist: {0 ↦ cons.0, 1 ↦ cons.1}
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
lily: {0 ↦ (list.0 ⊔ (lily.0 ⊓ list.1) ⊔ (list.1 ⊓ portable.0)), 1 ↦ ((lily.1 ⊓ list.1) ⊔ (list.1 ⊓ portable.1))}
list: {0 ↦ list.0, 1 ↦ ⊤}
orchid: {0 ↦ (orchid.0 ⊔ portable.0), 1 ↦ (orchid.1 ⊔ portable.1)}
tulip: {0 ↦ (tulip.0 ⊔ (portable.0 ⊓ tulip.1)), 1 ↦ ⊤}

Least fixpoint kinds:
[lfp] iter 0:
  lily: {0 ↦ ⊥, 1 ↦ ⊥}
  list: {0 ↦ ⊥, 1 ↦ ⊥}
  orchid: {0 ↦ ⊥, 1 ↦ ⊥}
  tulip: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 1:
  lily: {0 ↦ ⊥, 1 ↦ ⊥}
  list: {0 ↦ ⊥, 1 ↦ ⊤}
  orchid: {0 ↦ portable.0, 1 ↦ portable.1}
  tulip: {0 ↦ ⊥, 1 ↦ ⊤}
[lfp] iter 2:
  lily: {0 ↦ portable.0, 1 ↦ portable.1}
  list: {0 ↦ ⊥, 1 ↦ ⊤}
  orchid: {0 ↦ portable.0, 1 ↦ portable.1}
  tulip: {0 ↦ portable.0, 1 ↦ ⊤}
```

## ref.types

```
type foo('a1) = portended(ref('a1))
```

Program output:
```
Kinds:
foo: {0 ↦ (portended.0 ⊔ (portended.1 ⊓ ref.0)), 1 ↦ {(portended.1 ⊓ ref.1)}}

Least fixpoint kinds:
[lfp] iter 0:
  foo: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 1:
  foo: {0 ↦ (portended.0 ⊔ (portended.1 ⊓ ref.0)), 1 ↦ {(portended.1 ⊓ ref.1)}}
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
lily: {0 ↦ (list.0 ⊔ (lily.0 ⊓ list.1)), 1 ↦ list.1}
list: {0 ↦ list.0, 1 ↦ ⊤}
rose: {0 ↦ (list.0 ⊔ (list.1 ⊓ rose.0)), 1 ↦ {(list.1 ⊓ rose.1)}}

Least fixpoint kinds:
[lfp] iter 0:
  lily: {0 ↦ ⊥, 1 ↦ ⊥}
  list: {0 ↦ ⊥, 1 ↦ ⊥}
  rose: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 1:
  lily: {0 ↦ ⊥, 1 ↦ ⊥}
  list: {0 ↦ ⊥, 1 ↦ ⊤}
  rose: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 2:
  lily: {0 ↦ ⊥, 1 ↦ ⊤}
  list: {0 ↦ ⊥, 1 ↦ ⊤}
  rose: {0 ↦ ⊥, 1 ↦ ⊥}
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
ctx: {0 ↦ (down.0 ⊔ (ctx.0 ⊓ down.2)), 1 ↦ (down.1 ⊔ (ctx.1 ⊓ down.2))}
list: {0 ↦ (cons.0 ⊔ (cons.2 ⊓ list.0)), 1 ↦ (cons.1 ⊔ (cons.2 ⊓ list.1))}
zipper: {0 ↦ (ctx.0 ⊔ list.0), 1 ↦ (ctx.1 ⊔ list.1)}

Least fixpoint kinds:
[lfp] iter 0:
  ctx: {0 ↦ ⊥, 1 ↦ ⊥}
  list: {0 ↦ ⊥, 1 ↦ ⊥}
  zipper: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 1:
  ctx: {0 ↦ down.0, 1 ↦ down.1}
  list: {0 ↦ cons.0, 1 ↦ cons.1}
  zipper: {0 ↦ ⊥, 1 ↦ ⊥}
[lfp] iter 2:
  ctx: {0 ↦ down.0, 1 ↦ down.1}
  list: {0 ↦ cons.0, 1 ↦ cons.1}
  zipper: {0 ↦ (cons.0 ⊔ down.0), 1 ↦ (cons.1 ⊔ down.1)}
```
