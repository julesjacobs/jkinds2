# jkinds Types Report

Generated: 2025-08-12 02:13:45 UTC

## btree.types

```
type btree('a1) = (leaf('a1) + node(btree('a1), btree('a1)))
```

Program output:
```
Kinds:
btree: {btree.0 -> {{btree.0 ⊓ node.1} ⊔ {btree.0 ⊓ node.2} ⊔ {leaf.0} ⊔ {node.0}}, btree.1 -> {{btree.1 ⊓ node.1} ⊔ {btree.1 ⊓ node.2} ⊔ {leaf.1}}}

Least fixpoint kinds:
[lfp] iter 0:
  btree: {btree.0 -> ⊥, btree.1 -> ⊥}
[lfp] iter 1:
  btree: {btree.0 -> {{leaf.0} ⊔ {node.0}}, btree.1 -> {{leaf.1}}}
btree: {'a0 -> {{leaf.0} ⊔ {node.0}}, 'a1 -> {{leaf.1}}}
```

## list_sum_pair.types

```
type list('a1) = unit + 'a1 * list('a1)
```

Program output:
```
Kinds:
list: {list.0 -> {{list.0}}, list.1 -> ⊤}

Least fixpoint kinds:
[lfp] iter 0:
  list: {list.0 -> ⊥, list.1 -> ⊥}
[lfp] iter 1:
  list: {list.0 -> ⊥, list.1 -> ⊤}
list: {'a0 -> ⊥, 'a1 -> ⊤}
```

## mutual.types

```
type oddlist('a1) = unit + cons('a1, evenlist('a1))
type evenlist('a1) = unit + cons('a1, oddlist('a1))
```

Program output:
```
Kinds:
evenlist: {evenlist.0 -> {{cons.0} ⊔ {cons.2 ⊓ oddlist.0}}, evenlist.1 -> {{cons.1} ⊔ {cons.2 ⊓ oddlist.1}}}
oddlist: {oddlist.0 -> {{cons.0} ⊔ {cons.2 ⊓ evenlist.0}}, oddlist.1 -> {{cons.1} ⊔ {cons.2 ⊓ evenlist.1}}}

Least fixpoint kinds:
[lfp] iter 0:
  evenlist: {evenlist.0 -> ⊥, evenlist.1 -> ⊥}
  oddlist: {oddlist.0 -> ⊥, oddlist.1 -> ⊥}
[lfp] iter 1:
  evenlist: {evenlist.0 -> {{cons.0}}, evenlist.1 -> {{cons.1}}}
  oddlist: {oddlist.0 -> {{cons.0}}, oddlist.1 -> {{cons.1}}}
evenlist: {'a0 -> {{cons.0}}, 'a1 -> {{cons.1}}}
oddlist: {'a0 -> {{cons.0}}, 'a1 -> {{cons.1}}}
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
lily: {lily.0 -> {{lily.0 ⊓ list.1} ⊔ {list.0}}, lily.1 -> {{list.1}}}
list: {list.0 -> {{list.0}}, list.1 -> ⊤}
rose: {rose.0 -> {{list.0} ⊔ {list.1 ⊓ rose.0}}, rose.1 -> {{list.1 ⊓ rose.1}}}

Least fixpoint kinds:
[lfp] iter 0:
  lily: {lily.0 -> ⊥, lily.1 -> ⊥}
  list: {list.0 -> ⊥, list.1 -> ⊥}
  rose: {rose.0 -> ⊥, rose.1 -> ⊥}
[lfp] iter 1:
  lily: {lily.0 -> ⊥, lily.1 -> ⊥}
  list: {list.0 -> ⊥, list.1 -> ⊤}
  rose: {rose.0 -> ⊥, rose.1 -> ⊥}
[lfp] iter 2:
  lily: {lily.0 -> ⊥, lily.1 -> ⊤}
  list: {list.0 -> ⊥, list.1 -> ⊤}
  rose: {rose.0 -> ⊥, rose.1 -> ⊥}
lily: {'a0 -> ⊥, 'a1 -> ⊤}
list: {'a0 -> ⊥, 'a1 -> ⊤}
rose: {'a0 -> ⊥, 'a1 -> ⊥}
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
ctx: {ctx.0 -> {{ctx.0 ⊓ down.2} ⊔ {down.0}}, ctx.1 -> {{ctx.1 ⊓ down.2} ⊔ {down.1}}}
list: {list.0 -> {{cons.0} ⊔ {cons.2 ⊓ list.0}}, list.1 -> {{cons.1} ⊔ {cons.2 ⊓ list.1}}}
zipper: {zipper.0 -> {{ctx.0} ⊔ {list.0}}, zipper.1 -> {{ctx.1} ⊔ {list.1}}}

Least fixpoint kinds:
[lfp] iter 0:
  ctx: {ctx.0 -> ⊥, ctx.1 -> ⊥}
  list: {list.0 -> ⊥, list.1 -> ⊥}
  zipper: {zipper.0 -> ⊥, zipper.1 -> ⊥}
[lfp] iter 1:
  ctx: {ctx.0 -> {{down.0}}, ctx.1 -> {{down.1}}}
  list: {list.0 -> {{cons.0}}, list.1 -> {{cons.1}}}
  zipper: {zipper.0 -> ⊥, zipper.1 -> ⊥}
[lfp] iter 2:
  ctx: {ctx.0 -> {{down.0}}, ctx.1 -> {{down.1}}}
  list: {list.0 -> {{cons.0}}, list.1 -> {{cons.1}}}
  zipper: {zipper.0 -> {{cons.0} ⊔ {down.0}}, zipper.1 -> {{cons.1} ⊔ {down.1}}}
ctx: {'a0 -> {{down.0}}, 'a1 -> {{down.1}}}
list: {'a0 -> {{cons.0}}, 'a1 -> {{cons.1}}}
zipper: {'a0 -> {{cons.0} ⊔ {down.0}}, 'a1 -> {{cons.1} ⊔ {down.1}}}
```
