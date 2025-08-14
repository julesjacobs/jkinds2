# jkinds Types Report

Generated: 2025-08-14 02:06:07 UTC

## benjamin.types

```
type foo1('a1) = t2(t1('a1)) * t1(unit)
type foo2('a1) = t1(t2('a1)) * t2(unit)```

Program output:
```
Kinds:
foo1: {foo1.0 -> {{t1.0} ⊔ {t2.0}}, foo1.1 -> {{t1.1 ⊓ t2.1}}}
foo2: {foo2.0 -> {{t1.0} ⊔ {t2.0}}, foo2.1 -> {{t1.1 ⊓ t2.1}}}

Least fixpoint kinds:
[lfp] iter 0:
  foo1: {foo1.0 -> ⊥, foo1.1 -> ⊥}
  foo2: {foo2.0 -> ⊥, foo2.1 -> ⊥}
[lfp] iter 1:
  foo1: {foo1.0 -> {{t1.0} ⊔ {t2.0}}, foo1.1 -> {{t1.1 ⊓ t2.1}}}
  foo2: {foo2.0 -> {{t1.0} ⊔ {t2.0}}, foo2.1 -> {{t1.1 ⊓ t2.1}}}
```

## btree.types

```
type btree('a1) = (leaf('a1) + node(btree('a1), btree('a1)))
```

Program output:
```
Kinds:
btree: {btree.0 -> {{leaf.0} ⊔ {node.0} ⊔ {btree.0 ⊓ node.1} ⊔ {btree.0 ⊓ node.2}}, btree.1 -> {{leaf.1} ⊔ {btree.1 ⊓ node.1} ⊔ {btree.1 ⊓ node.2}}}

Least fixpoint kinds:
[lfp] iter 0:
  btree: {btree.0 -> ⊥, btree.1 -> ⊥}
[lfp] iter 1:
  btree: {btree.0 -> {{leaf.0} ⊔ {node.0}}, btree.1 -> {{leaf.1}}}
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
lily: {lily.0 -> {{list.0} ⊔ {lily.0 ⊓ list.1} ⊔ {list.1 ⊓ portable.0}}, lily.1 -> {{lily.1 ⊓ list.1} ⊔ {list.1 ⊓ portable.1}}}
list: {list.0 -> {{list.0}}, list.1 -> ⊤}
orchid: {orchid.0 -> {{orchid.0} ⊔ {portable.0}}, orchid.1 -> {{orchid.1} ⊔ {portable.1}}}
tulip: {tulip.0 -> {{tulip.0} ⊔ {portable.0 ⊓ tulip.1}}, tulip.1 -> ⊤}

Least fixpoint kinds:
[lfp] iter 0:
  lily: {lily.0 -> ⊥, lily.1 -> ⊥}
  list: {list.0 -> ⊥, list.1 -> ⊥}
  orchid: {orchid.0 -> ⊥, orchid.1 -> ⊥}
  tulip: {tulip.0 -> ⊥, tulip.1 -> ⊥}
[lfp] iter 1:
  lily: {lily.0 -> ⊥, lily.1 -> ⊥}
  list: {list.0 -> ⊥, list.1 -> ⊤}
  orchid: {orchid.0 -> {{portable.0}}, orchid.1 -> {{portable.1}}}
  tulip: {tulip.0 -> ⊥, tulip.1 -> ⊤}
[lfp] iter 2:
  lily: {lily.0 -> {{portable.0}}, lily.1 -> {{portable.1}}}
  list: {list.0 -> ⊥, list.1 -> ⊤}
  orchid: {orchid.0 -> {{portable.0}}, orchid.1 -> {{portable.1}}}
  tulip: {tulip.0 -> {{portable.0}}, tulip.1 -> ⊤}
```

## ref.types

```
type foo('a1) = portended(ref('a1))```

Program output:
```
Kinds:
foo: {foo.0 -> {{portended.0} ⊔ {portended.1 ⊓ ref.0}}, foo.1 -> {{portended.1 ⊓ ref.1}}}

Least fixpoint kinds:
[lfp] iter 0:
  foo: {foo.0 -> ⊥, foo.1 -> ⊥}
[lfp] iter 1:
  foo: {foo.0 -> {{portended.0} ⊔ {portended.1 ⊓ ref.0}}, foo.1 -> {{portended.1 ⊓ ref.1}}}
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
lily: {lily.0 -> {{list.0} ⊔ {lily.0 ⊓ list.1}}, lily.1 -> {{list.1}}}
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
ctx: {ctx.0 -> {{down.0} ⊔ {ctx.0 ⊓ down.2}}, ctx.1 -> {{down.1} ⊔ {ctx.1 ⊓ down.2}}}
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
```
