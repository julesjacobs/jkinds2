#!/usr/bin/env python3
"""
Generate a suite of benchmark .types files under test/bench/.

Patterns are inspired by existing samples in test/types/ to cover:
- Wide arity constructors and parameter fanout
- Deep/mutual recursion (abstract and concrete)
- Modality annotations and nesting
- Sums/products mixtures
- Cyclic mu binders and nested mus
- Parameter permutations across multi-arity constructors

Run: python3 scripts/generate_benchmarks.py
"""

from __future__ import annotations
import os
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TYPES_DIR = ROOT / "test" / "bench"


def ensure_dir() -> None:
    TYPES_DIR.mkdir(parents=True, exist_ok=True)


def w(s: str) -> str:
    return s.rstrip() + "\n"


def write_file(name: str, body: str) -> None:
    (TYPES_DIR / name).write_text(body)


def gen_wide_arity(n: int = 10) -> str:
    params = ",".join(f"'a{i}" for i in range(1, n + 1))
    # Sum of annotated params, alternating axes
    terms = []
    for i in range(1, n + 1):
        ax1 = 1 if i % 2 == 0 else 0
        ax2 = 1 if i % 3 == 0 else 0
        terms.append(f"('a{i} @@ [{ax1},{ax2}])")
    rhs = " + ".join(terms)
    return w(f"# Wide arity with {n} params, alternating modalities") + w(
        f"type Wide({params}) : {rhs}"
    )


def gen_deep_chain(k: int = 15) -> str:
    lines = [w(f"# Deep abstract chain of length {k} with alternating axes")]
    for i in range(1, k):
        ax1, ax2 = (1 if i % 2 == 0 else 0, 0 if i % 2 == 0 else 1)
        lines.append(w(f"type A{i}('a1) : A{i+1}('a1) @@ [{ax1},{ax2}] + 'a1"))
    # Close the cycle
    lines.append(w(f"type A{k}('a1) : A1('a1) @@ [0,1]"))
    return "".join(lines)


def gen_mutual_group(m: int = 6) -> str:
    lines = [w(f"# {m}-way mutual recursion mixing abstract/concrete")]
    for i in range(1, m + 1):
        j = 1 + (i % m)
        # Alternate concrete (=) and abstract (:) forms
        if i % 2 == 0:
            # concrete referencing the next
            lines.append(w(f"type M{i}('a1) = M{j}('a1) + ('a1 @@ [1,0])"))
        else:
            # abstract referencing the next with other axis
            lines.append(w(f"type M{i}('a1) : M{j}('a1) @@ [0,1] + 'a1"))
    return "".join(lines)


def gen_modality_nesting(levels: int = 6) -> str:
    lines = [w(f"# Nested modality annotations depth {levels}")]
    # Build nested annotation on a small expression
    expr = "'a1"
    for i in range(1, levels + 1):
        ax1, ax2 = (i % 2, (i + 1) % 2)
        expr = f"({expr} @@ [{ax1},{ax2}])"
    lines.append(w(f"type ModNest('a1) : {expr} + ('a1 @@ [1,1])"))
    return "".join(lines)


def gen_sum_pair_mix(depth: int = 8) -> str:
    lines = [w(f"# Sum/product alternating nest depth {depth}")]
    # Build a rose-tree/list-like pair/sum nest using a single parameter
    e = "'a1"
    for i in range(depth):
        if i % 2 == 0:
            e = f"({e} * {e})"
        else:
            e = f"(unit + {e})"
    lines.append(w(f"type Mix('a1) = {e}"))
    return "".join(lines)


def gen_cyclic_mu(depth: int = 2) -> str:
    lines = [w("# Cyclic mu binders with shallow and nested mus")]
    lines.append(w("type L('a1) : mu 'b1. ('a1 + 'b1)"))
    # Nested mus
    lines.append(w("type NL('a1) : mu 'b1. ((mu 'b2. ('b1 + 'b2)) + 'a1)"))
    # Add a chain of mus parameterized by depth
    # mu 'b1. mu 'b2. ... ('a1 + 'bd)
    inner = "'a1"
    for d in range(depth, 0, -1):
        inner = f"('b{d} + {inner})"
    expr = inner
    for d in range(1, depth + 1):
        expr = f"(mu 'b{d}. {expr})"
    lines.append(w(f"type LM('a1) : {expr}"))
    return "".join(lines)


def gen_param_permutations() -> str:
    lines = [w("# Three-arity parameter permutations with cross axes")]
    lines.append(w("type P3('a1,'a2,'a3) : ('a2 @@ [1,0]) + ('a1 @@ [0,1]) + ('a3 @@ [1,1])"))
    lines.append(w("type P3R('a1,'a2,'a3) = P3('a2,'a3,'a1)"))
    return "".join(lines)


def gen_abstract_concrete_mix() -> str:
    lines = [w("# Concrete-abstract mixed recursion with parameter swaps")]
    lines.append(w("type CA('a1) : CC('a1) @@ [0,1] + 'a1"))
    lines.append(w("type CC('a1) = CA('a1) + ('a1 @@ [1,0])"))
    lines.append(w("type CA2('a1,'a2) : CC2('a2,'a1) @@ [1,0] + ('a1 @@ [0,1])"))
    lines.append(w("type CC2('a1,'a2) = CA2('a1,'a2) + ('a2 @@ [1,0])"))
    return "".join(lines)


def gen_dense_cross_types(n_types: int = 200, arity: int = 2) -> str:
    """Generate many small-arity types that all cross-reference each other
    with sums, products, modality annotations, and nesting.

    The graph is dense (each type references several others) but each RHS is
    intentionally small to keep overall size manageable.
    """
    assert arity in (2, 3)
    lines = [w(f"# Dense cross-referencing set: {n_types} types, arity {arity}")]
    params = ",".join(f"'a{i}" for i in range(1, arity + 1))

    def pname(i: int) -> str:
        return f"DX{i}"

    def args(order: tuple[int, ...] | None = None) -> str:
        if order is None:
            order = tuple(range(1, arity + 1))
        return ",".join(f"'a{i}" for i in order)

    for i in range(n_types):
        j1 = (i + 1) % n_types
        j2 = (i + 7) % n_types
        j3 = (i * 5 + 3) % n_types
        # Alternate abstract and concrete to vary constraints
        op = ":" if (i % 3 != 0) else "="

        # Build 3–4 small terms mixing patterns
        t1 = f"{pname(j1)}({args(tuple(range(arity, 0, -1)))}) @@ [1,0]"
        if arity == 2:
            t2 = f"({pname(j2)}({args()}) * 'a1)"
            t3 = f"({pname(j3)}({pname(j1)}({args()}),'a2))"
            t4 = "'a2 @@ [0,1]"
        else:  # arity == 3
            t2 = f"({pname(j2)}({args()}) * 'a1)"
            t3 = f"({pname(j3)}({pname(j1)}({args()}),'a2,'a3))"
            t4 = "('a2 @@ [0,1]) + ('a3 @@ [1,0])"

        # Occasionally wrap with an extra modality
        if i % 4 == 0:
            t2 = f"({t2} @@ [0,1])"

        rhs_terms = [t1, t2, t3, t4]
        rhs = " + ".join(rhs_terms)
        lines.append(w(f"type {pname(i)}({params}) {op} {rhs}"))

    return "".join(lines)

def gen_dense_cross_types_simple(n_types: int = 200, arity: int = 2) -> str:
    """Generate many small-arity types that all cross-reference each other
    with sums, products, modality annotations, and nesting.

    The graph is dense (each type references several others) but each RHS is
    intentionally small to keep overall size manageable.
    """
    assert arity in (2, 3)
    lines = [w(f"# Dense simple cross-referencing set: {n_types} types, arity {arity}")]
    params = ",".join(f"'a{i}" for i in range(1, arity + 1))

    def pname(i: int) -> str:
        return f"DX{i}"

    def args(order: tuple[int, ...] | None = None) -> str:
        if order is None:
            order = tuple(range(1, arity + 1))
        return ",".join(f"'a{i}" for i in order)

    for i in range(n_types):
        j1 = (i + 1) % n_types
        j2 = (i + 7) % n_types
        j3 = (i * 5 + 3) % n_types
        # Alternate abstract and concrete to vary constraints
        op = ":" if (i % 3 != 0) else "="

        # Build 3–4 small terms mixing patterns
        t1 = f"{pname(j1)}({args(tuple(range(arity, 0, -1)))}) @@ [1,0]"
        if arity == 2:
            t2 = f"({pname(j2)}({args()}) * 'a1)"
            t3 = f"{pname(j3)}({args()}) + {pname(j1)}({args()})"
            t4 = "'a2 @@ [0,1]"
        else:  # arity == 3
            t2 = f"({pname(j2)}({args()}) * 'a1)"
            t3 = f"{pname(j3)}({args()}) + {pname(j1)}({args()})"
            t4 = "('a2 @@ [0,1]) + ('a3 @@ [1,0])"

        # Occasionally wrap with an extra modality
        if i % 4 == 0:
            t2 = f"({t2} @@ [0,1])"

        rhs_terms = [t1, t2, t3, t4]
        rhs = " + ".join(rhs_terms)
        lines.append(w(f"type {pname(i)}({params}) {op} {rhs}"))

    return "".join(lines)

def gen_dense_cross_types_concrete(n_types: int = 200, arity: int = 2) -> str:
    """Concrete-only variant of dense cross-referencing types.

    Mirrors gen_dense_cross_types but uses only concrete declarations (=).
    """
    assert arity in (2, 3)
    lines = [w(f"# Dense cross-referencing (concrete only): {n_types} types, arity {arity}")]
    params = ",".join(f"'a{i}" for i in range(1, arity + 1))

    def pname(i: int) -> str:
        return f"DXC{i}"

    def args(order: tuple[int, ...] | None = None) -> str:
        if order is None:
            order = tuple(range(1, arity + 1))
        return ",".join(f"'a{i}" for i in order)

    for i in range(n_types):
        j1 = (i + 1) % n_types
        j2 = (i + 7) % n_types
        j3 = (i * 5 + 3) % n_types

        # Build 3–4 small terms mixing patterns (same as dense variant)
        t1 = f"{pname(j1)}({args(tuple(range(arity, 0, -1)))}) @@ [1,0]"
        if arity == 2:
            t2 = f"({pname(j2)}({args()}) * 'a1)"
            t3 = f"({pname(j3)}({pname(j1)}({args()}),'a2))"
            t4 = "'a2 @@ [0,1]"
        else:  # arity == 3
            t2 = f"({pname(j2)}({args()}) * 'a1)"
            t3 = f"({pname(j3)}({pname(j1)}({args()}),'a2,'a3))"
            t4 = "('a2 @@ [0,1]) + ('a3 @@ [1,0])"

        # Occasionally wrap with an extra modality
        if i % 4 == 0:
            t2 = f"({t2} @@ [0,1])"

        rhs_terms = [t1, t2, t3, t4]
        rhs = " + ".join(rhs_terms)
        lines.append(w(f"type {pname(i)}({params}) = {rhs}"))

    return "".join(lines)

def gen_dense_cross_types_concrete_dag(n_types: int = 200, arity: int = 2) -> str:
    """Concrete-only dense cross-ref, but DAG: each type i only references < i.

    This ensures an acyclic dependency graph while keeping RHS expressions
    similar in size/shape to the dense variants.
    """
    assert arity in (2, 3)
    lines = [
        w(
            f"# Dense cross-referencing (concrete only, DAG): {n_types} types, arity {arity}"
        )
    ]
    params = ",".join(f"'a{i}" for i in range(1, arity + 1))

    def pname(i: int) -> str:
        return f"DXD{i}"

    def args(order: tuple[int, ...] | None = None) -> str:
        if order is None:
            order = tuple(range(1, arity + 1))
        return ",".join(f"'a{i}" for i in order)

    for i in range(n_types):
        if i == 0:
            # Base case can't reference previous types; just use params/modality
            if arity == 2:
                rhs = "('a1 @@ [1,0]) + ('a2 @@ [0,1])"
            else:  # arity == 3
                rhs = "('a1 @@ [1,0]) + ('a2 @@ [0,1]) + ('a3 @@ [1,1])"
            lines.append(w(f"type {pname(i)}({params}) = {rhs}"))
            continue

        j1 = i - 1
        j2 = max(0, i - 3)
        j3 = max(0, i // 2)

        t1 = f"{pname(j1)}({args(tuple(range(arity, 0, -1)))}) @@ [1,0]"
        if arity == 2:
            t2 = f"({pname(j2)}({args()}) * 'a1)"
            t3 = f"({pname(j3)}({pname(j1)}({args()}),'a2))"
            t4 = "'a2 @@ [0,1]"
        else:  # arity == 3
            t2 = f"({pname(j2)}({args()}) * 'a1)"
            t3 = f"({pname(j3)}({pname(j1)}({args()}),'a2,'a3))"
            t4 = "('a2 @@ [0,1]) + ('a3 @@ [1,0])"

        if i % 4 == 0:
            t2 = f"({t2} @@ [0,1])"

        rhs_terms = [t1, t2, t3, t4]
        rhs = " + ".join(rhs_terms)
        lines.append(w(f"type {pname(i)}({params}) = {rhs}"))

    return "".join(lines)

def main() -> None:
    ensure_dir()

    files = {
        # Increased sizes for heavier stress
        "bench_wide_arity.types": gen_wide_arity(80),
        "bench_deep_chain.types": gen_deep_chain(60),
        "bench_mutual_group.types": gen_mutual_group(24),
        "bench_modality_nesting.types": gen_modality_nesting(24),
        "bench_sum_pair_mix.types": gen_sum_pair_mix(40),
        "bench_cyclic_mu.types": gen_cyclic_mu(8),
        "bench_param_permutations.types": gen_param_permutations(),
        "bench_abstract_concrete_mix.types": gen_abstract_concrete_mix(),
        "bench_dense_cross.types": gen_dense_cross_types(35, 2),
        "bench_dense_cross_simple.types": gen_dense_cross_types_simple(35, 2),
        "bench_dense_cross_concrete.types": gen_dense_cross_types_concrete(35, 2),
        "bench_dense_cross_concrete_100_dag.types": gen_dense_cross_types_concrete_dag(100, 2),
        "bench_dense_cross_concrete_100.types": gen_dense_cross_types_concrete(100, 2),
    }

    for name, body in files.items():
        write_file(name, body)

    print(f"Wrote {len(files)} benchmark files to {TYPES_DIR}")


if __name__ == "__main__":
    main()
