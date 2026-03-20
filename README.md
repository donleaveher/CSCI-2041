# CSCI 2041 — Advanced Programming Principles

University of Minnesota · Spring 2025 · OCaml

This repository contains labs, projects, and learning materials for CSCI 2041, covering functional programming concepts in OCaml — from basic recursion to lazy evaluation, memoization, and symbolic computation.

---

## Labs

### Lab 1 — List Recursion Basics
Fundamental list operations implemented recursively:
- `howMany` — count occurrences of an element
- `delete` — remove all occurrences from a list
- `sum` / `length` / `mean` — basic float list statistics

### Lab 2 — Rational Number Arithmetic
Rational number representation and arithmetic using GCD:
- `gcd` — Euclidean algorithm
- `rat`, `ratAdd`, `ratMul`, `ratDiv`, `ratGt` — fraction operations
- `euler` — approximation of Euler's number *e* via series expansion (Σ 1/n!)

### Lab 3 — Binary Search Trees
Full BST implementation with a custom algebraic data type:
- `bstSearch`, `bstInsert`, `bstDelete` (handles leaf, one-child, two-children cases)
- `in_order`, `pre_order`, `post_order` traversals

### Lab 4 — Permutations & Higher-Order Functions
Combinatorial generation using callbacks and continuations:
- `choose` — iterate over elements with a callback
- `allbut` — remove first occurrence
- `permute` — generate all permutations via backtracking

### Lab 5 — Infinite Streams
Lazy evaluation with infinite stream data structures:
- `makeStream`, `first`, `rest`, `take`, `trim`
- `odds` — infinite stream of odd numbers
- `scale` / `sum` — stream transformations

### Lab 6 — Memoization
Performance optimization through hash-table memoization:
- `c` — naive recursive binomial coefficient C(n, k) (exponential time)
- `memyC` — memoized version (~1000× speedup on C(40, 10))

### Lab 7 — Lazy Lists
OCaml's `Lazy.t` for deferred evaluation:
- `lazyList` algebraic type with `lazyCons`, `lazyHead`, `lazyTail`, `lazyTake`
- Examples: `lazyInts` (range with side effects), `lazyFibs` (infinite Fibonacci)

---

## Project

### Project 1 — Symbolic Equation Solver
An algebraic equation solver using recursive pattern matching:
- `expression` type supporting `Var`, `Neg`, `Add`, `Sub`, `Mul`, `Div`, `Equ`
- `solver` — applies transformation rules to isolate a variable (e.g., `x + a = b` → `x = b - a`)
- `isInside` — checks variable presence in an expression
- Error handling with custom `SolvingError` exception

---

## Standalone Files

| File | Description |
|------|-------------|
| `tautology.ml` | Propositional logic tautology checker — evaluates all 2ⁿ truth assignments to verify logical formulas |
| `final.ml` | Efficient iterative binomial coefficient computation |
| `practice.ml` | Continuation-Passing Style (CPS) exercises — `filter_cps`, `sum_tree_cps`, `append_cps`, `map`, etc. |

---

## Learning Notes

In-class coding examples organized by date:

| File | Topic |
|------|-------|
| `learning/Feb23.ml` | Streams & lazy evaluation fundamentals |
| `learning/Feb25.ml` | Mutable references & stack abstraction via closures |
| `learning/Mar02.ml` | Array-based memoization (Fibonacci) |
| `learning/Mar04.ml` | Memoization patterns — association lists vs. arrays |
| `learning/Mar06.ml` | Thunks, futures, `Lazy.force` |

---

## Lecture Slides

The `lecture/` directory contains course lecture PDFs.

---

## Topics Covered

| Topic | Where |
|-------|-------|
| Recursion & list processing | Labs 1–2 |
| Algebraic data types & pattern matching | Labs 3, Project 1 |
| Higher-order functions & continuations | Lab 4, `practice.ml` |
| Lazy evaluation & infinite structures | Labs 5, 7 |
| Memoization & dynamic programming | Lab 6 |
| Propositional logic | `tautology.ml` |
| Symbolic computation | Project 1 |
| CPS (Continuation-Passing Style) | `practice.ml` |
| Imperative style in OCaml | `learning/Feb25.ml` |
