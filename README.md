# Combinatory Logic Term Rewriter & Backtracking Evaluator
**Non-deterministic AST Reduction via Custom Monad Transformers and Generic Zippers**

This repository contains an advanced functional term rewriting engine implemented in Haskell. It reduces complex combinatory logic expressions and executes pattern matching by utilizing a non-deterministic, backtracking search algorithm.

## Architectural Highlights
This project serves as a demonstration of high-level functional programming patterns, category theory applications, and immutable data structure design.

* **Heavy Monad Transformer Stack:** The core evaluation engine runs on a deeply nested monad stack: `ZipperT TreeMoves Expr (StateT ReductionState (LogicT IO))`. This cleanly separates side-effects: AST navigation (`ZipperT`), reduction history and depth tracking (`StateT`), and Prolog-like non-deterministic backtracking execution (`LogicT`).
* **Generic Huet's Zipper Implementation:** Developed a fully generic set of typeclasses (`Walkable`, `Zippable`, `Unzippable`) to implement Huet's Zipper. This allows for purely functional, O(1) in-place mutations and deep navigation of the Abstract Syntax Tree without the overhead of full tree reconstruction.
* **Non-deterministic Pattern Matching:** The reduction strategy (`reduceTo`, `checkPattern`) leverages the `Alternative` typeclass (`<|>`) to explore multiple reduction paths. When an irreversible reduction attempt fails, the engine seamlessly backtracks the entire virtual state to the last valid branch point.
* **Robust CLI:** Integrates `optparse-applicative` for comprehensive execution control, including maximum depth limiters, verbosity levels, and complete search path visualization.

## Theoretical Debt & Architectural Trade-offs
As an academic exploration of semantics and term rewriting, the implementation makes several strict theoretical trade-offs:

* **Combinatorial Explosion (LogicT Overhead):** The non-deterministic search operates purely on abstract syntax trees rather than Directed Acyclic Graphs (DAGs). Because the `LogicT` monad does not natively support sharing or memoization across backtracking branches, shared subexpressions are re-evaluated multiple times. The evaluation strategy is tree-based rather than graph-based, leading to exponential time complexity overhead on highly divergent reduction paths.
* **Mathematical Impurity (Partial Functions):** The AST translation layer relies on partial functions (`unexpectedValue` invoking hard exceptions) when mapping unsupported or malformed syntax. In strict type theory, this violates the principle of totality. A fully rigorous mathematical model would encapsulate all invalid states within an `ExceptT` or `Either` monad to formally prove behavior over the entire input domain.

## Tech Stack
* **Language:** Haskell (GHC)
* **Core Paradigms:** Monad Transformers, Logic Programming (Backtracking), Term Rewriting.
* **Libraries:** `logict`, `optparse-applicative`, `haskell-src`, `mtl`.
