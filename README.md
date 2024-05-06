# `oregano`
Staged regular expression library for Scala 3. It has the following overarching
goals:

1. Provide a static guarantee of a regex's syntactic correctness.
2. Provide a type-safe matching API à la https://dl.acm.org/doi/abs/10.1145/3550198.3550425
   (but without match types and duplication).
3. Provide an ahead-of-time compilation of the regex into a state machine.
4. Implement optimisations into that machine:
    - Linear state fusion
    - Sub-expression optimisation using Thompson's construction
    - Left-factoring
    - Optimising likely hot-paths with true DFA construction?
    - Common-sub expression elimination with some "calling convention"
    - Probably more...

These states can be completed independently, and the library can be published
with partially complete goals: each goal is already useful on its own.

## Notes
Given that there will be finite states in the end machine, it will be good to
track in-flight states (in the backtrackless execution model) with a
`mutable.Bitset`, which will eliminate allocation and allow a compact memory
representation.

Each state in the machine should be represented at runtime: the compilation
process is what is being statically eliminated, and removing the states at
runtime prevents the optimised Thompson's construction from working
(`Expr[List[State]]` does not have the right binding properties if `State` is
supposedly purely static). In a linear path, however, states can be fused
together and eliminated into super-states -- the handle for this state is the
dynamic part and the internal structure can be optimised.

We will probably need to handle divergence at some point too: naïvely, this can
be traced in a similar way to the `detectDivergence` combinator in
`parsley-debug`, but I'm guessing there is probably a cheaper way.
