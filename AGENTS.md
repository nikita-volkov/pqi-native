# AGENTS.md

## Fidelity is the contract

`pqi-native` reimplements the PostgreSQL wire protocol handled by the C
`libpq` library, in pure Haskell. Its contract is **identical observable output
to `libpq`** for every protocol-derived value: error message strings, notice
text, result status, field metadata, cell data, and every structured error
field. The
[upstream `libpq` source](https://github.com/postgres/postgres/tree/master/src/interfaces/libpq)
is the direct reference for any implementation question.

The contract is enforced by
[`pqi-conformance`](https://github.com/nikita-volkov/pqi-conformance), a
differential suite that runs each operation on this adapter and on a direct
`postgresql-libpq` connection against the same database and asserts the
observations are equal.

Why the suite carries so much weight: this codebase is largely LLM-generated,
so its trustworthiness comes from the proof system that checks its output
rather than from the process that produced it. See
[_Making libpq a choice_](https://nikita-volkov.github.io/pqi-making-libpq-a-choice/)
and [_Hasql v2: the native era_](https://nikita-volkov.github.io/hasql-v2-the-native-era/).

## Policy: every bug is reproduced in pqi-conformance first

**A behavioural divergence from `libpq` is fixed here only after its
reproduction has been committed to `pqi-conformance`.**

This holds however obvious the bug looks and wherever it surfaced: a `hasql`
issue, a user report, a failure of the conformance suite, or your own reading
of the code. A fix shipped without a spec is a bug free to come back
unnoticed, so a patch to `src/library` with no accompanying conformance spec is
unfinished work.

## The loop

`cabal.project.local` already points at the sibling `../pqi-conformance`
checkout, so the two repos build as one unit and there is no publish
round-trip in the middle.

1. **Write and commit the spec** in `../pqi-conformance`. That repo's
   `AGENTS.md` covers where it goes and how to choose an observation that
   compares meaningfully.
2. **Confirm red.** `cabal test` fails on the new spec against the unfixed
   adapter. A spec that passes here before the fix reproduces nothing, so go
   back and sharpen it.
3. **Fix it** under `src/library/Pqi/Native/`, taking the upstream `libpq`
   source as the reference for the intended behaviour.
4. **Confirm green.** The full battery passes.
5. **Pin the proof.** Raise the `pqi-conformance` lower bound in
   `pqi-native.cabal` to the version carrying the new spec, so a released
   `pqi-native` is tied to the suite that proves it.
6. **Release it.** Bump the fourth version component, and add a `## Fixes`
   entry to `CHANGELOG.md` describing the divergence, naming the spec module
   that now catches it (for example
   `Pqi.Conformance.Operation.Connectdb.UnixSocketUri`), and citing the
   upstream issue if there was one.
