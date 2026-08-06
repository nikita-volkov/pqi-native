# pqi-native

[![Hackage](https://img.shields.io/hackage/v/pqi-native.svg)](https://hackage.haskell.org/package/pqi-native)
[![Continuous Haddock](https://img.shields.io/badge/haddock-master-blue)](https://nikita-volkov.github.io/pqi-native/)

> **Status: Alpha.** `pqi-native` is an early implementation of a pure-Haskell
> transport for [`pqi`](https://github.com/nikita-volkov/pqi). It exists
> alongside [`pqi-ffi`](https://github.com/nikita-volkov/pqi-ffi), the
> established C-backed adapter, and the two are fully interchangeable: any
> code written against `pqi` runs unchanged on either one, so trying
> `pqi-native` carries no lock-in and no rewrite cost. Correctness is checked
> continuously against a conformance suite (see below), but the adapter
> hasn't yet accumulated production mileage. If you need a production-proven
> transport today, use `pqi-ffi`. See
> [_Making libpq a choice_](https://nikita-volkov.github.io/pqi-making-libpq-a-choice/)
> for why this project exists and what tradeoffs that implies.

A pure-Haskell [`pqi`](https://github.com/nikita-volkov/pqi) adapter
that speaks the PostgreSQL frontend/backend wire protocol directly — no
dependency on the C `libpq` library.

`pqi-native` reimplements the wire protocol handled by the PostgreSQL C
client library, [`libpq`](https://www.postgresql.org/docs/current/libpq.html),
from scratch in Haskell. The upstream [`libpq` source](https://github.com/postgres/postgres/tree/master/src/interfaces/libpq) is the direct reference for the implementation.

## Fidelity goal

The goal is **identical output to `libpq`** for every protocol-derived
value — error message strings, notice text, result status, field metadata,
cell data, and all structured error fields. Fidelity is continuously enforced
by [`pqi-conformance`](https://github.com/nikita-volkov/pqi-conformance), which
runs every operation on both this adapter and a direct
[`postgresql-libpq`](https://hackage.haskell.org/package/postgresql-libpq)
reference connection against the same database and asserts exact equality.

## Status

**Alpha.** The full `Pqi.Connection` capability record is implemented and
verified against the `postgresql-libpq` reference via the conformance
differential suite, but the library hasn't yet accumulated real-world
production mileage.

Because it implements the same `pqi` interface as
[`pqi-ffi`](https://github.com/nikita-volkov/pqi-ffi), switching between the
two is a one-line change — pass a different `Adapter` value, nothing else in
your code moves. That makes `pqi-native` low-risk to evaluate now and easy to
fall back from: adopt it where you want to shed the `libpq` dependency, and
drop back to `pqi-ffi` at any time without touching the rest of your
codebase. Read
[_Making libpq a choice_](https://nikita-volkov.github.io/pqi-making-libpq-a-choice/)
for the motivation and the tradeoffs of adopting it at this stage.

Authentication: **trust**, **MD5**, and **SCRAM-SHA-256** are implemented. SCRAM
is verified against a password-auth PostgreSQL 17 container (which defaults to
`scram-sha-256`).
