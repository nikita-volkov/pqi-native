# pqi-native

[![Hackage](https://img.shields.io/hackage/v/pqi-native.svg)](https://hackage.haskell.org/package/pqi-native)
[![Continuous Haddock](https://img.shields.io/badge/haddock-master-blue)](https://nikita-volkov.github.io/pqi-native/)

A pure-Haskell [`pqi`](https://github.com/nikita-volkov/pqi) adapter
that speaks the PostgreSQL frontend/backend wire protocol directly — no
dependency on the C `libpq` library.

`pqi-native` is an LLM-generated port of the PostgreSQL C client library, [`libpq`](https://www.postgresql.org/docs/current/libpq.html). The upstream [`libpq` source](https://github.com/postgres/postgres/tree/master/src/interfaces/libpq) is the direct reference for the implementation.

## Fidelity goal

The goal is **byte-identical output to `libpq`** for every protocol-derived
value — error message strings, notice text, result status, field metadata,
cell data, and all structured error fields. Fidelity is continuously enforced
by [`pqi-conformance`](https://github.com/nikita-volkov/pqi-conformance), which
runs every operation on both this adapter and a direct
[`postgresql-libpq`](https://hackage.haskell.org/package/postgresql-libpq)
reference connection against the same database and asserts exact equality.

## Status

All classes are implemented. Verified against the `postgresql-libpq` reference
via the conformance differential suite.

Authentication: **trust**, **MD5**, and **SCRAM-SHA-256** are implemented. SCRAM
is verified against a password-auth PostgreSQL 17 container (which defaults to
`scram-sha-256`).
