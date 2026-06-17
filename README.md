# pqi-native

[![Hackage](https://img.shields.io/hackage/v/pqi-native.svg)](https://hackage.haskell.org/package/pqi-native)
[![Continuous Haddock](https://img.shields.io/badge/haddock-master-blue)](https://nikita-volkov.github.io/pqi-native/)

A pure-Haskell [`pqi`](https://github.com/nikita-volkov/pqi) adapter
that speaks the PostgreSQL frontend/backend wire protocol directly — no
dependency on the C `libpq` library.

`pqi-native` is a port of the PostgreSQL C client library, `libpq`
(source: <https://github.com/postgres/postgres>). The upstream `libpq` source
is the direct reference for the implementation.

The byte-level transport (socket I/O, message framing, and the
serialization/deserialization of wire messages via
[`ptr-poker`](https://hackage.haskell.org/package/ptr-poker) and
[`ptr-peeker`](https://hackage.haskell.org/package/ptr-peeker)) is isolated in
an internal `transport` sub-library, exposing a `Comms` class for serializable
message types. The public library is an abstraction over it that implements the
`pqi` capability classes.

## Fidelity goal

The goal is **byte-identical output to `libpq`** for every protocol-derived
value — error message strings, notice text, result status, field metadata,
cell data, and all structured error fields. Fidelity is continuously enforced
by [`pqi-conformance`](https://github.com/nikita-volkov/pqi-conformance), which
runs every operation on both this adapter and a direct
[`postgresql-libpq`](https://hackage.haskell.org/package/postgresql-libpq)
reference connection against the same database and asserts exact equality.

## Status

All ten capability classes are implemented. Verified against the `postgresql-libpq` reference
via the conformance differential suite: `Connectivity`, `Querying` (simple +
extended query, full result accessors), `Escaping`, `AsyncCommands`
(`sendQuery`/`getResult` observed identical to `exec`), and `LargeObjects` (a
byte-identical round-trip; implemented over the server's `lo_*` SQL functions).

Also implemented (functional, but not differentially tested): `Cancellation`
(out-of-band cancel request), `Notifications` (`LISTEN`/`NOTIFY` and notice
collection), `Copying`, and `Control`. `Pipelining` is minimal: the sync/flush
requests are sent, but commands are not yet truly batched.

Authentication: **trust**, **MD5**, and **SCRAM-SHA-256** are implemented. SCRAM
is verified against a password-auth PostgreSQL 17 container (which defaults to
`scram-sha-256`).

Conninfo parsing covers the `key=value` form (`host`, `port`, `user`, `dbname`,
`password`).
