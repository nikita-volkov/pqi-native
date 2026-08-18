# v1.0.1.8

## Fixes

- Fixed `connectFailureMessage` (used when the initial `connect(2)` fails, e.g. a missing Unix-socket directory) hand-rolling a TCP-shaped message - the raw `Show`n `IOException` prefixed with `"could not connect to server: "` and a `(Unix domain socket '<path>')` parenthetical appended - instead of reporting the failure the way libpq does. That hand-rolled text retained the literal prefix `"could not connect to server: "`, which is also the substring `Hasql.Connection`'s error classifier matches to identify a transient networking failure - so a permanent misconfiguration (missing socket directory) was misclassified as transient. The Unix-socket branch now delegates to `unixSocketFailureMessage`, extracting the underlying `IOException`'s `ioe_description` (the raw OS `strerror` text, e.g. `"No such file or directory"`) instead of embedding its whole `Show`n form, and appending the same `"Is the server running locally and accepting connections on that socket?"` hint libpq appends for this failure - matching libpq's message for this case exactly. Also fixed `unixSocketFailureMessage` itself quoting the socket path with single quotes (`'<path>'`) instead of libpq's double quotes (`"<path>"`), which affects every message it formats, including the handshake-rejection path this Unix-socket branch now shares. Caught by the `pqi-conformance` spec `Pqi.Conformance.Operation.Connectdb.MissingUnixSocketDirectory`. Found via `hasql` issue #329.

# v1.0.1.7

## Fixes

- Fixed a `postgresql://` URI conninfo with a percent-encoded Unix-socket host (e.g. `%2ftmp%2f...`) being passed through to `host` still percent-encoded instead of decoded. `parseUri`'s `host`/`port` splitter decoded every other URI component (`user`, `password`, `dbname`, query params) but forwarded the raw, undecoded host bytes. Caught by the `pqi-conformance` differential spec `Pqi.Conformance.Operation.Connectdb.UnixSocketUri`.

# v1.0.1.6

## Fixes

- Fixed a mid-handshake server rejection (e.g. "sorry, too many clients already") escaping as an uncaught `IOException` instead of coming back as a classified `ConnectionBad` (#8). `establish` only wrapped the initial TCP connect in an exception handler; the handshake read that follows it is now caught too and routed through the same `tcpFailureMessage`/`unixSocketFailureMessage` wrapper `failWith` uses, with libpq's own wording for the EOF case. `tcpFailureMessage` also stopped adding a redundant "(ip)" parenthetical when the resolved peer IP is identical to the given host. Caught by the `pqi-conformance` spec added in `nikita-volkov/pqi-conformance@92f5205`.

- Fixed a pipelined command's result getting misattributed to a later, unrelated command after a prior pipeline aborted on a server error (#9). A pipelined command that sends a `Parse` (`sendQueryParams`/`sendPrepare`) records a FIFO entry so the eventual `ParseComplete` can be charged to the right command; that entry was only ever popped by a `ParseComplete` actually arriving. A command whose `Parse` itself fails - a syntax error, or being silently discarded by the server after a pipeline abort - never gets a `ParseComplete`, so its entry was leaked. A `sendPrepare` leaks a `True` entry, and once a later, unrelated command's genuine `ParseComplete` popped that stale `True` instead of its own, that command terminated immediately as `CommandOk` instead of collecting its real result, observed as a `SELECT` returning `CommandOk` instead of `TuplesOk`. Every pipelined command now pops its FIFO entry exactly once, whether via its own `ParseComplete` or, failing that, at its own terminal message (including the synthetic result generated for a command discarded after an abort). Caught by the differential coverage added in `pqi-conformance` 1.0.5.1.

# v1.0.1.5

## Fixes

- `sendQueryParams`, `execParams`, `sendPrepare`, `prepare`, `sendQueryPrepared`, and `execPrepared` now reject a parameter list (or, for `prepare`\/`sendPrepare`, a parameter-type list) longer than 65535 locally, without writing anything to the socket, mirroring `libpq`'s `PQ_QUERY_PARAM_MAX_LIMIT` check. The `Parse`\/`Bind` messages encode their parameter count as a 16-bit field; past the limit `fromIntegral` silently wrapped it (65536 became 0), producing a malformed message the server rejected with `invalid message format` and leaving the connection desynchronized - inside a pipeline, every command dispatched before the failing one was left with its results undrained. Caught by the differential coverage added in `pqi-conformance` 1.0.5.0. Found via `hasql` issue #326.

# v1.0.1.4

## Fixes

- Fixed missing support for connecting over a Unix-domain socket (#6). A `host` value that looks like an absolute path (e.g. `host=/var/run/postgresql`) names a socket directory rather than a TCP host, and the connection is made to a `.s.PGSQL.<port>` unix-domain socket in that directory, mirroring libpq's rule for the `host` conninfo parameter. A conninfo with no `host` (or an empty one) now defaults the way libpq itself does: the `PGHOST` environment variable if set and non-empty, otherwise a Unix-domain socket in `/tmp` on Unix-like systems (see `Pqi.Native.Connection.defaultUnixSocketDir`'s Haddock for how a distribution that compiles its own `libpq` with a different default, e.g. Fedora's `/run/postgresql`, can match it via `cabal.project` - no source patch needed), or `localhost` on Windows, unchanged. Connect-failure and handshake-failure messages now describe the socket path rather than a host/port pair when connecting this way.

  No privilege-elevation guard is applied to reading `PGHOST` (or the existing `PGUSER` lookup): libpq itself reads these with plain `getenv()`, with no `secure_getenv`/`geteuid`-vs-`getuid` check anywhere in `fe-connect.c` - responsibility for scrubbing the environment before opening a database connection is on any setuid/setgid caller, exactly as it is for libpq.

# v1.0.1.3

## Fixes

- Fixed an async exception around an aborted pipeline leaving a connection permanently stuck, with no timer able to reclaim it. Two changes, which are only a fix together:

  - `Query.getNextResult` now runs `mask_`ed. The connection's result bookkeeping - the pending-command counter, the separator flag, the `ParseComplete` origin FIFO - lives in separate `IORef`s that one logical transition updates in sequence. An interrupt landing between two of those updates left them inconsistent, and an inconsistent pair sends the next `getNextResult` off to wait for a message the backend has already decided not to send.

  - `Transport.receiveFrame` no longer runs `uninterruptibleMask_`ed. It buffers a whole frame before consuming any of it and takes it out of the buffer in one atomic step, so the framing 1.0.1.2 set out to protect stays intact - but the blocking wait is masked only across moving bytes off the socket, not across waiting for them. The 1.0.1.2 shape made every wait unabandonable, which is what turned the stall above into a deadlock `System.Timeout.timeout` could not break.

  Found via a hang in `hasql`'s `Integration.Sharing.Connection.Use.PipelineAbortedInterruptionCleanup`, which wedged only under concurrent load and only on this adapter.

# v1.0.1.2

## Fixes

- Fixed an async exception (e.g. from `System.Timeout.timeout`) landing mid-read permanently desyncing a connection's message framing. `Transport.receiveFrame` now runs `uninterruptibleMask_`ed, so a frame is either read to completion or not started, mirroring how `pqi-ffi`'s `safe` FFI call into `libpq` is structurally immune to the same hazard. Caught by the differential coverage in `pqi-conformance` 1.0.3.0 (#5).

# v1.0.1.1

## Fixes

- Builds on Windows: socket I/O and signal handling now branch on the host OS, selecting `Win32` in place of `unix`.

- Fixed a pipelined `sendPrepare` stealing the preceding `sendQueryParams`' `ParseComplete`, which produced a spurious `CommandOk` and shifted every later result by one. ParseComplete messages are now charged to the command that produced them via a per-command FIFO (#3).

# v1.0.1.0

## Non-breaking

- Picked up `pqi` 1.1.0.0, which renamed `Notify`'s fields to `notifyRelname`/`notifyBePid`/`notifyExtra` to match `postgresql-libpq`

# v1.0.0.1

Doc corrections.

# v1.0.0.0

## Non-breaking

- Support for the `resStatus` field of `Pqi.Adapter`

# v0.2.0.5

## Fixes

- Fixed the release process to actually result in publishing of the package.

# v0.2.0.4

## Fixes

- Fixed the connection closing.

# v0.2.0.3

Documentation corrections.

# v0.2.0.2

## Fixes

- Connection startup now forwards extra conninfo params (e.g. `application_name`, `options`) instead of silently dropping everything but `user` and `database`, so they reach the server the way libpq's do. Caught by the new differential coverage in `pqi-conformance` 0.1.1.0.

# v0.2.0.1

Documentation corrections.

# v0.2.0.0

## Breaking

- Hid the public sublibs.

# v0.1.0.1

## Fixes

- Adapted to GHC 8.10: pruned unsupported default-extensions (`ApplicativeDo`, `DuplicateRecordFields`, `NoFieldSelectors`, `OverloadedRecordDot`, `TemplateHaskell`) and rewrote the source accordingly

# v0.1.0.0

## Breaking

- Migrate to `pqi` 0.1's record-of-functions redesign and switch to exporting the `adapter` value instead of the `Connection` type.
