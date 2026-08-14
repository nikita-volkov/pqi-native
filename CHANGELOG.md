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
