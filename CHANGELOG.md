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
