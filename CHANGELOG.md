# Upcoming

## Fixes

- Adapted to GHC 8.10: pruned unsupported default-extensions (`ApplicativeDo`, `DuplicateRecordFields`, `NoFieldSelectors`, `OverloadedRecordDot`, `TemplateHaskell`) and rewrote the source accordingly

# v0.1.0.0

## Breaking

- Migrate to `pqi` 0.1's record-of-functions redesign and switch to exporting the `adapter` value instead of the `Connection` type.
