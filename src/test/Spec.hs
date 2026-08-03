-- | The native adapter's conformance test suite: a single delegate to
-- 'Pqi.Conformance.specs', which brings up a throwaway PostgreSQL
-- container, runs the full differential battery (core, capabilities, and
-- SCRAM) against the FFI reference, and tears the container down again.
module Main (main) where

import Pqi.Conformance (specs)
import qualified Pqi.Native
import Prelude
import Test.Hspec

main :: IO ()
main = hspec (specs Pqi.Native.adapter)
