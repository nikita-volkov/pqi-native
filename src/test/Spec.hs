-- | The native adapter's conformance test suite: a single delegate to
-- 'Pqi.Conformance.specs', which brings up a throwaway PostgreSQL
-- container, runs the full differential battery (core, capabilities, and
-- SCRAM) against the FFI reference, and tears the container down again.
module Main (main) where

import Data.Proxy (Proxy (..))
import Pqi.Conformance (specs)
import Pqi.Native (Connection)
import Test.Hspec
import Prelude

main :: IO ()
main = hspec (specs (Proxy @Connection))
