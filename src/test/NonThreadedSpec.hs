-- | The conformance specs that only reproduce on the non-threaded RTS: a
-- delegate to 'Pqi.Conformance.nonThreadedSpecs', which brings up a
-- throwaway PostgreSQL container and runs the same differential comparisons
-- as the main battery ('Spec'), but on the RTS without the threaded
-- scheduler, which is the configuration some divergences live in. Built
-- without @-threaded@.
module Main (main) where

import Pqi.Conformance (nonThreadedSpecs)
import qualified Pqi.Native
import Prelude
import Test.Hspec

main :: IO ()
main = hspec (nonThreadedSpecs Pqi.Native.adapter)
