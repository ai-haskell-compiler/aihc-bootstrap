module GHC.Clock where

import Data.Word (Word64)

foreign import prim "monotonicNanoseconds" getMonotonicTimeNSec :: IO Word64
