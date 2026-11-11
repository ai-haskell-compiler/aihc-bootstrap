module Data.Array.ST where

-- Arrays that are changed in place, in ST. Upstream these functions work
-- for several kinds of array in several monads; AIHC uses the unboxed kind
-- in ST, and nothing is unboxed here.

import Control.Monad.ST (ST (..))
import Data.Array (Array (..), position, rangeSize)
import Interpreter.Array (MutableArray)
import qualified Interpreter.Array as A

data STUArray s i e = STUArray (i, i) (MutableArray e)

type STArray = STUArray

newArray :: Enum i => (i, i) -> e -> ST s (STUArray s i e)
newArray limits initial = ST (fmap (STUArray limits) (A.new (rangeSize limits) initial))

newArray_ :: Enum i => (i, i) -> ST s (STUArray s i e)
newArray_ limits = newArray limits A.uninitialised

newListArray :: Enum i => (i, i) -> [e] -> ST s (STUArray s i e)
newListArray limits list = ST (pure (STUArray limits (A.fromListN (rangeSize limits) list)))

readArray :: Enum i => STUArray s i e -> i -> ST s e
readArray (STUArray limits elements) i = ST (A.read elements (position limits i))

writeArray :: Enum i => STUArray s i e -> i -> e -> ST s ()
writeArray (STUArray limits elements) i e = ST (A.write elements (position limits i) e)

modifyArray :: Enum i => STUArray s i e -> i -> (e -> e) -> ST s ()
modifyArray array i f = readArray array i >>= \e -> writeArray array i (f e)

getBounds :: STUArray s i e -> ST s (i, i)
getBounds (STUArray limits _) = pure limits

getNumElements :: STUArray s i e -> ST s Int
getNumElements (STUArray _ elements) = pure (A.length elements)

getElems :: STUArray s i e -> ST s [e]
getElems (STUArray _ elements) = ST (mapM (A.read elements) [0 .. A.length elements - 1])

-- A copy that is not changed any more.
freeze :: STUArray s i e -> ST s (Array i e)
freeze (STUArray limits elements) = ST (fmap (Array limits) (A.clone elements 0 (A.length elements)))

thaw :: Array i e -> ST s (STUArray s i e)
thaw (Array limits elements) = ST (fmap (STUArray limits) (A.clone elements 0 (A.length elements)))

-- The same elements, on the promise that they are not changed any more.
unsafeFreeze :: STUArray s i e -> ST s (Array i e)
unsafeFreeze (STUArray limits elements) = pure (Array limits elements)

unsafeThaw :: Array i e -> ST s (STUArray s i e)
unsafeThaw (Array limits elements) = pure (STUArray limits elements)
