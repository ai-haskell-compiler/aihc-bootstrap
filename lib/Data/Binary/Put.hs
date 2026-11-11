module Data.Binary.Put where

-- Writing bytes in a monad: an action is its result and the bytes it wrote.

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.Int
import Data.Word

data PutM a = PutM a Builder

type Put = PutM ()

instance Functor PutM where
  fmap f (PutM a written) = PutM (f a) written

instance Applicative PutM where
  pure a = PutM a mempty
  (<*>) (PutM f first) (PutM a second) = PutM (f a) (first <> second)

instance Monad PutM where
  (>>=) (PutM a first) f = case f a of
    PutM b second -> PutM b (first <> second)

instance Semigroup (PutM ()) where
  (<>) first second = first >> second

instance Monoid (PutM ()) where
  mempty = pure ()

-- Lazy byte strings are strict ones here (Data.ByteString.Lazy).
runPut :: Put -> ByteString
runPut (PutM _ written) = Builder.toLazyByteString written

execPut :: PutM a -> Builder
execPut (PutM _ written) = written

putBuilder :: Builder -> Put
putBuilder = PutM ()

putByteString :: ByteString -> Put
putByteString bytes = putBuilder (Builder.byteString bytes)

putLazyByteString :: ByteString -> Put
putLazyByteString = putByteString

putWord8 :: Word8 -> Put
putWord8 w = putBuilder (Builder.word8 w)

putInt8 :: Int8 -> Put
putInt8 i = putBuilder (Builder.int8 i)

putWord16le :: Word16 -> Put
putWord16le w = putBuilder (Builder.word16LE w)

putWord32le :: Word32 -> Put
putWord32le w = putBuilder (Builder.word32LE w)

putWord64le :: Word64 -> Put
putWord64le w = putBuilder (Builder.word64LE w)

putWord16be :: Word16 -> Put
putWord16be w = putBuilder (Builder.word16BE w)

putWord32be :: Word32 -> Put
putWord32be w = putBuilder (Builder.word32BE w)

putWord64be :: Word64 -> Put
putWord64be w = putBuilder (Builder.word64BE w)

putInt16le :: Int16 -> Put
putInt16le i = putBuilder (Builder.int16LE i)

putInt32le :: Int32 -> Put
putInt32le i = putBuilder (Builder.int32LE i)

putInt64le :: Int64 -> Put
putInt64le i = putBuilder (Builder.int64LE i)

putInt16be :: Int16 -> Put
putInt16be i = putBuilder (Builder.int16BE i)

putInt32be :: Int32 -> Put
putInt32be i = putBuilder (Builder.int32BE i)

putInt64be :: Int64 -> Put
putInt64be i = putBuilder (Builder.int64BE i)
