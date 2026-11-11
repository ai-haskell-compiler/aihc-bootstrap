module Data.Binary.Get where

-- Reading bytes in a monad. An action is given the whole input and how far
-- into it reading has come. It answers with the new position and a result,
-- or with the position where it failed and a message.

import Control.Applicative (Alternative (..))
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int
import Data.Word

type ByteOffset = Int64

newtype Get a = Get (ByteString -> Int -> Either (Int, String) (Int, a))

instance Functor Get where
  fmap f (Get get) = Get (\input position -> case get input position of
    Left failure -> Left failure
    Right (next, a) -> Right (next, f a))

instance Applicative Get where
  pure a = Get (\_ position -> Right (position, a))
  (<*>) getF getA = getF >>= (\f -> fmap f getA)

instance Monad Get where
  (>>=) (Get get) f = Get (\input position -> case get input position of
    Left failure -> Left failure
    Right (next, a) -> case f a of
      Get rest -> rest input next)

instance MonadFail Get where
  fail message = Get (\_ position -> Left (position, message))

-- The second action starts where the first one started.
instance Alternative Get where
  empty = fail "Data.Binary.Get(Alternative).empty"
  (<|>) (Get first) (Get second) = Get (\input position -> case first input position of
    Left _ -> second input position
    Right result -> Right result)

-- Lazy byte strings are strict ones here (Data.ByteString.Lazy). Either
-- way the answer has the input that was not read, and how much was.
runGetOrFail ::
  Get a ->
  ByteString ->
  Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, a)
runGetOrFail (Get get) input = case get input 0 of
  Left (position, message) -> Left (B.drop position input, fromIntegral position, message)
  Right (position, a) -> Right (B.drop position input, fromIntegral position, a)

runGet :: Get a -> ByteString -> a
runGet get input = case runGetOrFail get input of
  Left (_, _, message) -> error message
  Right (_, _, a) -> a

-- The next `count` bytes.
getByteString :: Int -> Get ByteString
getByteString count = Get (\input position ->
  if count <= B.length input - position
    then Right (position + max 0 count, B.take count (B.drop position input))
    else Left (position, "not enough bytes"))

getLazyByteString :: Int64 -> Get ByteString
getLazyByteString count = getByteString (fromIntegral count)

getRemainingLazyByteString :: Get ByteString
getRemainingLazyByteString = Get (\input position -> Right (B.length input, B.drop position input))

skip :: Int -> Get ()
skip count = fmap (\_ -> ()) (getByteString count)

isEmpty :: Get Bool
isEmpty = Get (\input position -> Right (position, position >= B.length input))

bytesRead :: Get Int64
bytesRead = Get (\_ position -> Right (position, fromIntegral position))

-- Runs the action without moving forward.
lookAhead :: Get a -> Get a
lookAhead (Get get) = Get (\input position -> case get input position of
  Left failure -> Left failure
  Right (_, a) -> Right (position, a))

getWord8 :: Get Word8
getWord8 = fmap B.head (getByteString 1)

getInt8 :: Get Int8
getInt8 = fmap fromIntegral getWord8

-- The next `count` bytes as one number, the highest byte first.
bigEndian :: Int -> Get Word64
bigEndian count = fmap (B.foldl' (\w byte -> shiftL w 8 .|. fromIntegral byte) 0) (getByteString count)

littleEndian :: Int -> Get Word64
littleEndian count = fmap (B.foldr (\byte w -> shiftL w 8 .|. fromIntegral byte) 0) (getByteString count)

getWord16be :: Get Word16
getWord16be = fmap fromIntegral (bigEndian 2)

getWord32be :: Get Word32
getWord32be = fmap fromIntegral (bigEndian 4)

getWord64be :: Get Word64
getWord64be = bigEndian 8

getWord16le :: Get Word16
getWord16le = fmap fromIntegral (littleEndian 2)

getWord32le :: Get Word32
getWord32le = fmap fromIntegral (littleEndian 4)

getWord64le :: Get Word64
getWord64le = littleEndian 8

getInt16be :: Get Int16
getInt16be = fmap fromIntegral getWord16be

getInt32be :: Get Int32
getInt32be = fmap fromIntegral getWord32be

getInt64be :: Get Int64
getInt64be = fmap fromIntegral getWord64be

getInt16le :: Get Int16
getInt16le = fmap fromIntegral getWord16le

getInt32le :: Get Int32
getInt32le = fmap fromIntegral getWord32le

getInt64le :: Get Int64
getInt64le = fmap fromIntegral getWord64le
