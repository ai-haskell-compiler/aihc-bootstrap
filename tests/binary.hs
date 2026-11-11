module Main where

import Control.Applicative ((<|>))
import Control.Monad (replicateM, when)
import qualified Data.Binary.Get as Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word16, Word8)

built :: Builder.Builder -> [Word8]
built = BL.unpack . Builder.toLazyByteString

-- A record as an object file would write it: a tag, a length, the bytes.
putRecord :: Word8 -> [Word8] -> Put
putRecord tag payload = do
  putWord8 tag
  putWord16le (fromIntegral (length payload))
  putByteString (B.pack payload)

getRecord :: Get.Get (Word8, [Word8])
getRecord = do
  tag <- Get.getWord8
  size <- Get.getWord16le
  payload <- Get.getByteString (fromIntegral size)
  pure (tag, B.unpack payload)

-- Reads records until a zero tag is next, without reading that tag.
getRecords :: Get.Get [(Word8, [Word8])]
getRecords = do
  next <- Get.lookAhead Get.getWord8
  if next == 0
    then pure []
    else (:) <$> getRecord <*> getRecords

run :: Show a => Get.Get a -> [Word8] -> IO ()
run get input = case Get.runGetOrFail get (BL.pack input) of
  Left (rest, offset, _) -> putStrLn ("failed at " ++ show offset ++ ", left " ++ show (BL.unpack rest))
  Right (rest, offset, a) -> putStrLn (show a ++ " at " ++ show offset ++ ", left " ++ show (BL.unpack rest))

main :: IO ()
main = do
  print (built mempty)
  print (built (Builder.word8 1 <> Builder.word8 2 <> mempty <> Builder.word8 3))
  print (built (Builder.char8 'a' <> Builder.string8 "bc\255" <> Builder.char8 '\n'))
  print (built (Builder.byteString (B.pack [9, 8]) <> Builder.lazyByteString (BL.pack [7, 6])))
  print (built (Builder.word16BE 0x0102 <> Builder.word16LE 0x0102))
  print (built (Builder.word32BE 0xDEADBEEF <> Builder.word32LE 0xDEADBEEF))
  print (built (Builder.word64BE 0x0102030405060708 <> Builder.word64LE 0xFFEEDDCCBBAA9988))
  print (built (Builder.int8 (-1) <> Builder.int16LE (-2) <> Builder.int32BE (-3) <> Builder.int64LE (-4)))
  print (built (mconcat (map Builder.word8 [1 .. 20])))
  print (built (foldl (\b w -> b <> Builder.word8 w) mempty [1 .. 10]))
  print (built (foldr (\w b -> Builder.word8 w <> b) mempty [1 .. 10]))

  print (BL.unpack (runPut (pure ())))
  print (BL.unpack (runPut (putWord8 1 >> putWord16le 0x0203 >> putWord32le 0x04050607)))
  print (BL.unpack (runPut (putWord64le 0x0102030405060708 >> putInt64le (-2))))
  print (BL.unpack (runPut (putWord16be 0x0102 >> putWord32be 0x03040506 >> putWord64be 7)))
  print (BL.unpack (runPut (putLazyByteString (BL.pack [1, 2]) >> putByteString (B.pack [3]))))
  print (BL.unpack (runPut (mapM_ (\n -> when (even n) (putWord8 n)) [1 .. 9])))
  print (BL.unpack (runPut (putWord8 1 <> mempty <> putWord8 2)))
  let records = runPut (putRecord 5 [1, 2, 3] >> putRecord 6 [] >> putRecord 7 [255] >> putWord8 0 >> putWord8 99)
  print (BL.unpack records)
  print (BL.length records)

  run getRecords (BL.unpack records)
  run getRecord [5, 3, 0, 1, 2]
  run Get.getWord8 []
  run Get.getWord8 [200, 1]
  run (replicateM 3 Get.getWord8) [1, 2, 3, 4]
  run ((,,) <$> Get.getWord16be <*> Get.getWord32be <*> Get.getWord64be) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
  run ((,,) <$> Get.getWord16le <*> Get.getWord32le <*> Get.getWord64le) [1, 2, 3, 4, 5, 6, 0, 0, 0, 0, 0, 0, 0, 128]
  run ((,) <$> Get.getInt8 <*> Get.getInt16be) [255, 255, 254]
  run (B.unpack <$> Get.getByteString 2) [1, 2, 3]
  run (B.unpack <$> Get.getByteString 0) [1, 2, 3]
  run (BL.unpack <$> (Get.getWord8 >> Get.getRemainingLazyByteString)) [1, 2, 3]
  run (Get.skip 2 >> Get.bytesRead) [1, 2, 3]
  run ((,) <$> Get.isEmpty <*> (Get.getWord8 >> Get.isEmpty)) [1]
  run (Get.lookAhead (replicateM 2 Get.getWord8)) [1, 2, 3]
  run (Get.getWord8 >> fail "no" :: Get.Get Word8) [1, 2, 3]
  run ((Get.getWord8 >> Get.getWord8 >> fail "no") <|> Get.getWord8) [1, 2, 3]
  run (fmap (+ 1) Get.getWord16be :: Get.Get Word16) [0, 255]
