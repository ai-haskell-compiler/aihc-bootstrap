module Main where

-- Needs the package cryptohash-sha256, which GHC does not ship. The
-- expected output is from `shasum -a 256` on the same bytes, and from the
-- test vectors of the standard.

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Numeric (showHex)

hex :: B.ByteString -> String
hex = concatMap byte . B.unpack
  where
    byte b = let digits = showHex b "" in replicate (2 - length digits) '0' ++ digits

main :: IO ()
main = do
  putStrLn (hex (SHA256.hash B.empty))
  putStrLn (hex (SHA256.hash (BC.pack "abc")))
  putStrLn (hex (SHA256.hash (BC.pack "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")))
  putStrLn (hex (SHA256.hash (BC.pack "The quick brown fox jumps over the lazy dog")))
  -- Lengths around the block size, where the padding changes shape.
  mapM_ (\n -> putStrLn (show n ++ " " ++ hex (SHA256.hash (B.replicate n 97)))) [55, 56, 57, 63, 64, 65, 119, 120, 128, 1000]
  putStrLn (hex (SHA256.hash (B.pack [0 .. 255])))
  putStrLn (hex (SHA256.hashlazy (BL.fromChunks [BC.pack "ab", BC.pack "c"])))
  putStrLn (hex (SHA256.finalize (SHA256.updates SHA256.init [BC.pack "a", B.empty, BC.pack "bc"])))
  putStrLn (hex (SHA256.finalize (SHA256.update (SHA256.update SHA256.init (BC.pack "ab")) (BC.pack "c"))))
  print (B.length (SHA256.hash (BC.pack "x")), SHA256.finalize SHA256.init == SHA256.hash B.empty)
