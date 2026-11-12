module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BSU
import Foreign.Ptr (castPtr)
import GHC.Fingerprint (Fingerprint (..), fingerprintData, fingerprintString)

md5 :: B.ByteString -> IO Fingerprint
md5 bytes = BSU.unsafeUseAsCStringLen bytes (\(ptr, size) -> fingerprintData (castPtr ptr) size)

main :: IO ()
main = do
  mapM_ (\s -> md5 (BC.pack s) >>= print) ["", "a", "abc", "message digest", "The quick brown fox jumps over the lazy dog"]
  mapM_ (\n -> md5 (B.replicate n 97) >>= print) [55, 56, 57, 63, 64, 65, 119, 120, 128, 1000]
  md5 (B.pack [0 .. 255]) >>= \f -> print (f, f == f, compare f (Fingerprint 0 0))
  md5 (BC.pack "abc") >>= \(Fingerprint a b) -> print (a, b)
  print (fingerprintString "abc", fingerprintString "λ")
