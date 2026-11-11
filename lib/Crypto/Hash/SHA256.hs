module Crypto.Hash.SHA256 where

-- SHA-256 (src/sha256.c). A context is the pieces given so far: the hash
-- is computed when it is asked for.

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Prelude hiding (init)

newtype Ctx = Ctx [ByteString]

-- The 32 bytes of the hash.
foreign import prim "sha256" hash :: ByteString -> ByteString

-- Lazy byte strings are strict ones here (Data.ByteString.Lazy).
hashlazy :: ByteString -> ByteString
hashlazy = hash

init :: Ctx
init = Ctx []

update :: Ctx -> ByteString -> Ctx
update (Ctx pieces) bytes = Ctx (bytes : pieces)

updates :: Ctx -> [ByteString] -> Ctx
updates = foldl update

finalize :: Ctx -> ByteString
finalize (Ctx pieces) = hash (B.concat (reverse pieces))
