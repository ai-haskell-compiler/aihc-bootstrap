module System.Info where

foreign import prim "systemOs" os :: String

foreign import prim "systemArch" arch :: String
