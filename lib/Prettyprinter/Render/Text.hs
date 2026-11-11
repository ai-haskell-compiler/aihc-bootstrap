module Prettyprinter.Render.Text where

import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter.Internal
import System.IO (Handle, hPutStr, stdout)

renderStrict :: SimpleDocStream ann -> Text
renderStrict sds = T.pack (renderShowS sds "")

-- Lazy text is strict text here (Data.Text.Lazy).
renderLazy :: SimpleDocStream ann -> Text
renderLazy = renderStrict

renderIO :: Handle -> SimpleDocStream ann -> IO ()
renderIO handle sds = hPutStr handle (renderShowS sds "")

putDoc :: Doc ann -> IO ()
putDoc = hPutDoc stdout

hPutDoc :: Handle -> Doc ann -> IO ()
hPutDoc handle doc = renderIO handle (layoutPretty defaultLayoutOptions doc)
