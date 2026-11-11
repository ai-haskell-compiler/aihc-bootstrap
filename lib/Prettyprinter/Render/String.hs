module Prettyprinter.Render.String where

import Prettyprinter.Internal (SimpleDocStream, renderShowS)

renderString :: SimpleDocStream ann -> String
renderString sds = renderShowS sds ""
