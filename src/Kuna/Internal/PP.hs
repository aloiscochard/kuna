module Kuna.Internal.PP where

import System.IO (stdout)
import Text.PrettyPrint.ANSI.Leijen

printDoc :: Doc -> IO ()
printDoc doc = displayIO stdout $ renderSmart 1 80 $ doc <> linebreak

keyword :: String -> Doc
keyword txt = dullyellow $ text txt <> space

local :: Show a => a -> Doc
local i = dullgreen $ text $ concat ["#", show i]
