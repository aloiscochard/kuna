module Kuna.Internal.PP where

import System.IO (stdout)
import Text.PrettyPrint.ANSI.Leijen

printDoc :: Doc -> IO ()
printDoc doc = displayIO stdout $ renderSmart 1 80 $ doc <> linebreak

keyword :: String -> Doc
keyword txt = keyword' txt <> space

keyword' :: String -> Doc
keyword' txt = dullyellow $ text txt

local :: String -> Doc
local txt = local' $ concat ["#", txt]

local' :: String -> Doc
local' txt = dullgreen $ text $ txt
