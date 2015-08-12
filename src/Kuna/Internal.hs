module Kuna.Internal where

import Data.Text (Text, unpack)
import Text.Printf (PrintfArg, formatArg, formatString)

-- TODO Remove once https://github.com/bos/text/pull/132 is released
instance PrintfArg Text where
  formatArg txt = formatString $ unpack txt
