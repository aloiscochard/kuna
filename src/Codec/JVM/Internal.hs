module Codec.JVM.Internal where

import Data.Binary.Put (Put, putWord16be, putWord32be)

putI16 :: Int -> Put
putI16 = putWord16be . fromIntegral

putI32 :: Int -> Put
putI32 = putWord32be . fromIntegral
