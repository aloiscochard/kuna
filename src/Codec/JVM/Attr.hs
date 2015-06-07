{-# LANGUAGE OverloadedStrings #-}
module Codec.JVM.Attr where

import Data.Binary.Put (Put, putByteString, runPut)
import Data.Text (Text)

import qualified Data.ByteString.Lazy as BS

import Codec.JVM.Const (Const(CUTF8))
import Codec.JVM.ConstPool (ConstPool, putIx)
import Codec.JVM.Internal (putI16, putI32)

data Attr
  = ACode
    { maxStack  :: Int
    , maxLocals :: Int
    , code      :: Put }

instance Show Attr where
  show (ACode _ _ _) = "ACode" -- TODO Print debug information

codeAttrName :: Text
codeAttrName  = "Code"

putAttr :: ConstPool -> Attr -> Put
putAttr cp attr = do
  putIx cp $ CUTF8 codeAttrName
  let xs = runPut $ putAttrBody attr
  putI32 . fromIntegral $ BS.length xs
  putByteString $ BS.toStrict xs

putAttrBody :: Attr -> Put
putAttrBody (ACode ms ls putCode) = do
  putI16 ms
  putI16 ls
  let xs = runPut putCode
  putI32 . fromIntegral $ BS.length xs
  putByteString $ BS.toStrict xs
  putI16 0 -- TODO Exception table
  putI16 0 -- TODO Attributes
