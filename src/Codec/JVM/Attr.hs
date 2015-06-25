{-# LANGUAGE OverloadedStrings #-}
module Codec.JVM.Attr where

import Data.ByteString (ByteString)
import Data.Binary.Put (Put, putByteString, putWord8, runPut)
import Data.Text (Text)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text

import Codec.JVM.Const (Const(CUTF8))
import Codec.JVM.ConstPool (ConstPool, putIx)
import Codec.JVM.Internal (putI16, putI32)

data Attr
  = ACode
    { maxStack  :: Int
    , maxLocals :: Int
    , code      :: ByteString }
  | AStackMapTable [StackMapFrame]

instance Show Attr where
  show attr = "A" ++ (Text.unpack $ attrName attr)

attrName :: Attr -> Text
attrName (ACode _ _ _)        = "Code"
attrName (AStackMapTable _)   = "StackMapTable"

unpackAttr :: Attr -> [Const]
unpackAttr = return . CUTF8 . attrName

putAttr :: ConstPool -> Attr -> Put
putAttr cp attr = do
  putIx cp $ CUTF8 $ attrName attr
  let xs = runPut $ putAttrBody attr
  putI32 . fromIntegral $ LBS.length xs
  putByteString $ LBS.toStrict xs

putAttrBody :: Attr -> Put
putAttrBody (ACode ms ls xs) = do
  putI16 ms
  putI16 ls
  putI32 . fromIntegral $ BS.length xs
  putByteString xs
  putI16 0 -- TODO Exception table
  putI16 0 -- TODO Attributes
putAttrBody (AStackMapTable xs) = do
  putI16 $ length xs
  mapM_ putStackMapFrame xs

data StackMapFrame = Same Int

putStackMapFrame :: StackMapFrame -> Put
putStackMapFrame (Same i) = putWord8 $ fromIntegral i
