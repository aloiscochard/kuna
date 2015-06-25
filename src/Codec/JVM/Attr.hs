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
import Codec.JVM.Types (PrimType(..))


data Attr
  = ACode
    { maxStack  :: Int
    , maxLocals :: Int
    , code      :: ByteString
    , codeAttrs :: [Attr] }
  | AStackMapTable [StackMapFrame]

instance Show Attr where
  show attr = "A" ++ (Text.unpack $ attrName attr)

attrName :: Attr -> Text
attrName (ACode _ _ _ _)      = "Code"
attrName (AStackMapTable _)   = "StackMapTable"

unpackAttr :: Attr -> [Const]
unpackAttr attr@(ACode _ _ _ xs) = (CUTF8 $ attrName attr):(unpackAttr =<< xs)
unpackAttr attr = return . CUTF8 . attrName $ attr

putAttr :: ConstPool -> Attr -> Put
putAttr cp attr = do
  putIx cp $ CUTF8 $ attrName attr
  let xs = runPut $ putAttrBody cp attr
  putI32 . fromIntegral $ LBS.length xs
  putByteString $ LBS.toStrict xs

putAttrBody :: ConstPool -> Attr -> Put
putAttrBody cp (ACode ms ls xs attrs) = do
  putI16 ms
  putI16 ls
  putI32 . fromIntegral $ BS.length xs
  putByteString xs
  putI16 0 -- TODO Exception table
  putI16 $ length attrs
  mapM_ (putAttr cp) attrs
putAttrBody _ (AStackMapTable xs) = do
  putI16 $ length xs
  mapM_ putStackMapFrame xs

-- http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.4

data StackMapFrame
  = SameFrame Int
  | SameLocals Int VerifType
  deriving Show

putStackMapFrame :: StackMapFrame -> Put
putStackMapFrame (SameFrame i)  = putWord8 $ fromIntegral i
putStackMapFrame (SameLocals i vt) = do
  putWord8 $ fromIntegral i + 64
  putVerifType vt

data VerifType
  = VPrim PrimType
  deriving Show

putVerifType :: VerifType -> Put
putVerifType (VPrim JInt) = putWord8 1
