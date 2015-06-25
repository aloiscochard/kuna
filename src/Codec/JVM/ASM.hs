{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- | Usage:
--
-- You can assemble a java using the Writer DSL:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- import Data.Binary.Put (runPut)
-- import Data.Foldable (fold)
-- import qualified Data.ByteString.Lazy as BS
--
-- import Codec.JVM.ASM (mkClassFile, mkMethodDef)
-- import Codec.JVM.ASM.Code
-- import Codec.JVM.Class (ClassFile, putClassFile)
-- import Codec.JVM.Method (AccessFlag(..))
-- import Codec.JVM.Types
--
-- mainClass :: ClassFile
-- mainClass = mkClassFile java8 [] "HelloWorld" Nothing
--   [ mkMethodDef [Public, Static] "main" [arr.obj $ "java/lang/String"] void $ fold
--     [ getstatic systemOut
--     , bipush 42
--     , invokevirtual printlnI
--     , vreturn ]
--   ]
--     where
--       systemOut   = mkFieldRef  "java/lang/System"    "out"     (obj "java/io/PrintStream")
--       printlnI    = mkMethodRef "java/io/PrintStream" "println" [prim JInt]                   void
--
-- main :: IO ()
-- main = BS.writeFile "HelloWorld.class" $ runPut . putClassFile $ mainClass
-- @
--
module Codec.JVM.ASM where

import Data.Maybe (fromMaybe)
import Data.Text (Text)

import qualified Data.ByteString as BS
import qualified Data.Set as Set

import Codec.JVM.ASM.Code (Code)
import Codec.JVM.Attr (Attr(ACode), attrName)
import Codec.JVM.Class (ClassFile(..))
import Codec.JVM.Const (Const(..))
import Codec.JVM.ConstPool (mkConstPool)
import Codec.JVM.Method (MethodInfo(..))
import Codec.JVM.Types

import qualified Codec.JVM.ASM.Code as Code
import qualified Codec.JVM.Class as Class
import qualified Codec.JVM.Method as Method
import qualified Codec.JVM.ConstPool as CP

mkClassFile :: Version -> [Class.AccessFlag] -> IClassName -> Maybe IClassName -> [MethodDef] -> ClassFile
mkClassFile v afs tc sc mds = ClassFile cp v (Set.fromList afs) tc sc [] [] mis []
    where
      cs = ccs ++ mcs where
        ccs = concat [CP.unpackClassName tc, CP.unpackClassName $ fromMaybe jlObject sc]
        -- TODO Add attrName constants from attributes defined on codes, instead of harcoding that one
        mcs = (CUTF8 $ attrName (ACode 0 0 BS.empty)):(mds >>= unpackMethodDef)
      cp = mkConstPool cs
      mis = f <$> mds where
        f (MethodDef afs' n' (MethodDesc d as) code) =
          MethodInfo (Set.fromList afs') n' (Desc d) [Code.toAttr as cp code]

data MethodDef = MethodDef [Method.AccessFlag] UName MethodDesc Code

mkMethodDef :: [Method.AccessFlag] -> Text -> [FieldType] -> ReturnType -> Code -> MethodDef
mkMethodDef afs n fts rt c = mkMethodDef' afs n (mkMethodDesc fts rt) c

mkMethodDef' :: [Method.AccessFlag] -> Text -> MethodDesc -> Code -> MethodDef
mkMethodDef' afs n md c = MethodDef afs (UName n) md c

unpackMethodDef :: MethodDef -> [Const]
unpackMethodDef (MethodDef _ (UName n') (MethodDesc d _) code) = CUTF8 n':CUTF8 d:Code.consts code
