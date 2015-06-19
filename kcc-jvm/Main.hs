{-# LANGUAGE OverloadedStrings #-}

import Codec.JVM.ASM (mkClassFile, mkMethodDef)
import Codec.JVM.ASM.Code (getstatic, invokevirtual, vreturn)
import Codec.JVM.Class (ClassFile, putClassFile)
import Codec.JVM.Method (AccessFlag(..))
import Codec.JVM.Types
import Data.Binary.Put (runPut)
import Data.Foldable (fold)

import qualified Data.ByteString.Lazy as BS

import Kuna.Core (Expr(..), litInt32, machineName, var)

import qualified Kuna.JCore as JCore
import qualified Kuna.Mach as Mach

simpleExpr :: Expr
simpleExpr =
  App
    (App
      (var . machineName . Mach.callId $ Mach.PlusInt32)
      (litInt32 21))
    (litInt32 21)

mainClass :: ClassFile
mainClass = mkClassFile java8 [] "Main" Nothing
  [ mkMethodDef [Public, Static] "main" [arr.obj $ "java/lang/String"] void $ fold
    [ getstatic systemOut
    , JCore.compExpr simpleExpr
    , invokevirtual printlnI
    , vreturn ]
  ]
    where
      systemOut   = mkFieldRef  "java/lang/System"    "out"     (obj "java/io/PrintStream")
      printlnI    = mkMethodRef "java/io/PrintStream" "println" [prim JInt]                   void

main :: IO ()
main = BS.writeFile "Main.class" $ runPut . putClassFile $ mainClass
