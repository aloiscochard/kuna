{-# LANGUAGE OverloadedStrings #-}

import Codec.JVM.ASM (mkClassFile, mkMethodDef)
import Codec.JVM.ASM.Code (getstatic, invokestatic, invokevirtual, ireturn, vreturn)
import Codec.JVM.Class (ClassFile, putClassFile)
import Codec.JVM.Method (AccessFlag(..))
import Codec.JVM.Types
import Data.Binary.Put (runPut)
import Data.Foldable (fold)
import Data.Text (Text)

import qualified Data.ByteString.Lazy as BS

import Kuna.Core (Expr(..), apply, litInt32, machineName, name, var)

import qualified Kuna.JCore as JCore
import qualified Kuna.Mach as Mach

varM :: Mach.Call -> Expr
varM = var . machineName . Mach.callId

varI :: Text -> Expr
varI = var . name

conditionExpr :: Expr
conditionExpr =
  (Fld
    (litInt32 1)
    (litInt32 8)
    (litInt32 16))


additionExpr :: Expr
additionExpr = apply (varM Mach.PlusInt32) [litInt32 21, litInt32 21]

mainClass :: ClassFile
mainClass = mkClassFile java8 [] "Main" Nothing
  [ mkMethodDef [Public, Static] "foo"  []              (return jInt) $ fold
    [ JCore.compExpr conditionExpr
    , ireturn ]
  , mkMethodDef [Public, Static] "main" [arr jString]  void          $ fold
    [ getstatic systemOut
    , invokestatic foo
    , invokevirtual printlnI
    , vreturn ]
  ]
    where
      foo         = mkMethodRef "Main"                "foo"     []                            (return jInt)
      systemOut   = mkFieldRef  "java/lang/System"    "out"     (obj "java/io/PrintStream")
      printlnI    = mkMethodRef "java/io/PrintStream" "println" [jInt]                        void

main :: IO ()
main = BS.writeFile "Main.class" $ runPut . putClassFile $ mainClass
