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

import Kuna.Kore.Syn (Expr(..), KoreExpr, apply, bind, litInt32, machineName, name, var)

import qualified Kuna.Java as J
import qualified Kuna.Kore.Mach as KMach

varM :: KMach.Call -> KoreExpr
varM = var . machineName . KMach.callId

varI :: Text -> KoreExpr
varI = var . name

conditionExpr :: KoreExpr
conditionExpr =
  (Fld
    (litInt32 1)
    (litInt32 8)
    (litInt32 16))

additionExpr :: KoreExpr
additionExpr = bind "x" (litInt32 21) $ apply (varM KMach.PlusInt32) [varI "x", varI "x"]

mainClass :: KoreExpr -> ClassFile
mainClass expr = mkClassFile java8 [] "Main" Nothing
  [ mkMethodDef [Public, Static] "foo"  []              (return jInt) $ fold
    [ J.compExpr expr
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
main = do
  print expr
  BS.writeFile "Main.class" $ runPut . putClassFile $ mainClass expr
    where expr = additionExpr
