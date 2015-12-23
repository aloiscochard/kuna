{-# LANGUAGE OverloadedStrings #-}

import Codec.JVM.ASM (mkClassFile, mkMethodDef)
import Codec.JVM.ASM.Code (Code, getstatic, invokestatic, invokevirtual, ireturn, vreturn)
import Codec.JVM.Class (ClassFile, putClassFile)
import Codec.JVM.Method (AccessFlag(..))
import Codec.JVM.Types
import Data.Binary.Put (runPut)
import Data.Foldable (fold)
import Data.Text (Text)

import qualified Data.ByteString.Lazy as BS

import Kuna.Internal.PP (printDoc)
import Kuna.Java.Syn.PP (prettyJExpr)
import Kuna.Kore.Syn (Expr(..), KoreExpr, apply, bindings, litInt32, machineName, name, var)
import Kuna.Kore.Syn.PP (prettyExpr)

import qualified Kuna.Java as J
import qualified Kuna.Java.KoreComp as JComp
import qualified Kuna.Kore.Mach as KMach

varM :: KMach.Call -> KoreExpr
varM = var . machineName . KMach.callId

varI :: Text -> KoreExpr
varI = var . name

bind :: [(Text, KoreExpr)] -> KoreExpr -> KoreExpr
bind bs = bindings $ fmap (\(n, expr) -> (name n, expr)) bs

conditionExpr :: KoreExpr
conditionExpr =
  (Fld
    (litInt32 1)
    (litInt32 8)
    (litInt32 16))

additionExpr :: KoreExpr
additionExpr = bind [("x", litInt32 21)] $ apply (varM KMach.PlusInt32) [varI "x", varI "x"]

localExpr :: KoreExpr
localExpr = bind [("x", apply (varM KMach.PlusInt32) [litInt32 8, litInt32 4])] $
  apply (varM KMach.PlusInt32) [varI "x", varI "x"]

mainClass :: Code -> ClassFile
mainClass expr = mkClassFile java8 [] "Main" Nothing
  [ mkMethodDef [Public, Static] "foo"  []              (return jInt) $ fold
    [ expr
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
  printDoc $ prettyExpr expr
  putStrLn ""
  printDoc $ prettyJExpr jexpr
  BS.writeFile "Main.class" $ runPut . putClassFile $ mainClass $ J.compJExpr jexpr
    where
      expr = localExpr
      jexpr = JComp.unsafeBuildJExpr expr
