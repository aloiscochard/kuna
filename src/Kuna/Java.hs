module Kuna.Java where

import Codec.JVM.ASM.Code (Code)
import Codec.JVM.Const (ConstVal(..))
import Codec.JVM.Types (jInt, mkMethodRef)
import Data.Foldable (fold)
import Data.Monoid ((<>))

import qualified Codec.JVM.ASM.Code as Code

import Kuna.Kore.Syn (KoreExpr)
import Kuna.Java.Syn (BindLocal(..), JExpr(..), CName(..), VName(..), toFieldType)
import Kuna.Java.KoreComp (unsafeBuildJExpr)

-- TODO Error handling! (return position, ...)

compExpr :: KoreExpr -> Code
compExpr = compJExpr . unsafeBuildJExpr

compConst :: ConstVal -> Code
compConst (CInteger i) | i < 128    = Code.bipush jInt $ fromIntegral i
compConst (CInteger i) | i < 32768  = Code.sipush jInt $ fromIntegral i
compConst cv                        = Code.ldc cv

compJExpr :: JExpr -> Code
compJExpr (JVar (JLocalVar n tpe))    = Code.iload n
compJExpr (JConst c)                  = compConst c
compJExpr (JCall name jts jargs jrt)  = argsCode <> compCall name
  where
    argsCode = (fold $ compJExpr <$> jargs)
    fts = toFieldType <$> jts
    rt = Just $ toFieldType jrt
    compCall (JMethod cn mn)  = Code.invokestatic mr where mr = mkMethodRef cn mn fts rt
    compCall (JOp _ c)        = c

compJExpr (JIf cd p ok ko jrt) =
  compJExpr p <> Code.iif cd (compJExpr ok) (compJExpr ko)

compJExpr (JLocal (BindLocal n exp tpe) exp') =
  compJExpr exp <> Code.istore n <> compJExpr exp'
