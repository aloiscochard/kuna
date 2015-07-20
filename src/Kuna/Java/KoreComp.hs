module Kuna.Java.KoreComp where

import Data.Maybe (fromMaybe)
import Kuna.Kore.Syn (Expr(..), KoreExpr)
import Kuna.Java.Syn (JExpr(..), JName(..), fromMachType, jExprType, unpackLit)

import qualified Codec.JVM.ASM.Code as Code
import qualified Codec.JVM.Cond as CD
import qualified Kuna.Kore.Syn as K
import qualified Kuna.Kore.Mach as KMach

data BuildCall = BuildCall { runBuildCall :: JExpr -> Either BuildCall JExpr }

mkBuildCall :: KMach.Call -> ([JExpr] -> JExpr) -> BuildCall
mkBuildCall call mk = BuildCall $ f [] where
  n = length $ KMach.callTypes call
  f xs e | length xs < (n-2)  = Left . BuildCall $ f (e:xs)
  f xs e                      = Right . mk $ reverse (e:xs)

unsafeBuildJExpr :: KoreExpr -> JExpr
unsafeBuildJExpr expr = either (const $ error "unexpected BuildCall") id (buildJExpr expr)

mkJCall :: JName -> KMach.Call -> [JExpr] -> JExpr
mkJCall n c xs = JCall n (init ys) xs (last ys) where
  ys = fromMachType <$> KMach.callTypes c

buildJExpr :: KoreExpr -> Either BuildCall JExpr
buildJExpr (Var (K.Name id' K.Machine)) = Left $ mkBuildCall call f where
  call = fromMaybe (error "call not found.") $ KMach.callsById id'
  f = mkJCall name call
  name = case call of
    KMach.PlusInt32 -> JOp Code.iadd

buildJExpr (Lit lit) = Right $ unpackLit lit

buildJExpr (App expr arg) = case buildJExpr expr of
  Left mk -> runBuildCall mk $ unsafeBuildJExpr arg
  Right _ -> error "unexpected App"

buildJExpr (Fld p ok ko) =
  Right $ JIf CD.NE expP expOK expKO rt
    where
      expP = unsafeBuildJExpr p
      expOK = unsafeBuildJExpr ok
      expKO = unsafeBuildJExpr ko
      rt = jExprType expOK

buildJExpr (Let bind expr) =
  -- TODO Add bind in scope to build field
  buildJExpr expr
