{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kuna.Java.KoreComp where

import Bound (instantiate)
import Control.Monad.Trans.RWS.Strict
import Text.Printf (printf)

import qualified Codec.JVM.ASM.Code as Code
import qualified Codec.JVM.Cond as CD
import qualified Data.Vector as V

import Kuna.Internal()
import Kuna.Kore.Syn (Expr(..), KoreExpr)
import Kuna.Java.Syn (JExpr(..), CName(..), fromMachType, jExprType, unpackLit)
import Kuna.Java.KoreComp.Types

import qualified Kuna.Kore.Mach as KMach
import qualified Kuna.Kore.Syn as K

buildJExpr :: KoreExpr -> ([Error], CompExpr)
buildJExpr expr = runKoreComp $ compExpr expr

unsafeBuildJExpr :: KoreExpr -> JExpr
unsafeBuildJExpr expr = f $ buildJExpr expr where
  f (_, Done jexpr)  = jexpr
  f (_, Partial _)   = error "Unexpected partial expression."
  f (es, Failure)    = error $ unlines $ f' <$> es where f' (Error msg) = msg

mkJCall :: CName -> KMach.Call -> [JExpr] -> JExpr
mkJCall n c xs = JCall n (init ys) xs (last ys) where
  ys = fromMachType <$> KMach.callTypes c

runKoreComp :: KoreComp a -> ([Error], a)
runKoreComp expr = f $ runRWS expr mempty () where f (a, _, es) = (es, a)

throw ::  String -> KoreComp CompExpr
throw msg = do
  tell $ [Error msg]
  return Failure

compExpr :: KoreExpr -> KoreComp CompExpr
compExpr (Var (K.Name id' K.Machine)) =
  maybe (throw $ printf "Machine call '%v' not found." id') f $ KMach.callsById id' where
    f call = return . Partial $ callBldr call $ mkJCall name call where
      name = case call of
        KMach.PlusInt32 -> JOp Code.iadd

compExpr (Var (K.Name id' K.Internal)) = throw $ printf "Not in scope: '%v'" id'

compExpr (Lit lit) = return . Done $ unpackLit lit

compExpr (App expr arg) = do
  ce <- compExpr expr
  case ce of
    Partial mk  -> doneOrFail arg f where
      f jexp = return $ buildCall mk jexp
    Done _      -> throw "unexpected App"
    Failure     -> return Failure

compExpr (Fld p ok ko) =
  doneOrFail p fP where
    fP jexpP = doneOrFail ok fOK where
      fOK jexpOK = doneOrFail ko fKO where
        fKO jexpKO = return . Done $ JIf CD.NE jexpP jexpOK jexpKO rt where
          rt = jExprType jexpOK

compExpr (Let scopes expr) =
  compExpr $ instantiate (bounds V.!) expr where
    bounds = foldr f V.empty scopes where
      f scope xs = V.snoc xs $ instantiate (xs V.!) scope

doneOrFail :: KoreExpr -> (JExpr -> KoreComp CompExpr) -> KoreComp CompExpr
doneOrFail ke f = do
      ce' <- compExpr ke
      case ce' of
        Done jexpr  -> f jexpr
        Partial _   -> throw "unexpected CallBldr"
        Failure     -> return Failure
