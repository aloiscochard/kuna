{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kuna.Java.KoreComp where

import Bound (Scope, instantiate)
import Control.Monad.Trans.RWS.Strict
import Text.Printf (printf)

import qualified Codec.JVM.ASM.Code as Code
import qualified Codec.JVM.Cond as CD
import qualified Data.Vector as V

import Kuna.Internal()
import Kuna.Kore.Syn (Expr(..), KoreExpr)
import Kuna.Java.Syn (BindLocal(..), JExpr(..), CName(..), fromMachType, jExprType, unpackLit)
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

runKoreComp :: KoreComp -> ([Error], CompExpr)
runKoreComp expr = f $ runRWS expr mempty () where f (a, _, es) = (es, a)

throw ::  String -> KoreComp
throw msg = do
  tell $ [Error msg]
  return Failure

type JKoreExpr = Expr (Either BindLocal K.Name)

compExpr :: KoreExpr -> KoreComp
compExpr expr = compJKoreExpr $ fmap Right expr

compJKoreExpr :: JKoreExpr -> KoreComp
compJKoreExpr (Var (Left  bl))    = compVarLocal bl
compJKoreExpr (Var (Right name))  = compVarGlobal name
compJKoreExpr (Lit lit) = return . Done $ unpackLit lit
compJKoreExpr (App expr arg) = compApp expr arg
compJKoreExpr (Fld p ok ko) = compFld p ok ko
compJKoreExpr (Let scopes expr) = compLet scopes expr

compVarGlobal :: K.Name -> KoreComp
compVarGlobal (K.Name id' K.Internal) = throw $ printf "Not in scope: '%v'" id'
compVarGlobal (K.Name id' K.Machine) =
  maybe (throw $ printf "Machine call '%v' not found." id') f $ KMach.callsById id' where
    f call = return . Partial $ callBldr call $ mkJCall name call where
      name = case call of
        KMach.PlusInt32 -> JOp KMach.PlusInt32 Code.iadd

compVarLocal :: BindLocal -> KoreComp
compVarLocal = undefined

compApp :: JKoreExpr -> JKoreExpr -> KoreComp
compApp expr arg = do
  ce <- compJKoreExpr expr
  case ce of
    Partial mk  -> doneOrFail arg f where
      f jexp = return $ buildCall mk jexp
    Done _      -> throw "unexpected App"
    Failure     -> return Failure

compFld :: JKoreExpr -> JKoreExpr -> JKoreExpr -> KoreComp
compFld p ok ko =
  doneOrFail p fP where
    fP jexpP = doneOrFail ok fOK where
      fOK jexpOK = doneOrFail ko fKO where
        fKO jexpKO = return . Done $ JIf CD.NE jexpP jexpOK jexpKO rt where
          rt = jExprType jexpOK


-- TODO Does not instantiate (inline) inter-dependencies but bind to local variables instead.
compLet :: [Scope Int Expr (Either BindLocal K.Name)] -> Scope Int Expr (Either BindLocal K.Name) -> KoreComp
compLet scopes expr =
  compJKoreExpr $ instantiate f expr where
    f i = bounds V.! i
    bounds = foldr f V.empty scopes where
      f scope xs = V.snoc xs $ instantiate (xs V.!) scope

doneOrFail :: JKoreExpr -> (JExpr -> KoreComp) -> KoreComp
doneOrFail ke f = do
      ce' <- compJKoreExpr ke
      case ce' of
        Done jexpr  -> f jexpr
        Partial _   -> throw "unexpected CallBldr"
        Failure     -> return Failure
