{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kuna.Java.KoreComp where

import Bound (Scope, instantiate)
import Control.Monad.Trans.RWS.Strict
import Text.Printf (printf)
import Data.Foldable (foldl')

import qualified Codec.JVM.ASM.Code as Code
import qualified Codec.JVM.Cond as CD
import qualified Data.Vector as V

import Kuna.Internal()
import Kuna.Kore.Syn (Expr(..), KoreExpr)
import Kuna.Java.Syn (BindLocal(..), JExpr(..), CName(..), VName(JLocalVar), fromMachType, jExprType, unpackLit)
import Kuna.Java.KoreComp.Types

import qualified Kuna.Kore.Mach as KMach
import qualified Kuna.Kore.Syn as K

{--
bar :: A -> B -> C -> D

foo :: A -> B -> C -> D
foo a b = bar

data JMethod = JMethod K.Name [(K.Name, JType)] JType JExpr

uncurry :: K.Name -> KoreExpr -> KoreComp (Maybe JMethod)
uncurry name expr = f expr [] where
  f (Lam n e) xs = f e n:xs
  f (App n e) xs = f e n:xs
  --}

buildJExpr :: KoreExpr -> ([Error], JCompExpr)
buildJExpr expr = runKoreComp $ compExpr expr

unsafeBuildJExpr :: KoreExpr -> JExpr
unsafeBuildJExpr expr = f $ buildJExpr expr where
  f (_, Done jexpr)  = jexpr
  f (_, Partial _)   = error "Unexpected partial expression."
  f (es, Failure)    = error $ unlines $ f' <$> es where f' (Error msg) = msg

mkJCall :: CName -> KMach.Call -> [JExpr] -> JExpr
mkJCall n c xs = JCall n (init ys) xs (last ys) where
  ys = fromMachType <$> KMach.callTypes c

runKoreComp :: JKoreComp -> ([Error], JCompExpr)
runKoreComp expr = f $ runRWS expr mempty () where f (a, _, es) = (es, a)

throw ::  String -> JKoreComp
throw msg = do
  tell $ [Error msg]
  return Failure

compExpr :: KoreExpr -> JKoreComp
compExpr expr = compJKoreExpr $ fmap Right expr

compJKoreExpr :: JKoreExpr -> JKoreComp
compJKoreExpr (Var (Left  bl))    = compVarLocal bl
compJKoreExpr (Var (Right name))  = compVarGlobal name
compJKoreExpr (Lit lit) = return . Done $ unpackLit lit
compJKoreExpr (App expr arg) = compApp expr arg
compJKoreExpr (Fld p ok ko) = compFld p ok ko
compJKoreExpr (Let scopes expr) = compLet scopes expr

compVarGlobal :: K.Name -> JKoreComp
compVarGlobal (K.Name id' K.Internal) = throw $ printf "Not in scope: '%v'" id'
compVarGlobal (K.Name id' K.Machine) =
  maybe (throw $ printf "Machine call '%v' not found." id') f $ KMach.callsById id' where
    f call = return . Partial $ callBldr call $ mkJCall name call where
      name = case call of
        KMach.PlusInt32 -> JOp KMach.PlusInt32 Code.iadd

compVarLocal :: VName -> JKoreComp
compVarLocal = return . Done . JVar

compApp :: JKoreExpr -> JKoreExpr -> JKoreComp
compApp expr arg = do
  ce <- compJKoreExpr expr
  case ce of
    Partial mk  -> doneOrFail arg f where
      f jexp = return $ buildCall mk jexp
    Done _      -> throw "unexpected App"
    Failure     -> return Failure

compFld :: JKoreExpr -> JKoreExpr -> JKoreExpr -> JKoreComp
compFld p ok ko =
  doneOrFail p fP where
    fP jexpP = doneOrFail ok fOK where
      fOK jexpOK = doneOrFail ko fKO where
        fKO jexpKO = return . Done $ JIf CD.NE jexpP jexpOK jexpKO rt where
          rt = jExprType jexpOK

compLet :: [Scope Int Expr (Either VName K.Name)] -> Scope Int Expr (Either VName K.Name) -> JKoreComp
compLet scopes expr = do
  (Done body) <- compJKoreExpr $ instantiate (f names) expr
  return . Done . snd $ foldl g (fromIntegral $ (length bounds) - 1, body) bounds where
    g (i, e) b = (i-1, JLocal (BindLocal i b $ jExprType b) e)
    f ns i = Var . Left $ ns V.! i
    (n, names, bounds) = foldr f' (0, V.empty, []) $ reverse scopes where
      f' scope (i, names, exprs) = (i + 1, V.cons name names, jexpr:exprs) where
        name = JLocalVar i $ jExprType jexpr
        jexpr = unsafeGetKoreComp . compJKoreExpr $ instantiate (\i -> Var . Left $ names V.! i) scope
    -- TODO Remove once we have facilities to work with the compiler stack.
    unsafeGetKoreComp :: JKoreComp -> JExpr
    unsafeGetKoreComp kc = let (_, (Done e)) = runKoreComp kc in e

doneOrFail :: JKoreExpr -> (JExpr -> JKoreComp) -> JKoreComp
doneOrFail ke f = do
      ce' <- compJKoreExpr ke
      case ce' of
        Done jexpr  -> f jexpr
        Partial _   -> throw "unexpected CallBldr"
        Failure     -> return Failure

{--
compExpr (Lam _) = throw "unexpected Lam"

compExpr (Let (Bind (K.Name id' K.Internal) (Lam bexpr))  expr) = do
  -- TODO Create methods (as fx, in writer?), and then bind ref
  -- --}
