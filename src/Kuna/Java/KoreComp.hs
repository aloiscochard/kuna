{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kuna.Java.KoreComp where

import Control.Monad.Trans.RWS.Strict
import Data.Text (Text)
import Text.Printf (printf)

import qualified Codec.JVM.ASM.Code as Code
import qualified Codec.JVM.Cond as CD
import qualified Data.Map.Strict as Map

import Kuna.Internal()
import Kuna.Kore.Syn (Expr(..), KoreExpr, Bind(..))
import Kuna.Java.Syn (JExpr(..), CName(..), VName(..), fromMachType, jExprType, unpackLit, unpackLit')
import Kuna.Java.KoreComp.Types

import qualified Kuna.Kore.Mach as KMach
import qualified Kuna.Kore.Syn as K

buildJExpr :: KoreExpr -> ([Error], CompExpr)
buildJExpr expr = runKoreComp emptyScope $ compExpr expr

unsafeBuildJExpr :: KoreExpr -> JExpr
unsafeBuildJExpr expr = f $ buildJExpr expr where
  f (_, Done jexpr)  = jexpr
  f (_, Partial _)   = error "Unexpected partial expression."
  f (es, Failure)    = error $ unlines $ f' <$> es where f' (Error msg) = msg

mkJCall :: CName -> KMach.Call -> [JExpr] -> JExpr
mkJCall n c xs = JCall n (init ys) xs (last ys) where
  ys = fromMachType <$> KMach.callTypes c

runKoreComp :: Scope -> KoreComp a -> ([Error], a)
runKoreComp scope expr = f $ runRWS expr mempty scope where f (a, _, es) = (es, a)

scoped :: KoreComp a -> KoreComp a
scoped kc = do
  scope <- get
  x <- kc
  put scope
  return x

throw ::  String -> KoreComp CompExpr
throw msg = do
  tell $ [Error msg]
  return Failure

bind :: Text -> JExpr -> KoreComp ()
bind n v = modify f where f (Scope i xs) = Scope i $ Map.insert n v xs

bindVar :: Text -> (Int -> JExpr) -> KoreComp Int
bindVar n f = do
  i <- next
  bind n $ f i
  modify inc
  return i where
    next = get >>= \(Scope i _) -> return $ i + 1
    inc (Scope i xs) = Scope (i + 1) xs

bindVar_ :: Text -> (Int -> JExpr) -> KoreComp ()
bindVar_ n f = bindVar n f >> return ()

compExpr :: KoreExpr -> KoreComp CompExpr
compExpr (Var (K.Name id' K.Machine)) =
  maybe (throw $ printf "Machine call '%v' not found." id') f $ KMach.callsById id' where
    f call = return . Partial $ callBldr call $ mkJCall name call where
      name = case call of
        KMach.PlusInt32 -> JOp Code.iadd

compExpr (Var (K.Name id' K.Internal)) = do
  (Scope _ ls) <- get
  case Map.lookup id' ls of
    Just expr -> return $ Done expr
    Nothing   -> throw $ printf "Local variable '%v' not bound." id'

compExpr (Lit lit) = return . Done $ unpackLit lit

compExpr (App expr arg) = do
  ce <- compExpr expr
  case ce of
    Partial mk  -> doneOrFail arg f where
      f jexp = return $ buildCall mk jexp
    Done _      -> throw "unexpected App"
    Failure     -> return Failure

compExpr (Fld p ok ko) =
  scoped $ doneOrFail p fP where
    fP jexpP = scoped $ doneOrFail ok fOK where
      fOK jexpOK = scoped $ doneOrFail ko fKO where
        fKO jexpKO = return . Done $ JIf CD.NE jexpP jexpOK jexpKO rt where
          rt = jExprType jexpOK

compExpr (Let (Bind (K.Name id' K.Internal) bexpr)  expr) = do
  case bexpr of
    (Lit lit) -> do
      bind id' $ JConst $ unpackLit' lit
      compExpr expr
    bexpr'    -> doneOrFail bexpr' f where
      f jexp = do
        bindVar_ id' $ \i -> JVar $ JLocalVar i $ jExprType jexp
        compExpr expr

compExpr (Let (Bind (K.Name id' sort) _)  _) =
  throw $ printf "Invalid name sort '%v' in local binding '%v'." (show sort) id'

doneOrFail :: KoreExpr -> (JExpr -> KoreComp CompExpr) -> KoreComp CompExpr
doneOrFail ke f = do
      ce' <- compExpr ke
      case ce' of
        Done jexpr  -> f jexpr
        Partial _   -> throw "unexpected CallBldr"
        Failure     -> return Failure
