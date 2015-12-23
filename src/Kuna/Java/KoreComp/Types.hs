{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Kuna.Java.KoreComp.Types where

import Control.Monad.Trans.RWS.Strict

import Kuna.Java.Syn (JExpr(..), VName)

import qualified Kuna.Kore.Syn as K
import qualified Kuna.Kore.Mach as KMach

type JKoreComp = KoreComp JExpr
type JKoreExpr = K.Expr (Either VName K.Name)
type JCompExpr = CompExpr JExpr

-- TODO Better name?
data CompExpr a = Failure | Partial CallBldr | Done a
  deriving (Functor, Foldable, Traversable)

data CallBldr = CallBldr { buildCall :: JExpr -> CompExpr JExpr }

callBldr :: KMach.Call -> ([JExpr] -> JExpr) -> CallBldr
callBldr call mk = CallBldr $ f [] where
  n = length $ KMach.callTypes call
  f xs e | length xs < (n-2)  = Partial . CallBldr $ f (e:xs)
  f xs e                      = Done . mk $ reverse (e:xs)

data Error = Error String

type KoreComp a = (RWS () [Error] () (CompExpr a))
