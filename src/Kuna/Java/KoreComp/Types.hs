module Kuna.Java.KoreComp.Types where

import Control.Monad.Trans.RWS.Strict

import Kuna.Java.Syn (JExpr(..))

import qualified Kuna.Kore.Mach as KMach

data CompExpr = Failure | Partial CallBldr | Done JExpr

data CallBldr = CallBldr { buildCall :: JExpr -> CompExpr }

callBldr :: KMach.Call -> ([JExpr] -> JExpr) -> CallBldr
callBldr call mk = CallBldr $ f [] where
  n = length $ KMach.callTypes call
  f xs e | length xs < (n-2)  = Partial . CallBldr $ f (e:xs)
  f xs e                      = Done . mk $ reverse (e:xs)

data Error = Error String

type KoreComp a = (RWS () [Error] () a)
