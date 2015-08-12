module Kuna.Java.KoreComp.Types where

import Control.Monad.Trans.RWS.Strict
import Data.Map (Map)
import Data.Text (Text)

import qualified Data.Map.Strict as Map

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

type KoreComp a = (RWS () [Error] Scope a)

data Scope = Scope Int (Map Text JExpr)

emptyScope :: Scope
emptyScope = Scope 0 Map.empty

