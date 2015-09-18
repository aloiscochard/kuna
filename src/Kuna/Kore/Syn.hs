{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Kuna.Kore.Syn where

import Bound (Scope, abstract)
import Control.Monad (ap)
import Control.Monad.Trans.Class (lift)
import Data.List (elemIndex)
import Data.Text (Text)
import Data.Word (Word32)
import Prelude.Extras

type Arg = Expr
type Pre = Expr
type Bind = Scope Int Expr
type Body = Scope Int Expr

type KoreExpr = Expr Name

data Expr b
  = Var b
  | Lit Literal
  | App (Expr b) (Arg b)
  | Fld (Pre b) (Expr b) (Expr b)
--  | Lam (Scope Int Exp b)
  | Let [Bind b] (Body b)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

apply :: Expr b -> [Expr b] -> Expr b
apply v as = foldr f v as where f a tree = App tree a

bindings :: Eq a => [(a, Expr a)] -> Expr a -> Expr a
bindings [] b = b
bindings bs b = Let (map (abstr . snd) bs) (abstr b)
  where abstr = abstract (`elemIndex` map fst bs)

data Literal = LitInt32 Word32
  deriving (Eq, Ord, Show, Read)

litInt32 :: Int -> Expr b
litInt32 = Lit . LitInt32 . fromIntegral

data Name = Name
  { nameId    :: Text
  , nameSort  :: NameSort }
  deriving (Eq, Ord, Show, Read)

var :: Name -> KoreExpr
var = Var

name :: Text -> Name
name id' = Name id' Internal

machineName :: Text -> Name
machineName id' = Name id' Machine

data NameSort
  = Internal
  -- | External Module
  | Machine
  deriving (Eq, Ord, Show, Read)

instance Eq1 Expr
instance Ord1 Expr
instance Show1 Expr
instance Read1 Expr
instance Applicative Expr where
  pure = Var
  (<*>) = ap

instance Monad Expr where
  return = Var
  Var a >>= f = f a
  Lit l >>= _ = Lit l
  App x y >>= f = App (x >>= f) (y >>= f)
  Fld p ok ko >>= f = Fld (p >>= f) (ok >>= f) (ko >>= f)
  -- Lam e  >>= f = Lam (e >>>= f)
  Let scopes expr >>= f = Let (fmap (>>= (lift . f)) scopes) (expr >>= (lift . f))
