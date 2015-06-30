module Kuna.Core where

-- import Bound (Var(..))
import Data.Word (Word32)
import Data.Text (Text)

type Arg = Expr

data Expr
  = Var Name
  | Lit Literal
  | App Expr Expr
  | Fld Expr Expr Expr
  -- | Lam Name Expr
  -- | Let Bind Expr

apply :: Expr -> [Expr] -> Expr
apply v as = foldr f v as where f a tree = App tree a

data Literal =
  LitInt32 Word32

litInt32 :: Int -> Expr
litInt32 = Lit . LitInt32 . fromIntegral

data Name = Name
  { nameId    :: Text
  , nameSort  :: NameSort }

var :: Name -> Expr
var = Var

data Bind = Bind Name Expr

name :: Text -> Name
name id' = Name id' Internal

machineName :: Text -> Name
machineName id' = Name id' Machine

data NameSort
  = Internal
  -- | External Module
  | Machine
