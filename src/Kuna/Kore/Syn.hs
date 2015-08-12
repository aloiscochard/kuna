module Kuna.Kore.Syn where

-- import Bound (Var(..))
import Data.Word (Word32)
import Data.Text (Text)

type Arg = Expr
type Pre = Expr

type KoreExpr = Expr Name

data Expr b
  = Var b
  | Lit Literal
  | App (Expr b) (Arg b)
  | Fld (Pre b) (Expr b) (Expr b)
--  | Lam b (Expr b)
  | Let (Bind b) (Expr b)

apply :: Expr b -> [Expr b] -> Expr b
apply v as = foldr f v as where f a tree = App tree a

data Literal =
  LitInt32 Word32

litInt32 :: Int -> Expr b
litInt32 = Lit . LitInt32 . fromIntegral

bind :: Text -> KoreExpr -> KoreExpr -> KoreExpr
bind txt e = Let $ Bind (Name txt Internal) e

data Name = Name
  { nameId    :: Text
  , nameSort  :: NameSort }
  deriving (Eq, Ord)

var :: Name -> KoreExpr
var = Var

data Bind b = Bind b (Expr b)

name :: Text -> Name
name id' = Name id' Internal

machineName :: Text -> Name
machineName id' = Name id' Machine

data NameSort
  = Internal
  -- | External Module
  | Machine
  deriving (Eq, Ord, Show)
