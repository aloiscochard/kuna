module Kuna.Core where

-- import Bound (Var(..))
import Data.Word (Word32)
import Data.Text (Text)

type Arg = Expr

data Expr
  = Var Name
  | Lit Literal
  | App Expr Expr
  -- | Lam Name (Expr b)
  -- | Let Bind (Expr b)

data Literal =
  LitInt32 Word32

data Name = Name
  { nameId    :: Text
  , nameSort  :: NameSort }

litInt32 :: Int -> Expr
litInt32 = Lit . LitInt32 . fromIntegral

var :: Name -> Expr
var = Var

{--
name :: Text -> Name
name id' = Name id' Internal
--}

machineName :: Text -> Name
machineName id' = Name id' Machine

data NameSort =
  --  Internal
  -- | External Module
  -- | Machine
  Machine

