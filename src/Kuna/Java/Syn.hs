module Kuna.Java.Syn where

import Codec.JVM.ASM.Code (Code)
import Codec.JVM.Cond (Cond)
import Codec.JVM.Const (ConstVal(..))
import Codec.JVM.Types (IClassName, FieldType(..), PrimType(..), UName, jlObject, jlString)
import Data.Word (Word8)

import Kuna.Kore.Syn (Literal(..))
import qualified Kuna.Kore.Mach as KMach

data JType
  = JPrim PrimType
  | JRef  IClassName

toFieldType :: JType -> FieldType
toFieldType (JPrim pt)  = BaseType pt
toFieldType (JRef cn) = ObjectType cn

fromMachType :: KMach.Type -> JType
fromMachType KMach.TyBool  = JPrim JBool
fromMachType KMach.TyInt32 = JPrim JInt
fromMachType KMach.TyData  = JRef jlObject

data CName
  = JMethod IClassName UName
  | JOp     Code

data VName
  = JLocalVar JType Word8
  -- | JField

data JExpr
  = JVar    VName
  | JConst  ConstVal
  | JCall   CName [JType] [JExpr] JType
  | JIf     Cond JExpr JExpr JExpr JType
  | JLocal  BindLocal JExpr

data BindLocal = BindLocal Int JExpr JType

jExprType :: JExpr -> JType
jExprType (JConst (CInteger _)) = JPrim JInt
jExprType (JConst (CString _))  = JRef jlString
jExprType (JCall _ _ _ tpe)     = tpe
jExprType (JIf _ _ _ _ tpe)     = tpe

unpackLit :: Literal -> JExpr
unpackLit = JConst . unpackLit'

unpackLit' :: Literal -> ConstVal
unpackLit' (LitInt32 bs) = CInteger $ fromIntegral bs

