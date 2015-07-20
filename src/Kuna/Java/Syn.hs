module Kuna.Java.Syn where

import Codec.JVM.ASM.Code (Code)
import Codec.JVM.Cond (Cond)
import Codec.JVM.Const (ConstVal(..))
import Codec.JVM.Types (IClassName, FieldType(..), PrimType(..), UName, jlObject, jlString)

import Kuna.KoreSyn (Literal(..))
import qualified Kuna.KoreMach as KMach

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

data JName
  = JMethod IClassName UName
  | JOp     Code

data JExpr
  = JConst  ConstVal
  | JCall   JName [JType] [JExpr] JType
  | JIf     Cond JExpr JExpr JExpr JType

jExprType :: JExpr -> JType
jExprType (JConst (CInteger _)) = JPrim JInt
jExprType (JConst (CString _))  = JRef jlString
jExprType (JCall _ _ _ tpe)     = tpe
jExprType (JIf _ _ _ _ tpe)     = tpe

unpackLit :: Literal -> JExpr
unpackLit (LitInt32 bs) = JConst . CInteger $ fromIntegral bs

