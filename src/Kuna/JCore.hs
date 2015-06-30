module Kuna.JCore where

import Codec.JVM.ASM.Code (Code)
import Codec.JVM.Cond (Cond)
import Codec.JVM.Const (ConstVal(..))
import Codec.JVM.Types (IClassName, FieldType(..), PrimType(..), UName, jInt, jlObject, jlString, mkMethodRef)
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import qualified Codec.JVM.ASM.Code as Code
import qualified Codec.JVM.Cond as CD

import Kuna.Core (Expr(..), Literal(..))

import qualified Kuna.Core as Core
import qualified Kuna.Mach as Mach

-- TODO Error handling! (return position, ...)

compExpr :: Expr -> Code
compExpr = compJExpr . unsafeBuildJExpr

data JType
  = JPrim PrimType
  | JRef  IClassName

toFieldType :: JType -> FieldType
toFieldType (JPrim pt)  = BaseType pt
toFieldType (JRef cn) = ObjectType cn

fromMachType :: Mach.Type -> JType
fromMachType Mach.TyBool  = JPrim JBool
fromMachType Mach.TyInt32 = JPrim JInt
fromMachType Mach.TyData  = JRef jlObject

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

compConst :: ConstVal -> Code
compConst (CInteger i) | i < 128    = Code.bipush jInt $ fromIntegral i
compConst (CInteger i) | i < 32768  = Code.sipush jInt $ fromIntegral i
compConst cv                        = Code.ldc cv

compJExpr :: JExpr -> Code
compJExpr (JConst c) = compConst c

compJExpr (JCall name jts jargs jrt) = argsCode <> compCall name
  where
    argsCode = (fold $ compJExpr <$> jargs)
    fts = toFieldType <$> jts
    rt = Just $ toFieldType jrt
    compCall (JMethod cn mn)  = Code.invokestatic mr where mr = mkMethodRef cn mn fts rt
    compCall (JOp c)          = c

compJExpr (JIf cd p ok ko jrt) = compJExpr p <> Code.iif cd rt (compJExpr ok) (compJExpr ko) where
    rt = Just $ toFieldType jrt

data BuildCall = BuildCall { runBuildCall :: JExpr -> Either BuildCall JExpr }

mkBuildCall :: Mach.Call -> ([JExpr] -> JExpr) -> BuildCall
mkBuildCall call mk = BuildCall $ f [] where
  n = length $ Mach.callTypes call
  f xs e | length xs < (n-2)  = Left . BuildCall $ f (e:xs)
  f xs e                      = Right . mk $ reverse (e:xs)

unsafeBuildJExpr :: Expr -> JExpr
unsafeBuildJExpr expr = either (const $ error "unexpected BuildCall") id (buildJExpr expr)

mkJCall :: JName -> Mach.Call -> [JExpr] -> JExpr
mkJCall n c xs = JCall n (init ys) xs (last ys) where
  ys = fromMachType <$> Mach.callTypes c

buildJExpr :: Expr -> Either BuildCall JExpr
buildJExpr (Var (Core.Name id' Core.Machine)) = Left $ mkBuildCall call f where
  call = fromMaybe (error "call not found.") $ Mach.callsById id'
  f = mkJCall name call
  name = case call of
    Mach.PlusInt32 -> JOp Code.iadd

buildJExpr (Lit lit)                          = Right $ unpackLit lit
buildJExpr (App expr arg)                     = case buildJExpr expr of
  Left mk -> runBuildCall mk $ unsafeBuildJExpr arg
  Right _ -> error "unexpected App"
buildJExpr (Fld p ok ko)                     =
  Right $ JIf CD.NE expP expOK expKO rt
    where
      expP = unsafeBuildJExpr p
      expOK = unsafeBuildJExpr ok
      expKO = unsafeBuildJExpr ko
      rt = jExprType expOK

