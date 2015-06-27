module Codec.JVM.ASM.Code where

import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.Monoid ((<>))
import Data.Word (Word8, Word16)

import qualified Data.ByteString as BS

import Codec.JVM.ASM.Code.Instr (Instr, runInstr)
import Codec.JVM.Attr (Attr(ACode, AStackMapTable))
import Codec.JVM.Cond (Cond)
import Codec.JVM.Const (Const(..), ConstVal, constValType)
import Codec.JVM.ConstPool (ConstPool)
import Codec.JVM.Internal (packWord16be)
import Codec.JVM.Opcode (Opcode)
import Codec.JVM.Types

import qualified Codec.JVM.ASM.Code.CtrlFlow as CF
import qualified Codec.JVM.Cond as CD
import qualified Codec.JVM.ASM.Code.Instr as IT
import qualified Codec.JVM.ConstPool as CP
import qualified Codec.JVM.Opcode as OP

-- TODO Return `Either` with error (currently CF.pop is unsafe)
toAttrs :: Int -> ConstPool -> Code -> [Attr]
toAttrs as cp code = f $ runInstr (instr code) cp where
  f (xs, cf, frames) = [ACode maxStack' maxLocals' xs attrs] where
      maxLocals' = max as $ CF.maxLocals cf
      maxStack' = CF.maxStack cf
      attrs = if null frames then [] else [AStackMapTable frames]

data Code = Code
  { consts  :: [Const]
  , instr   :: Instr }

instance Monoid Code where
  mempty = Code mempty mempty
  mappend (Code cs0 i0) (Code cs1 i1) = Code (mappend cs0 cs1) (mappend i0 i1)

mkCode :: [Const] -> Instr -> Code
mkCode cs i = Code cs i

mkCode' :: Instr -> Code
mkCode' = mkCode []

codeConst :: Opcode -> FieldType -> Const -> Code
codeConst oc ft c = mkCode cs $ fold
  [ IT.op oc
  , IT.ix c
  , IT.mapStack $ CF.push ft ]
    where cs = CP.unpack c

codeBytes :: ByteString -> Code
codeBytes bs = mkCode [] $ IT.bytes bs

op :: Opcode -> Code
op = mkCode' . IT.op

pushBytes :: Opcode -> FieldType -> ByteString -> Code
pushBytes oc ft bs = mkCode' $ fold
  [ IT.op oc
  , IT.bytes bs
  , IT.mapStack $ CF.push ft ]

--
-- Operations
--

bipush :: FieldType -> Word8 -> Code
bipush ft w = pushBytes OP.bipush ft $ BS.singleton w

sipush :: FieldType -> Word16 -> Code
sipush ft w = pushBytes OP.sipush ft $ packWord16be w

ldc :: ConstVal -> Code
ldc cv = codeConst OP.ldc ft $ CValue cv where ft = constValType cv

invoke :: Opcode -> MethodRef -> Code
invoke oc mr@(MethodRef _ _ fts rt) = mkCode cs $ fold
  [ IT.op oc
  , IT.ix c
  , IT.mapStack $ CF.pop (length fts) <> maybe mempty CF.push rt ]
    where
      c = CMethodRef mr
      cs = CP.unpack c

invokevirtual :: MethodRef -> Code
invokevirtual = invoke OP.invokevirtual

invokespecial :: MethodRef -> Code
invokespecial = invoke OP.invokespecial

invokestatic :: MethodRef -> Code
invokestatic = invoke OP.invokestatic

iadd :: Code
iadd = mkCode' $ IT.op OP.iadd <> i where
  i = IT.mapStack $ CF.pop 2 <> CF.push jInt

iif :: Cond -> ReturnType -> Code -> Code -> Code
iif cond rt ok ko = mkCode cs $ IT.branch oc rt (instr ko) (instr ok) where
  cs = [ok, ko] >>= consts
  oc = case cond of
          CD.EQ -> OP.ifeq
          CD.NE -> OP.ifne

ifne :: ReturnType -> Code -> Code -> Code
ifne = iif CD.NE

ifeq :: ReturnType -> Code -> Code -> Code
ifeq = iif CD.EQ

ireturn :: Code
ireturn = op OP.ireturn

vreturn :: Code
vreturn = op OP.vreturn

getstatic :: FieldRef -> Code
getstatic fr@(FieldRef _ _ ft) = codeConst OP.getstatic ft $ CFieldRef fr
