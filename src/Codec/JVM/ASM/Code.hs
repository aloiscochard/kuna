module Codec.JVM.ASM.Code where

import Data.Binary.Put (Put, putByteString, putWord8, putWord16be, runPut)
import Data.Monoid ((<>))
import Data.Word (Word8, Word16)

import Codec.JVM.ASM.Code.CtrlFlow
import Codec.JVM.Attr (Attr(ACode))
import Codec.JVM.Const (Const(..), ConstVal)
import Codec.JVM.ConstPool (ConstPool)
import Codec.JVM.Internal (putI16)
import Codec.JVM.Opcode (Opcode, opcode)
import Codec.JVM.Types

import qualified Codec.JVM.ConstPool as CP
import qualified Codec.JVM.Opcode as OP
import qualified Data.ByteString.Lazy as BS

-- TODO Return `Either` with error if flowVal reach negative value (need to add a flag in Flow)
toAttr :: Int -> ConstPool -> Code -> Attr
toAttr as cp code = ACode maxStack' maxLocals' $ runCode code cp where
  maxLocals' = max as $ flMax $ cfLocals cf
  maxStack' = flMax $ cfStack cf
  cf = ctrlFlow $ code

data Code = Code
  { consts    :: [Const]
  , ctrlFlow  :: CtrlFlow
  , runCode   :: ConstPool -> Put }


instance Monoid Code where
  mempty = Code [] mempty (const $ return ())
  mappend c0 c1 = Code cs cf f where
    cs = (consts c0 ++ consts c1)
    cf = mappend (ctrlFlow c0) (ctrlFlow c1)
    f cp = runCode c0 cp *> runCode c1 cp

mkCode :: [Const] -> CtrlFlow -> (ConstPool -> Put) -> Code
mkCode cs cf f = Code cs cf f

mkCode' :: [Const] -> Int -> (ConstPool -> Put) -> Code
mkCode' cs s f = Code cs cf f where cf = mkCtrlFlow (flowInc s) mempty

mkCodeStack :: Opcode -> Flow -> Code
mkCodeStack op' fl = mkCode [] cf $ const (putOpcode op') where
  cf = mkCtrlFlow fl mempty

mkCodeIdx :: Opcode -> Const -> Code
mkCodeIdx op' c = mkCode' cs 2 f where
  cs = CP.unpack c
  f cp = do
    putOpcode op'
    putI16 $ fromIntegral . CP.ix $ CP.unsafeIndex c cp


codeBytes :: Int -> Put -> Code
codeBytes s xs = Code [] cf $ const xs where cf = mkCtrlFlow (flowInc s) mempty

putOpcode :: Opcode -> Put
putOpcode = putWord8 . opcode

op :: Int -> Opcode -> Code
op s op' = codeBytes s $ putOpcode op'

--
-- Operations
--

bipush :: Word8 -> Code
bipush w = codeBytes 1 $ putOpcode OP.bipush *> putWord8 w

sipush :: Word16 -> Code
sipush w = codeBytes 2 $ putOpcode OP.sipush *> putWord16be w

ldc :: ConstVal -> Code
ldc cv = mkCodeIdx OP.ldc $ CValue cv

aload_0 :: Code
aload_0 = mkCodeStack OP.aload_0 $ flowInc 1

invoke :: Opcode -> MethodRef -> Code
invoke op'  mr@(MethodRef _ _ (MethodDesc _ as)) = mkCode cs (mkCtrlFlow flow mempty) f where
  flow = flowDec as
  c = CMethodRef mr
  cs = CP.unpack c
  f cp = do
    putOpcode op'
    putWord16be $ fromIntegral . CP.ix $ CP.unsafeIndex c cp

invokevirtual :: MethodRef -> Code
invokevirtual = invoke OP.invokevirtual

invokespecial :: MethodRef -> Code
invokespecial = invoke OP.invokespecial

invokestatic :: MethodRef -> Code
invokestatic = invoke OP.invokestatic

iadd :: Code
iadd = mkCodeStack OP.iadd $ flowDec 2 <> flowInc 1

vreturn :: Code
vreturn = op 0 OP.vreturn

getstatic :: FieldRef -> Code
getstatic fr = mkCodeIdx OP.getstatic $ CFieldRef fr
