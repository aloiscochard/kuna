module Codec.JVM.ASM.Code where

import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.Monoid ((<>))
import Data.Word (Word8, Word16)

import qualified Data.ByteString as BS

import Codec.JVM.ASM.Code.CtrlFlow
import Codec.JVM.Attr (Attr(ACode))
import Codec.JVM.Cond (Cond)
import Codec.JVM.Const (Const(..), ConstVal)
import Codec.JVM.ConstPool (ConstPool)
import Codec.JVM.Internal (packI16, packWord16be)
import Codec.JVM.Opcode (Opcode, opcode)
import Codec.JVM.Types

import qualified Codec.JVM.Cond as CD
import qualified Codec.JVM.ConstPool as CP
import qualified Codec.JVM.Opcode as OP

-- TODO Return `Either` with error if flowVal reach negative value (need to add a flag in Flow)
toAttrs :: Int -> ConstPool -> Code -> [Attr]
toAttrs as cp code = [ACode maxStack' maxLocals' $ runCode code cp 0] where
  maxLocals' = max as $ flMax $ cfLocals cf
  maxStack' = flMax $ cfStack cf
  cf = ctrlFlow $ code

data Code = Code
  { consts    :: [Const]
  , ctrlFlow  :: CtrlFlow
  , runCode   :: ConstPool -> Int -> ByteString }

instance Monoid Code where
  mempty = Code [] mempty (const $ const BS.empty)
  mappend c0 c1 = Code cs cf f where
    cs = (consts c0 ++ consts c1)
    cf = mappend (ctrlFlow c0) (ctrlFlow c1)
    f cp off = xs <> runCode c1 cp (off + BS.length xs) where
      xs = runCode c0 cp off

mkCode :: [Const] -> CtrlFlow -> (ConstPool -> Int -> ByteString) -> Code
mkCode cs cf f = Code cs cf f

mkCode' :: [Const] -> Int -> (ConstPool -> Int -> ByteString) -> Code
mkCode' cs s f = Code cs cf f where cf = mkCtrlFlow (flowInc s) mempty

mkCodeStack :: Opcode -> Flow -> Code
mkCodeStack op' fl = mkCode [] cf $ const $ const (packOpcode op') where
  cf = mkCtrlFlow fl mempty

mkCodeIdx :: Opcode -> Const -> Code
mkCodeIdx op' c = mkCode' cs 2 f where
  cs = CP.unpack c
  f cp _ =
    (packOpcode op')                                          <>
    (packI16 $ fromIntegral . CP.ix $ CP.unsafeIndex c cp)


codeBytes :: Int -> ByteString -> Code
codeBytes s xs = Code [] cf $ const $ const xs where cf = mkCtrlFlow (flowInc s) mempty

packOpcode :: Opcode -> ByteString
packOpcode op' = BS.pack [opcode op']

op :: Int -> Opcode -> Code
op s op' = codeBytes s $ packOpcode op'

--
-- Operations
--

bipush :: Word8 -> Code
bipush w = codeBytes 1 $ packOpcode OP.bipush <> BS.pack [w]

sipush :: Word16 -> Code
sipush w = codeBytes 2 $ packOpcode OP.sipush <> packWord16be w

ldc :: ConstVal -> Code
ldc cv = mkCodeIdx OP.ldc $ CValue cv

aload_0 :: Code
aload_0 = mkCodeStack OP.aload_0 $ flowInc 1

invoke :: Opcode -> MethodRef -> Code
invoke op'  mr@(MethodRef _ _ (MethodDesc _ as)) = mkCode cs (mkCtrlFlow flow mempty) f where
  flow = flowDec as
  c = CMethodRef mr
  cs = CP.unpack c
  f cp _ = do
    packOpcode op' <> (packWord16be $ fromIntegral . CP.ix $ CP.unsafeIndex c cp)

invokevirtual :: MethodRef -> Code
invokevirtual = invoke OP.invokevirtual

invokespecial :: MethodRef -> Code
invokespecial = invoke OP.invokespecial

invokestatic :: MethodRef -> Code
invokestatic = invoke OP.invokestatic

iadd :: Code
iadd = mkCodeStack OP.iadd $ flowDec 2 <> flowInc 1

iif :: Cond -> Code -> Code -> Code
iif cond ok ko  = mkCode cs (mkCtrlFlow flow mempty) f where
  flow = flowDec 1
  cs = [ok, ko] >>= consts
  op' = case cond of
          CD.EQ -> OP.ifeq
          CD.NE -> OP.ifne
  f cp off = fold [xs, ys, zs] where
    xs = fold [packOpcode op', packI16 . fromIntegral $ (BS.length ys + off + 1)]
    ys = runCode ko cp (off + BS.length xs)
    zs = fold [packOpcode OP.goto, packI16 . fromIntegral $ (BS.length okbs + 3), okbs] where
      okbs = runCode ok cp (BS.length xs + BS.length ys + 3)

vreturn :: Code
vreturn = op 0 OP.vreturn

getstatic :: FieldRef -> Code
getstatic fr = mkCodeIdx OP.getstatic $ CFieldRef fr
