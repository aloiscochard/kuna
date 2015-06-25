module Codec.JVM.ASM.Code where

import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.Monoid ((<>))
import Data.Word (Word8, Word16)

import qualified Data.ByteString as BS

import Codec.JVM.ASM.Code.CtrlFlow
import Codec.JVM.Attr (Attr(ACode, AStackMapTable), StackMapFrame(..), VerifType(..))
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
toAttrs as cp code = f $ runCode code cp 0 where
  f (xs, frames) = [ACode maxStack' maxLocals' xs attrs] where
      maxLocals' = max as $ flMax $ cfLocals cf
      maxStack' = flMax $ cfStack cf
      cf = ctrlFlow $ code
      attrs = if null frames then [] else [AStackMapTable frames]

data Code = Code
  { consts    :: [Const]
  , ctrlFlow  :: CtrlFlow
  , runCode   :: ConstPool -> Int -> (ByteString, [StackMapFrame]) }

instance Monoid Code where
  mempty = Code [] mempty (const $ const mempty)
  mappend c0 c1 = Code cs cf f where
    cs = (consts c0 ++ consts c1)
    cf = mappend (ctrlFlow c0) (ctrlFlow c1)
    f cp off = xs <> runCode c1 cp (off + (BS.length $ fst xs)) where
      xs = runCode c0 cp off

mkCode :: [Const] -> CtrlFlow -> (ConstPool -> Int -> (ByteString, [StackMapFrame])) -> Code
mkCode cs cf f = Code cs cf f

mkCode' :: [Const] -> CtrlFlow -> (ConstPool -> Int -> ByteString) -> Code
mkCode' cs cf f = mkCode cs cf f' where f' cp i = (f cp i, mempty)

mkCode'' :: [Const] -> Int -> (ConstPool -> Int -> ByteString) -> Code
mkCode'' cs s f = Code cs cf f' where
  f' cp i = (f cp i, mempty)
  cf = mkCtrlFlow (flowInc s) mempty


mkCodeStack :: Opcode -> Flow -> Code
mkCodeStack op' fl = mkCode [] cf $ const $ const ((packOpcode op'), mempty) where
  cf = mkCtrlFlow fl mempty

mkCodeIdx :: Opcode -> Const -> Code
mkCodeIdx op' c = mkCode'' cs 2 f where
  cs = CP.unpack c
  f cp _ =
    (packOpcode op')                                          <>
    (packI16 $ fromIntegral . CP.ix $ CP.unsafeIndex c cp)


codeBytes :: Int -> ByteString -> Code
codeBytes s xs = Code [] cf $ const $ const (xs, mempty) where cf = mkCtrlFlow (flowInc s) mempty

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
invoke op'  mr@(MethodRef _ _ (MethodDesc _ as)) = mkCode' cs (mkCtrlFlow flow mempty) f where
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
  f cp off = (fold [xs, fst ko', zs], frames) where
    okStart = off + koOff + 4
    xs = fold [packOpcode op', packI16 $ fromIntegral okStart]
    ko' = runCode ko cp (off + BS.length xs)
    koOff = BS.length $ fst ko'
    ok' = runCode ok cp (BS.length xs + koOff + 3)
    okOff = BS.length $ fst ok'
    zs = fold [packOpcode OP.goto, packI16 . fromIntegral $ okOff + 3, fst ok']
    frames = snd ko' ++ [SameFrame $ okStart + 2, SameLocals 1 (VPrim JInt)] ++ snd ok'

ireturn :: Code
ireturn = op 0 OP.ireturn

vreturn :: Code
vreturn = op 0 OP.vreturn

getstatic :: FieldRef -> Code
getstatic fr = mkCodeIdx OP.getstatic $ CFieldRef fr
