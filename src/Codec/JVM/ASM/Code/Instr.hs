module Codec.JVM.ASM.Code.Instr where

import Control.Monad.Trans.RWS
import Data.ByteString (ByteString)

import qualified Data.ByteString as BS

import Codec.JVM.ASM.Code.CtrlFlow (CtrlFlow, Flow)
import Codec.JVM.Attr (StackMapFrame(..), VerifType(..))
import Codec.JVM.Const (Const)
import Codec.JVM.Internal (packI16)
import Codec.JVM.Opcode (Opcode, opcode)
import Codec.JVM.ConstPool (ConstPool)
import Codec.JVM.Types (ReturnType)

import qualified Codec.JVM.ASM.Code.CtrlFlow as CF
import qualified Codec.JVM.ConstPool as CP
import qualified Codec.JVM.Opcode as OP

type InstrRWS a = (RWS ConstPool (ByteString, [StackMapFrame]) CtrlFlow a)

newtype Instr = Instr (InstrRWS ())

instrRWS :: Instr -> InstrRWS ()
instrRWS (Instr irws) = irws

instance Monoid Instr where
  mempty = Instr $ return mempty
  mappend (Instr rws0) (Instr rws1) = Instr $ do
    rws0
    rws1

runInstr :: Instr -> ConstPool -> (ByteString, CtrlFlow, [StackMapFrame])
runInstr instr cp = runInstr' instr cp CF.empty

runInstr' :: Instr -> ConstPool -> CtrlFlow -> (ByteString, CtrlFlow, [StackMapFrame])
runInstr' (Instr instr) cp cf = f $ runRWS instr cp cf where
  f (_, cf', (bs, smfs)) = (bs, cf', smfs)

branch :: Opcode -> ReturnType -> Instr -> Instr -> Instr
branch oc rt br0 br1 = Instr $ do
  op' oc
  writeJump br0 6
  op' OP.goto
  writeStackMapFrame $ \offset -> SameFrame $ offset + 2
  writeJump br1 3
  case rt of
    Nothing -> writeStackMapFrame SameFrame
    Just ft -> writeStackMapFrame $ \offset -> SameLocals offset $ VerifType ft
  where
    writeJump instr padding = do
      cp <- ask
      cf <- get
      let (bs, cf', smfs) = runInstr' instr cp (CF.incOffset 2 cf)
      let relativeOffset = BS.length bs + padding
      writeBytes $ packI16 relativeOffset
      tell (bs, smfs)
      put cf'

bytes :: ByteString -> Instr
bytes = Instr . writeBytes

ix :: Const -> Instr
ix c = Instr $ do
  cp <- ask
  writeBytes . packI16 $ CP.ix $ CP.unsafeIndex c cp

op :: Opcode -> Instr
op = Instr . op'

op' :: Opcode -> InstrRWS ()
op' = writeBytes . BS.singleton . opcode

mapStack :: (Flow -> Flow) -> Instr
mapStack f = Instr $ state s where s cf = (mempty, CF.mapStack f cf)

writeBytes :: ByteString -> InstrRWS ()
writeBytes bs = do
  modify $ CF.incOffset (BS.length bs)
  tell (bs, mempty)

writeStackMapFrame :: (Int -> StackMapFrame) -> InstrRWS ()
writeStackMapFrame f = do
  cf <- get
  tell (mempty, [f $ CF.offset cf])

