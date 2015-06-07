module Codec.JVM.ASM.Code.CtrlFlow where

data CtrlFlow = CtrlFlow
  { cfStack  :: Flow
  , cfLocals :: Flow }
  deriving Show

instance Monoid CtrlFlow where
  mempty = CtrlFlow mempty mempty
  mappend (CtrlFlow s0 l0) (CtrlFlow s1 l1) = CtrlFlow (mappend s0 s1) (mappend l0 l1)

mkCtrlFlow :: Flow -> Flow -> CtrlFlow
mkCtrlFlow s l = CtrlFlow s l

data Flow = Flow
  { flOp  :: Int -> Int
  , flCurrent :: Int
  , flMax :: Int }

instance Show Flow where
  show (Flow _ c m) = concat ["Flow(", show c, "/", show m, ")"]

mkFlow :: (Int -> Int) -> Flow
mkFlow f = Flow f x x where x = f 0

instance Monoid Flow where
  mempty = mkFlow id
  mappend (Flow f0 c0 m0) (Flow f1 _ m1) =
    Flow (f0 . f1) c0' (max (max c0' m0) m1) where c0' = (f1 c0)

flowInc :: Int -> Flow
flowInc i = mkFlow $ (+) i

flowDec :: Int -> Flow
flowDec i = mkFlow $ (-) i
