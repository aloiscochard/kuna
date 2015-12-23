module Codec.JVM.ASM.Code.CtrlFlow where

import Codec.JVM.Types (FieldType, fieldSize)

-- TODO Refactor locals support to be indexed by their indentifier (Word8), and max to be unique.

data CtrlFlow = CtrlFlow
  { stack  :: Flow
  , locals :: Flow
  , offset :: Int }
  deriving Show

empty :: CtrlFlow
empty = CtrlFlow mempty mempty 0

maxStack :: CtrlFlow -> Int
maxStack = flowMax . stack

maxLocals :: CtrlFlow -> Int
maxLocals = flowMax . locals

incOffset :: Int -> CtrlFlow -> CtrlFlow
incOffset i cf = cf { offset = offset cf + i }

mapStack :: (Flow -> Flow) -> CtrlFlow -> CtrlFlow
mapStack f cf = cf { stack = f $ stack cf }

mapLocals :: (Flow -> Flow) -> CtrlFlow -> CtrlFlow
mapLocals f cf = cf { locals = f $ locals cf }

data Flow = Flow
  { flowBytes :: [FieldType]
  , flowMax   :: Int }
  deriving Show

instance Monoid Flow where
  mempty = Flow mempty 0
  mappend (Flow fs0 m0) (Flow fs1 m1) = Flow (fs1 ++ fs0) (max m0 m1)

push :: FieldType -> Flow -> Flow
push ft (Flow xs i) = Flow (replicate s ft ++ xs) (max i (length xs + s)) where s = fieldSize ft

pop :: Int -> Flow -> Flow
pop s (Flow xs i) = Flow (drop s xs) i
