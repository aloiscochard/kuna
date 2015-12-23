module Codec.JVM.ASM.Code.CtrlFlow where

import Codec.JVM.Types (FieldType, fieldSize)
import Data.IntMap as IMap
import Data.Word (Word8)

data CtrlFlow = CtrlFlow
  { stack  :: Stack
  , locals :: IntMap FieldType
  , offset :: Int }
  deriving Show

empty :: CtrlFlow
empty = CtrlFlow mempty mempty 0

mapStack :: (Stack -> Stack) -> CtrlFlow -> CtrlFlow
mapStack f cf = cf { stack = f $ stack cf }

maxStack :: CtrlFlow -> Int
maxStack = stackMax . stack

maxLocals :: CtrlFlow -> Int
maxLocals = IMap.size . locals

incOffset :: Int -> CtrlFlow -> CtrlFlow
incOffset i cf = cf { offset = offset cf + i }

load :: Word8 -> FieldType -> CtrlFlow -> CtrlFlow
load n ft cf = cf { locals = IMap.insert (fromIntegral n) ft $ locals cf, stack = push ft $ stack cf }

store :: Word8 -> FieldType -> CtrlFlow -> CtrlFlow
store n ft cf = cf { locals = IMap.insert (fromIntegral n) ft $ locals cf, stack = pop ft $ stack cf }

data Stack = Stack
  { stackVal    :: [FieldType]
  , stackMax    :: Int }
  deriving Show

instance Monoid Stack where
  mempty = Stack [] 0
  mappend (Stack vs0 m0) (Stack vs1 m1) = Stack (vs1 ++ vs0) (max m0 m1)

push :: FieldType -> Stack -> Stack
push ft (Stack xs i) = Stack ys (max i $ sum $ fieldSize <$> ys) where ys = ft:xs

pop :: FieldType -> Stack -> Stack
pop ft = pop' $ fieldSize ft

pop' :: Int -> Stack -> Stack
pop' s (Stack xs i) = Stack (drop s xs) i
