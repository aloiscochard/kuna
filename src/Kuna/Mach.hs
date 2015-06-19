{-# LANGUAGE OverloadedStrings #-}
module Kuna.Mach where

import Data.Text (Text)

import qualified Data.Map.Strict as Map

import Kuna.Core (Name, machineName)

data Type
  = TyWord8
  | TyWord16
  | TyWord32
  | TyWord64
  | TyChar
  | TyDouble
  | TyFloat
  | TyInt32
  | TyInt64
  | TyData

data Call
  = PlusInt32
  deriving (Bounded, Enum)

calls :: [Call]
calls = enumFrom $ minBound

callName :: Call -> Name
callName = machineName . callId

callId :: Call -> Text
callTypes :: Call -> [Type]

callId    PlusInt32 = "Kuna.Mach.plusInt32"
callTypes PlusInt32 = [TyInt32, TyInt32, TyInt32]

callsById :: Text -> Maybe Call
callsById id' = Map.lookup id' xs where
  xs = Map.fromList $ (\c -> (callId c, c)) <$> calls
