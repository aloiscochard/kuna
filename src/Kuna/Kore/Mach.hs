{-# LANGUAGE OverloadedStrings #-}
module Kuna.Kore.Mach where

import Data.Text (Text)

import qualified Data.Map.Strict as Map

import Kuna.Kore.Syn (Name, machineName)

data Type
  = TyBool
--  | TyWord8
--  | TyWord16
--  | TyWord32
--  | TyWord64
--  | TyChar
--  | TyDouble
--  | TyFloat
  | TyInt32
--  | TyInt64
  | TyData

data Call
  = EqInt32
  | PlusInt32
  deriving (Bounded, Eq, Enum, Show)

calls :: [Call]
calls = enumFrom $ minBound

callName :: Call -> Name
callName = machineName . callId

callId :: Call -> Text
callId EqInt32       = "eqInt32"
callId PlusInt32     = "plusInt32"

callTypes :: Call -> [Type]
callTypes EqInt32       = [TyInt32, TyInt32, TyBool]
callTypes PlusInt32     = [TyInt32, TyInt32, TyInt32]


callsById :: Text -> Maybe Call
callsById id' = Map.lookup id' xs where
  xs = Map.fromList $ (\c -> (callId c, c)) <$> calls
