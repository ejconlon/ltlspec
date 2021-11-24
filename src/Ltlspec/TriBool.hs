{-# LANGUAGE DeriveAnyClass #-}

module Ltlspec.TriBool where

import Control.DeepSeq (NFData)
import Data.Foldable (foldl')
import GHC.Generics (Generic)

data TriBool =
    TriBoolTrue
  | TriBoolFalse
  | TriBoolUnknown
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

toTriBool :: Maybe Bool -> TriBool
toTriBool = \case
  Nothing -> TriBoolUnknown
  Just b -> if b then TriBoolTrue else TriBoolFalse

fromTriBool :: TriBool -> Maybe Bool
fromTriBool = \case
  TriBoolTrue -> Just True
  TriBoolFalse -> Just False
  TriBoolUnknown -> Nothing

triBoolNot :: TriBool -> TriBool
triBoolNot = \case
  TriBoolTrue -> TriBoolFalse
  TriBoolFalse -> TriBoolTrue
  TriBoolUnknown -> TriBoolUnknown

triBoolAnd :: TriBool -> TriBool -> TriBool
triBoolAnd x y =
  case x of
    TriBoolTrue -> y
    TriBoolFalse -> TriBoolFalse
    TriBoolUnknown ->
      case y of
        TriBoolTrue -> TriBoolUnknown
        TriBoolFalse -> TriBoolFalse
        TriBoolUnknown -> TriBoolUnknown

triBoolOr :: TriBool -> TriBool -> TriBool
triBoolOr x y =
  case x of
    TriBoolTrue -> TriBoolTrue
    TriBoolFalse -> y
    TriBoolUnknown -> case y of
      TriBoolTrue -> TriBoolTrue
      TriBoolFalse -> TriBoolUnknown
      TriBoolUnknown -> TriBoolUnknown

triBoolAndAll :: Foldable f => f TriBool -> TriBool
triBoolAndAll = foldl' triBoolAnd TriBoolTrue

triBoolOrAll :: Foldable f => f TriBool -> TriBool
triBoolOrAll = foldl' triBoolOr TriBoolFalse

triBoolUntil :: TriBool -> TriBool -> TriBool
triBoolUntil _ r2 = r2

triBoolRelease :: TriBool -> TriBool -> TriBool
triBoolRelease r1 r2 =
  case r2 of
    TriBoolTrue -> TriBoolTrue
    TriBoolFalse -> r1
    TriBoolUnknown ->
      case r1 of
        TriBoolTrue -> TriBoolTrue
        TriBoolFalse -> TriBoolUnknown
        TriBoolUnknown -> TriBoolUnknown
