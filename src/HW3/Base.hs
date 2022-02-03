{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module HW3.Base
  ( HiAction (..)
  , HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiMonad (..)
  , HiValue (..)
  ) where

import           Codec.Serialise.Class (Serialise)
import           Data.ByteString       (ByteString)
import           Data.Map              (Map)
import           Data.Sequence         (Seq)
import           Data.Text             (Text)
import           Data.Time             (UTCTime)
import           GHC.Generics          (Generic)

-- | Function for Hi
data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Eq, Ord, Show)
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Value for Hi
data HiValue
  = HiValueBool Bool
  | HiValueNull
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Eq, Ord, Show)
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Expression for Hi
data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Eq, Ord, Show)
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Error for Hi
data HiError
  = HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorInvalidArgument
  | HiErrorDivideByZero
  deriving (Eq, Ord, Show)
  deriving stock (Generic)
  deriving anyclass (Serialise)

instance Semigroup HiError where
  (<>) = max

instance Monoid HiError where
  mempty = HiErrorInvalidFunction

-- | Action for Hi
data HiAction
  = HiActionRead FilePath             -- ^ Get a list of directories or read a file
  | HiActionWrite FilePath ByteString -- ^ Write bytestring to a file
  | HiActionMkDir FilePath            -- ^ Make a directory
  | HiActionChDir FilePath            -- ^ Change a directory
  | HiActionCwd                       -- ^ Get a path of current working directory
  | HiActionNow                       -- ^ Get the current time
  | HiActionRand Int Int              -- ^ Get a random number in the interval
  | HiActionEcho Text                 -- ^ Output a string
  deriving (Eq, Ord, Show)
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Monad for executing actions
class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
