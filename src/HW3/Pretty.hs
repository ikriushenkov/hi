{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HW3.Pretty
  ( prettyValue
  )
where

import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B
import           Data.Foldable                 (toList)
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Scientific               (floatingOrInteger,
                                                fromRationalRepetendUnlimited)
import           Data.Sequence                 (Seq)
import qualified Data.Sequence                 as S
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time.Clock               (UTCTime)
import           Data.Word                     (Word8)
import           GHC.Real                      (Ratio (..))
import           HW3.Base
import           Numeric                       (showFFloat, showHex)
import           Prettyprinter                 (Doc, pretty, unsafeViaShow)
import           Prettyprinter.Render.Terminal (AnsiStyle)

-- | Converting a HiValue to a pretty show
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue HiValueNull            = pretty @Text "null"
prettyValue (HiValueNumber value)  = showValue value
prettyValue (HiValueFunction fun)  = showValue fun
prettyValue (HiValueBool bool)     = showValue bool
prettyValue (HiValueString str)    = showValue str
prettyValue (HiValueList s)        = showValue s
prettyValue (HiValueBytes str)     = showValue str
prettyValue (HiValueAction action) = showValue action
prettyValue (HiValueTime time)     = showValue time
prettyValue (HiValueDict dict)     = showValue dict

-- pretty actions

-- | Pretty show for an unary action
prettyUnaryAction :: Prettiable a => HiAction -> a -> Doc AnsiStyle
prettyUnaryAction action x = pretty (actionName action) <> pretty @Text "(" <> showValue x <> pretty @Text ")"

-- | Pretty show for a binary action
prettyBinaryAction :: (Prettiable a, Prettiable b) => HiAction -> a -> b -> Doc AnsiStyle
prettyBinaryAction action x y = pretty (actionName action) <> pretty @Text "(" <> showValue x <> pretty @Text ", " <> showValue y <> pretty @Text ")"

-- | Pretty show for an action
prettyAction :: HiAction -> Doc AnsiStyle
prettyAction HiActionCwd = pretty $ actionName HiActionCwd
prettyAction HiActionNow = pretty $ actionName HiActionNow
prettyAction action@(HiActionRead path) = prettyUnaryAction action path
prettyAction action@(HiActionMkDir path) = prettyUnaryAction action path
prettyAction action@(HiActionChDir path) = prettyUnaryAction action path
prettyAction action@(HiActionWrite path str) = prettyBinaryAction action path str
prettyAction action@(HiActionRand from to) = prettyBinaryAction action from to
prettyAction action@(HiActionEcho str) = prettyUnaryAction action str

-- mapping names

-- | Mapping function to string
funName :: HiFun -> Text
funName HiFunAdd            = "add"
funName HiFunSub            = "sub"
funName HiFunMul            = "mul"
funName HiFunDiv            = "div"
funName HiFunNot            = "not"
funName HiFunAnd            = "and"
funName HiFunOr             = "or"
funName HiFunLessThan       = "less-than"
funName HiFunGreaterThan    = "greater-than"
funName HiFunEquals         = "equals"
funName HiFunNotLessThan    = "not-less-than"
funName HiFunNotGreaterThan = "not-greater-than"
funName HiFunNotEquals      = "not-equals"
funName HiFunIf             = "if"
funName HiFunLength         = "length"
funName HiFunToUpper        = "to-upper"
funName HiFunToLower        = "to-lower"
funName HiFunReverse        = "reverse"
funName HiFunTrim           = "trim"
funName HiFunList           = "list"
funName HiFunRange          = "range"
funName HiFunFold           = "fold"
funName HiFunPackBytes      = "pack-bytes"
funName HiFunUnpackBytes    = "unpack-bytes"
funName HiFunEncodeUtf8     = "encode-utf8"
funName HiFunDecodeUtf8     = "decode-utf8"
funName HiFunZip            = "zip"
funName HiFunUnzip          = "unzip"
funName HiFunSerialise      = "serialise"
funName HiFunDeserialise    = "deserialise"
funName HiFunRead           = "read"
funName HiFunWrite          = "write"
funName HiFunMkDir          = "mkdir"
funName HiFunChDir          = "cd"
funName HiFunParseTime      = "parse-time"
funName HiFunRand           = "rand"
funName HiFunEcho           = "echo"
funName HiFunCount          = "count"
funName HiFunKeys           = "keys"
funName HiFunValues         = "values"
funName HiFunInvert         = "invert"

-- | Mapping action to string
actionName :: HiAction -> Text
actionName HiActionCwd         = "cwd"
actionName HiActionNow         = "now"
actionName (HiActionRead _)    = "read"
actionName (HiActionMkDir _)   = "mkdir"
actionName (HiActionChDir _)   = "cd"
actionName (HiActionEcho _)    = "echo"
actionName (HiActionWrite _ _) = "write"
actionName (HiActionRand _ _)  = "rand"

-- Prettiable instances

-- | Pretty show value
class Prettiable a where
  showValue :: a -> Doc AnsiStyle

instance Prettiable Rational where
  showValue value@(x :% y) = case fromRationalRepetendUnlimited value of
    (n, Nothing) -> case (floatingOrInteger n :: Either Double Integer) of
      Left double -> pretty $ showFFloat Nothing double ""
      Right int   -> pretty int
    _ -> case quotRem x y of
      (0, r) -> pretty $ fraction r y
      (q, r) -> pretty $ show q ++ " " ++ sign ++ " " ++ fraction (abs r) y
        where
          sign = if r >= 0 then "+" else "-"
      where
        fraction a b = show a ++ "/" ++ show b

instance Prettiable Int where
  showValue = pretty . show

instance Prettiable HiFun where
  showValue = pretty . funName

instance Prettiable Bool where
  showValue bool = if bool then "true" else "false"

instance Prettiable Text where
  showValue = unsafeViaShow

instance Prettiable FilePath where
  showValue = unsafeViaShow

instance Prettiable (Seq HiValue) where
  showValue S.Empty = pretty @Text "[ ]"
  showValue s = pretty $ "[ " <> T.intercalate ", " (map (T.pack . show . prettyValue) (toList s)) <> " ]"

instance Prettiable ByteString where
  showValue str =
    pretty $
      if B.null str
        then "[# #]"
        else "[# " <> T.unwords (map (T.pack . show . showValue) $ B.unpack str) <> " #]"

instance Prettiable Word8 where
  showValue n =
    pretty $
      if length hex == 1 then "0" ++ hex else hex
    where
      hex = showHex n ""

instance Prettiable HiAction where
  showValue = prettyAction

instance Prettiable UTCTime where
  showValue time = pretty @Text"parse-time(\"" <> pretty (show time) <> pretty @Text "\")"

instance Prettiable (HiValue, HiValue) where
  showValue (first, second) = prettyValue first <> pretty @Text ": " <> prettyValue second

instance Prettiable (Map HiValue HiValue) where
  showValue dict =
    pretty $
      if M.null dict
        then "{ }"
        else "{ " <> T.intercalate ", " (map (T.pack . show . showValue) $ M.toList dict) <> " }"
