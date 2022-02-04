{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeApplications       #-}

module HW3.Evaluator
  ( eval
  , ToValue
  , toValue
  ) where

import           Codec.Compression.Zlib          (bestCompression,
                                                  compressLevel, compressWith,
                                                  decompress,
                                                  defaultCompressParams)
import           Codec.Serialise                 (deserialise, serialise)
import           Control.Applicative             (liftA2)
import           Control.Applicative.Combinators ((<|>))
import           Control.Monad                   (join)
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Except      (ExceptT, except, runExceptT)
import           Data.ByteString                 (ByteString, unpack)
import qualified Data.ByteString                 as B
import           Data.ByteString.Internal        (packBytes, packChars)
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import           Data.Data                       (Typeable, cast)
import           Data.Foldable                   (foldlM, toList)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromMaybe)
import           Data.Semigroup                  (stimes)
import           Data.Sequence                   (Seq (..), fromList, (><))
import qualified Data.Sequence                   as S
import           Data.Text                       (Text, singleton, strip,
                                                  toLower, toUpper)
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8', encodeUtf8)
import           Data.Time                       (UTCTime, addUTCTime,
                                                  diffUTCTime)
import           Data.Word                       (Word8)
import           GHC.Real                        (Ratio (..))
import           HW3.Base                        (HiAction (..), HiError (..),
                                                  HiExpr (..), HiFun (..),
                                                  HiMonad, HiValue (..),
                                                  runAction)
import           Prelude                         hiding (length)
import           Text.Read                       (readMaybe)

-- | Evaluates the value of the expression (Right) or returns an error (Left)
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . eval'

-- |  Evaluates the value of the expression in ExceptT
eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eval' (HiExprDict dict) = evalDict dict
eval' (HiExprValue value) = returnVal value
eval' (HiExprApply a b) = evalApply a b
eval' (HiExprRun action) = do
  value <- eval' action
  toType value >>= evalAction

-- | Return an error of evaluating
throw :: HiMonad m => HiError -> ExceptT HiError m a
throw = except . Left

-- | Return value
returnVal :: HiMonad m => a -> ExceptT HiError m a
returnVal = except . Right

-- evaluate an expression

-- | Apply an expression to arguments
evalApply :: HiMonad m => HiExpr -> [HiExpr] -> ExceptT HiError m HiValue
evalApply (HiExprValue value) = applyValue value
evalApply (HiExprDict dict) = \x -> do
  evaluatedDict <- evalDict dict
  applyValue evaluatedDict x
evalApply (HiExprApply expr exprArgs) = \args -> do
  exprValue <- HiExprValue <$> evalApplyArgument expr exprArgs
  evalApply exprValue args
evalApply _ = \_ -> throw HiErrorInvalidFunction

-- | Apply the expression, but evaluate all the arguments in advance
evalApplyStrict :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalApplyStrict value args = do
  evaluated <- mapM eval' args
  evalStrictFun value evaluated

-- | Evaluate the expression that is applied
evalApplyArgument :: HiMonad m => HiExpr -> [HiExpr] -> ExceptT HiError m HiValue
evalApplyArgument expr args = do
  exprValue <- HiExprValue <$> eval' expr
  evalApply exprValue args

-- evaluate a function

-- | Evaluate a function
evalFun :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalFun HiFunAnd = evalBinaryByFun (evalBinaryLazyJustReturn isFalse)
evalFun HiFunOr  = evalBinaryByFun (evalBinaryLazyJustReturn (not . isFalse))
evalFun HiFunIf  = evalIf
evalFun fun      = evalApplyStrict fun

-- | Evaluate a function whose arguments are evaluated
evalStrictFun :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
evalStrictFun HiFunAdd =
  evalBiFun @Rational (+)
    <||> evalBiFun @Text (<>)
    <||> evalBiFun @(Seq HiValue) (><)
    <||> evalBiFun @ByteString (<>)
    <||> evalBiFun (\time diff -> addUTCTime (fromRational diff) time)
evalStrictFun HiFunSub =
  evalBiFun @Rational (-)
    <||> evalBiFun (\x y -> toRational $ diffUTCTime x y)
evalStrictFun HiFunMul =
  evalBiFun @Rational (*)
    <||> evalBinaryMonadFun @Text checkedStimes
    <||> evalBinaryMonadFun @(Seq HiValue) checkedStimes
    <||> evalBinaryMonadFun @ByteString checkedStimes
evalStrictFun HiFunDiv =
  evalBinaryMonadFun @Rational (\x y -> if y == 0 then throw HiErrorDivideByZero else returnVal $ x / y)
    <||> evalBiFun @Text (\str1 str2 -> str1 <> "/" <> str2)
evalStrictFun HiFunNot = evalUnaryFun not
evalStrictFun HiFunLessThan = evalBiFun @HiValue (<)
evalStrictFun HiFunGreaterThan = evalBiFun @HiValue (>)
evalStrictFun HiFunEquals = evalBiFun @HiValue (==)
evalStrictFun HiFunNotLessThan = evalBiFun @HiValue (>=)
evalStrictFun HiFunNotGreaterThan = evalBiFun @HiValue (<=)
evalStrictFun HiFunNotEquals = evalBiFun @HiValue (/=)
evalStrictFun HiFunLength =
  evalUnaryFun T.length
    <||> evalUnaryFun @(Seq HiValue) S.length
    <||> evalUnaryFun B.length
evalStrictFun HiFunToUpper = evalUnaryFun toUpper
evalStrictFun HiFunToLower = evalUnaryFun toLower
evalStrictFun HiFunReverse =
  evalUnaryFun T.reverse
    <||> evalUnaryFun @(Seq HiValue) S.reverse
    <||> evalUnaryFun B.reverse
evalStrictFun HiFunTrim = evalUnaryFun strip
evalStrictFun HiFunList = evalList
evalStrictFun HiFunRange = evalBiFun @Rational enumFromTo
evalStrictFun HiFunFold = evalBinaryMonadFun foldlHi
evalStrictFun HiFunPackBytes = evalUnaryMonadFun evalPack
evalStrictFun HiFunUnpackBytes = evalUnaryFun evalUnpack
evalStrictFun HiFunEncodeUtf8 = evalUnaryFun encodeUtf8
evalStrictFun HiFunDecodeUtf8 = evalUnaryMonadFun evalDecode
evalStrictFun HiFunZip = evalUnaryFun evalZip
evalStrictFun HiFunUnzip = evalUnaryFun evalUnzip
evalStrictFun HiFunSerialise = evalUnaryFun evalSerialise
evalStrictFun HiFunDeserialise = evalUnaryFun evalDeserialise
evalStrictFun HiFunRead = evalUnaryFunAction HiActionRead
evalStrictFun HiFunMkDir = evalUnaryFunAction HiActionMkDir
evalStrictFun HiFunChDir = evalUnaryFunAction HiActionChDir
evalStrictFun HiFunWrite =
  evalBinaryFunAction (\path str -> HiActionWrite path (packChars str))
    <||> evalBinaryFunAction HiActionWrite
evalStrictFun HiFunParseTime = evalUnaryFun timeToValue
evalStrictFun HiFunRand = evalBinaryFunAction HiActionRand
evalStrictFun HiFunEcho = evalUnaryFunAction HiActionEcho
evalStrictFun HiFunKeys = evalUnaryFun @(Map.Map HiValue HiValue) Map.keys
evalStrictFun HiFunValues = evalUnaryFun @(Map.Map HiValue HiValue) Map.elems
evalStrictFun HiFunCount =
  evalUnaryFun @Text count
    <||> evalUnaryFun @(Seq HiValue) count
    <||> evalUnaryFun @ByteString count
evalStrictFun HiFunInvert = evalUnaryFun invert
evalStrictFun HiFunAnd = evalBiFun (\x y -> if isFalse x then x else y)
evalStrictFun HiFunOr = evalBiFun (\x y -> if isFalse x then y else x)
evalStrictFun HiFunIf = evalStrictIf

-- apply

-- | Apply value to arguments
applyValue :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
applyValue (HiValueFunction fun) = evalFun fun
applyValue (HiValueString str)   = applySliceable str
applyValue (HiValueList list)    = applySliceable list
applyValue (HiValueBytes bytes)  = applySliceable bytes
applyValue (HiValueDict dict)    = getFromDict dict
applyValue _                     = \_ -> throw HiErrorInvalidFunction

-- | Apply sliceable value to arguments
applySliceable :: (Sliceable t a, HiMonad m) => t -> [HiExpr] -> ExceptT HiError m HiValue
applySliceable str [n] = do
  indexValue <- fromExpr n
  case checkedIndex indexValue str of
    Nothing -> returnVal HiValueNull
    Just ch -> returnVal $ toValue ch
applySliceable str [from, to] = do
  fromIndex <- toCorrectIndex str <$> fromExpr from <|> 0 <$ fromExpr @() from
  toIndex <- toCorrectIndex str <$> fromExpr to <|> length str <$ fromExpr @() to
  returnVal $ toValue $ correctSlice fromIndex toIndex str
applySliceable _ _ = throw HiErrorArityMismatch

-- evaluate an action

-- | Evaluate an action
evalAction :: HiMonad m => HiAction -> ExceptT HiError m HiValue
evalAction = lift . runAction

-- | Evaluate an unary action
evalUnaryFunAction :: (ToType a, HiMonad m) => (a -> HiAction) -> [HiValue] -> ExceptT HiError m HiValue
evalUnaryFunAction f [x] = do
  value <- toType x
  returnVal $ HiValueAction $ f value
evalUnaryFunAction _ _ = throw HiErrorArityMismatch

-- | Evaluate a binary action
evalBinaryFunAction :: (ToType a, ToType b, HiMonad m) => (a -> b -> HiAction) -> [HiValue] -> ExceptT HiError m HiValue
evalBinaryFunAction f [x, y] = do
  xValue <- toType x
  yValue <- toType y
  returnVal $ HiValueAction $ f xValue yValue
evalBinaryFunAction _ _ = throw HiErrorArityMismatch

-- Sliceable instances

-- | A class for which taking by index and slice is available
class (ToValue t, ToValue a) => Sliceable t a | t -> a where
  fold' :: (b -> a -> b) -> b -> t -> b
  empty :: t
  length :: t -> Int
  byIndex :: t -> Int -> a
  slice :: Int -> Int -> t -> t

instance Sliceable Text Char where
  fold' = T.foldl'
  empty = T.empty
  length = T.length
  byIndex = T.index
  slice from to xs = T.take (to - from) (T.drop from xs)

instance Sliceable (Seq HiValue) HiValue where
  fold' = foldl
  empty = S.empty
  length = S.length
  byIndex = S.index
  slice from to xs = S.take (to - from) (S.drop from xs)

instance Sliceable ByteString Word8 where
  fold' = B.foldl'
  empty = B.empty
  length = B.length
  byIndex = B.index
  slice from to xs = B.take (to - from) (B.drop from xs)

-- ToValue instances

-- | Converting a value to HiValue
class ToValue a where
  toValue :: a -> HiValue

instance ToValue Rational where
  toValue = HiValueNumber

instance ToValue Int where
  toValue = toValue . toRational

instance ToValue () where
  toValue = const HiValueNull

instance ToValue Bool where
  toValue = HiValueBool

instance ToValue Text where
  toValue = HiValueString

instance ToValue (Seq HiValue) where
  toValue = HiValueList

instance ToValue a => ToValue [a] where
  toValue arr = HiValueList $ fromList (map toValue arr)

instance ToValue Char where
  toValue = HiValueString . singleton

instance ToValue Word8 where
  toValue = HiValueNumber . toRational

instance ToValue HiValue where
  toValue = id

instance ToValue ByteString where
  toValue = HiValueBytes

instance ToValue UTCTime where
  toValue = HiValueTime

instance (ToValue a, ToValue b) => ToValue (Map.Map a b) where
  toValue = HiValueDict . Map.mapKeys toValue . Map.map toValue

-- ToType instances

-- | Taking the value from HiValue
class ToType a where
  toType :: HiMonad m => HiValue -> ExceptT HiError m a

instance ToType Rational where
  toType = valueToType

instance ToType Int where
  toType expr = do
    value <- toType expr
    case value of
      (x :% y) -> case quotRem x y of
        (n, 0) ->
          if toInteger (minBound :: Int) <= n && n <= toInteger (maxBound :: Int)
            then returnVal $ fromEnum n
            else throw HiErrorInvalidArgument
        _ -> throw HiErrorInvalidArgument

instance ToType Bool where
  toType = valueToType

instance ToType Text where
  toType = valueToType

instance ToType HiFun where
  toType = valueToType

instance ToType () where
  toType = valueToType

instance ToType a => ToType (Seq a) where
  toType (HiValueList list) = mapM toType list
  toType _ = throw HiErrorInvalidArgument

instance ToType HiValue where
  toType = return

instance ToType ByteString where
  toType = valueToType

instance ToType HiAction where
  toType = valueToType

instance ToType String where
  toType str = T.unpack <$> valueToType str

instance ToType UTCTime where
  toType = valueToType

instance ToType (Map.Map HiValue HiValue) where
  toType = valueToType

-- eval function helpers

-- | Alternative for evaluating functions
(<||>) ::
  (HiMonad m) =>
  ([HiValue] -> ExceptT HiError m HiValue) ->
  ([HiValue] -> ExceptT HiError m HiValue) ->
  ([HiValue] -> ExceptT HiError m HiValue)
(<||>) a b args = a args <|> b args

-- | Evaluate a binary function
evalBiFun ::
  (ToType a, ToType b, ToValue c, HiMonad m) =>
  (a -> b -> c) ->
  [HiValue] ->
  ExceptT HiError m HiValue
evalBiFun f = evalBinaryMonadFun (\x y -> returnVal $ f x y)

-- | Evaluate a binary monad function
evalBinaryMonadFun ::
  (ToType a, ToType b, ToValue c, HiMonad m) =>
  (a -> b -> ExceptT HiError m c) ->
  [HiValue] ->
  ExceptT HiError m HiValue
evalBinaryMonadFun op [a, b] = do
  value <- join $ liftA2 op (toType a) (toType b)
  returnVal $ toValue value
evalBinaryMonadFun _ _ = throw HiErrorArityMismatch

-- | Evaluate an unary function
evalUnaryFun ::
  (ToType a, ToValue b, HiMonad m) =>
  (a -> b) ->
  [HiValue] ->
  ExceptT HiError m HiValue
evalUnaryFun op = evalUnaryMonadFun (returnVal . op)

-- | Evaluate an unary monad function
evalUnaryMonadFun ::
  (ToType a, ToValue b, HiMonad m) =>
  (a -> ExceptT HiError m b) ->
  [HiValue] ->
  ExceptT HiError m HiValue
evalUnaryMonadFun op [expr] = do
  value <- toType expr >>= op
  returnVal $ toValue value
evalUnaryMonadFun _ _ = throw HiErrorArityMismatch

-- | Evaluate the binary function that gets the list
evalBinaryByFun ::
  HiMonad m =>
  (HiExpr -> HiExpr -> ExceptT HiError m HiValue) ->
  [HiExpr] ->
  ExceptT HiError m HiValue
evalBinaryByFun op [x, y] = op x y
evalBinaryByFun _ _       = throw HiErrorArityMismatch

-- | Lazy evaluate a binary function
evalBinaryLazy ::
  HiMonad m =>
  (HiValue -> Bool) ->
  (HiValue -> HiExpr -> ExceptT HiError m HiValue) ->
  (HiValue -> HiValue -> ExceptT HiError m HiValue) ->
  HiExpr ->
  HiExpr ->
  ExceptT HiError m HiValue
evalBinaryLazy cond trueReturn falseReturn x y = do
  xValue <- fromExpr x
  if cond xValue
    then trueReturn xValue y
    else fromExpr y >>= falseReturn xValue

-- | Lazy binary function that return arguments
evalBinaryLazyJustReturn ::
  HiMonad m =>
  (HiValue -> Bool) ->
  HiExpr ->
  HiExpr ->
  ExceptT HiError m HiValue
evalBinaryLazyJustReturn cond = evalBinaryLazy cond (const . returnVal) (const returnVal)

-- | Evaluate a binary function to function for a list
evalBinaryFunToList :: HiMonad m => HiFun -> HiValue -> HiValue -> ExceptT HiError m HiValue
evalBinaryFunToList f a b = evalStrictFun f [a, b]

-- functions for evaluate

-- | Get value form dict (or null if the value does not exist)
getFromDict :: HiMonad m => Map.Map HiValue HiValue -> [HiExpr] -> ExceptT HiError m HiValue
getFromDict dict [x] = do
  value <- eval' x
  returnVal $ fromMaybe HiValueNull $ Map.lookup value dict
getFromDict _ _ = throw HiErrorArityMismatch

-- | Evaluate a lazy if
evalIf :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalIf [cond, a, b] = do
  condValue <- fromExpr cond
  value <- eval' $ if condValue then a else b
  returnVal value
evalIf _ = throw HiErrorArityMismatch

-- | Evaluate a strict if (the arguments have already been evaluated)
evalStrictIf :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
evalStrictIf [cond, x, y] = do
  evaluatedCond <- toType cond
  returnVal $ if evaluatedCond then x else y
evalStrictIf _ = throw HiErrorArityMismatch

-- | Convert list to HiValueList
evalList :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
evalList = returnVal . HiValueList . fromList

-- | Foldl for function and sliceable value (with the condition foldl(_, [x]) = x)
foldlHi :: HiMonad m => HiValue -> Seq HiValue -> ExceptT HiError m HiValue
foldlHi _ (h :<| S.Empty)               = returnVal h
foldlHi (HiValueFunction fun) (h :<| t) = foldlM (evalBinaryFunToList fun) h t
foldlHi (HiValueFunction _) _           = returnVal HiValueNull
foldlHi (HiValueString str) args        = applySliceable str (mapToExpr args)
foldlHi (HiValueList list) args         = applySliceable list (mapToExpr args)
foldlHi (HiValueBytes bytes) args       = applySliceable bytes (mapToExpr args)
foldlHi _ _                             = throw HiErrorInvalidArgument

-- | Count value in sliceable value to Map
count :: (Ord a, Sliceable t a) => t -> Map.Map a Int
count = fold' (\m a -> Map.insertWith (+) a 1 m) Map.empty

-- | Invert Map
invert :: Map.Map HiValue HiValue -> Map.Map HiValue [HiValue]
invert m = Map.fromListWith (++) [(v, [k]) | (k, v) <- Map.toList m]

-- | Convert time to value (or null in case of failure)
timeToValue :: Text -> HiValue
timeToValue str = maybe HiValueNull HiValueTime (readMaybe $ T.unpack str)

-- | Serialise a HiValue
evalSerialise :: HiValue -> ByteString
evalSerialise = toStrict . serialise

-- | Desealize a ByteString to a HiValue
evalDeserialise :: ByteString -> HiValue
evalDeserialise = deserialise . fromStrict

-- | Convert Int to Word8 (or return error in case of failure)
toByte :: HiMonad m => Int -> ExceptT HiError m Word8
toByte n =
  if n >= 0 && n <= 255
    then returnVal $ fromIntegral n
    else throw HiErrorInvalidArgument

-- | Unpack a ByteString to a list of int
evalUnpack :: ByteString -> [Int]
evalUnpack str = map fromEnum (unpack str)

-- | Zip ByteString
evalZip :: ByteString -> ByteString
evalZip str = toStrict $ compressWith defaultCompressParams { compressLevel = bestCompression } (fromStrict str)

-- | Unzip Bytestring
evalUnzip :: ByteString -> ByteString
evalUnzip str = toStrict $ decompress (fromStrict str)

-- | Decode ByteString to Text (or null in case of failure)
evalDecode :: HiMonad m => ByteString -> ExceptT HiError m HiValue
evalDecode str = case decodeUtf8' str of
  Left _        -> returnVal HiValueNull
  Right decoded -> returnVal $ HiValueString decoded

-- | Pack a list of int to ByteString
evalPack :: HiMonad m => Seq Int -> ExceptT HiError m ByteString
evalPack arr = packBytes . toList <$> mapM toByte arr

-- | Evaluate a pair of expressions to a pair of values
evalPair :: HiMonad m => (HiExpr, HiExpr) -> ExceptT HiError m (HiValue, HiValue)
evalPair (first, second) = do
  firstValue <- eval' first
  secondValue <- eval' second
  returnVal (firstValue, secondValue)

-- | Evaluate a list of pairs to Map
evalDict :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m HiValue
evalDict dict = HiValueDict . Map.fromList <$> mapM evalPair dict

-- | Checking that the value is false or null
isFalse :: HiValue -> Bool
isFalse bool = bool == HiValueBool False || bool == HiValueNull

-- Sliceable helpers

-- | Slice for sliceable that returns a empty value if from >= to
correctSlice :: Sliceable t a => Int -> Int -> t -> t
correctSlice from to xs = if from >= to then empty else slice from to xs

-- | Convert index to interval 0..length
toCorrectIndex :: Sliceable t a => t -> Int -> Int
toCorrectIndex str n = max 0 (min negateIndex (length str))
  where
    negateIndex = if n < 0 then n + length str else n

-- |  Checking that the index is correct (in interval 0..length)
correctIndex :: Sliceable t a => Int -> t -> Bool
correctIndex n str = n >= 0 && n < length str

-- | Taking by index (or Nothing is index is not correct)
checkedIndex :: Sliceable t a => Int -> t -> Maybe a
checkedIndex n str = if correctIndex n str then Just $ byIndex str n else Nothing

-- | Map a sequnce of values to a list of expressions
mapToExpr :: Seq HiValue -> [HiExpr]
mapToExpr = toList . fmap HiExprValue

-- | Return stimes (or error if multiplier is not positive)
checkedStimes :: (Semigroup a, HiMonad m) => a -> Int -> ExceptT HiError m a
checkedStimes x n = if n > 0 then returnVal $ stimes n x else throw HiErrorInvalidArgument

-- evaluate helpers

-- | Throw error for invalid type
invalidType :: HiMonad m => ExceptT HiError m a
invalidType = throw HiErrorInvalidArgument

-- | Trying to get a value from a
castToValue :: (Typeable a, Typeable b, HiMonad m) => a -> ExceptT HiError m b
castToValue value = maybe invalidType return (cast value)

-- | Trying to get a value from HiValue
valueToType :: (Typeable a, HiMonad m) => HiValue -> ExceptT HiError m a
valueToType (HiValueNumber num)    = castToValue num
valueToType (HiValueBool bool)     = castToValue bool
valueToType (HiValueFunction fun)  = castToValue fun
valueToType (HiValueString str)    = castToValue str
valueToType HiValueNull            = castToValue ()
valueToType (HiValueList list)     = castToValue list
valueToType (HiValueBytes bytes)   = castToValue bytes
valueToType (HiValueAction action) = castToValue action
valueToType (HiValueTime time)     = castToValue time
valueToType (HiValueDict dict)     = castToValue dict

-- | Trying to get a value from HiExpression
fromExpr :: (ToType a, HiMonad m) => HiExpr -> ExceptT HiError m a
fromExpr expr = do
  value <- eval' expr
  toType value
