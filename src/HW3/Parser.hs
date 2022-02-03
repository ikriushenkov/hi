module HW3.Parser
  ( parse
  ) where

import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.ByteString.Internal       (packBytes)
import           Data.Char                      (digitToInt, isAlpha,
                                                 isAlphaNum, isHexDigit)
import           Data.List                      (intercalate)
import           Data.Text                      (pack)
import           Data.Void                      (Void)
import           Data.Word                      (Word8)
import           HW3.Base                       (HiAction (..), HiExpr (..),
                                                 HiFun (..), HiValue (..))
import           Text.Megaparsec                (ParseErrorBundle, Parsec,
                                                 between, choice, eof, many,
                                                 manyTill, notFollowedBy,
                                                 optional, runParser, satisfy,
                                                 sepBy, sepBy1, sepEndBy, try,
                                                 (<|>))
import           Text.Megaparsec.Char           (char, space, space1, string)
import           Text.Megaparsec.Char.Lexer     (scientific, signed)
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = Parsec Void String

-- | Parse a string to HiExpr (Right) or return error (Left) in case of failure
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser parse' ""

-- parse expression

-- | Parse a line of expression
parse' :: Parser HiExpr
parse' = do
  space
  expr <- parseWithOperators
  space
  eof
  return expr

-- | Parse a number to HiExpr
parseNumber :: Parser HiExpr
parseNumber = HiExprValue . HiValueNumber . toRational <$> signed space scientific

-- | Parse a function name to HiExpr
parseNameFun :: Parser HiExpr
parseNameFun = do
  name <- identifier
  let exprMaybe = fmap HiValueFunction (mapNameFun name) <|> fmap HiValueAction (mapNameAction name)
  case exprMaybe of
    Nothing   -> fail "Expected a name of function"
    Just expr -> return $ HiExprValue expr

-- | Parse a null to HiExpr
parseNull :: Parser HiExpr
parseNull = HiExprValue HiValueNull <$ string "null"

-- | Parse a string to HiExpr
parseString :: Parser HiExpr
parseString = stringToExpr <$> stringLiteral

-- | Parse a bool to HiExpr
parseBool :: Parser HiExpr
parseBool = HiExprValue . HiValueBool <$> bool

-- | Parse a list to HiExpr
parseList :: Parser HiExpr
parseList = do
  args <- squareParens $ lexeme parseArgumentsWithoutParens
  return $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) args

-- | Parse a Map to HiExpr
parseMap :: Parser HiExpr
parseMap = curlyParens $ do
  pairs <- sepBy parsePair (lexeme $ char ',')
  return $ HiExprDict pairs

-- | Parse a list of bytes to HiExpr
parseBytes :: Parser HiExpr
parseBytes = HiExprValue . HiValueBytes . packBytes <$> bytes

-- | Parse a term to HiExpr
parseTerm :: Parser HiExpr
parseTerm =
  lexeme $
    choice
      [ parseBytes
      , parseList
      , parseNull
      , parseString
      , parseBool
      , parseNumber
      , parseMap
      , parseNameFun
      ]

-- | Parse expression to HiExpr
parseExpr :: Parser HiExpr
parseExpr = lexeme $ do
  expr <- parens parseWithOperators <|> parseTerm
  foldExpr expr

-- | Parse expression with operators to HiExpr
parseWithOperators :: Parser HiExpr
parseWithOperators = makeExprParser parseExpr operators

-- parse expression after expression

-- | Parse a dot with identifier
parseDot :: HiExpr -> Parser HiExpr
parseDot expr = do
  value <- char '.' >> identifier
  return $ HiExprApply expr [stringToExpr value]

-- | Parse '!' after expression
parseRun :: HiExpr -> Parser HiExpr
parseRun expr = char '!' >> return (HiExprRun expr)

-- | Parse an apply to expression
parseApply :: HiExpr -> Parser HiExpr
parseApply expr = HiExprApply expr <$> parseArguments

-- | Parse a term after expression
parseAfterExpr :: HiExpr -> Parser HiExpr
parseAfterExpr expr =
  choice $
    map
      (\parser -> parser expr)
      [ parseRun
      , parseApply
      , parseDot
      ]

-- | Parser foldl for HiExpr
foldExpr :: HiExpr -> Parser HiExpr
foldExpr expr = do
  val <- optional $ parseAfterExpr expr
  maybe (return expr) foldExpr val

-- parse primitive type

-- | Parse a bool
bool :: Parser Bool
bool = False <$ string "false" <|> True <$ string "true"

-- | Parse a string
stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

-- | Parse a byte (exactly two digits)
byte :: Parser Word8
byte = do
  first <- fromIntegral . digitToInt <$> satisfy isHexDigit
  second <- fromIntegral . digitToInt <$> satisfy isHexDigit
  return $ first * 16 + second

-- | Parse a list of bytes
bytes :: Parser [Word8]
bytes = byteParens $ lexeme $ sepEndBy byte space1

-- | Parse a identifier
identifier :: Parser String
identifier = intercalate "-" <$> (((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-')

-- | Parse a pair of HiExpr
parsePair :: Parser (HiExpr, HiExpr)
parsePair = do
  first <- parseWithOperators
  _ <- lexeme $ char ':'
  second <- parseWithOperators
  return (first, second)

-- | Parse an arguments without parens
parseArgumentsWithoutParens :: Parser [HiExpr]
parseArgumentsWithoutParens = sepBy parseWithOperators (lexeme $ char ',')

-- | Parse an arguments with parens
parseArguments :: Parser [HiExpr]
parseArguments = lexeme $ parens parseArgumentsWithoutParens

-- helpers

-- | Converting string to HiExpr
stringToExpr :: String -> HiExpr
stringToExpr = HiExprValue . HiValueString . pack

-- | Lexeme for Parser (allows spaces after the Parser)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

-- mapping

-- | Mapping a string to HiFun
mapNameFun :: String -> Maybe HiFun
mapNameFun "div"              = Just HiFunDiv
mapNameFun "mul"              = Just HiFunMul
mapNameFun "add"              = Just HiFunAdd
mapNameFun "sub"              = Just HiFunSub
mapNameFun "not"              = Just HiFunNot
mapNameFun "and"              = Just HiFunAnd
mapNameFun "or"               = Just HiFunOr
mapNameFun "less-than"        = Just HiFunLessThan
mapNameFun "greater-than"     = Just HiFunGreaterThan
mapNameFun "equals"           = Just HiFunEquals
mapNameFun "not-less-than"    = Just HiFunNotLessThan
mapNameFun "not-greater-than" = Just HiFunNotGreaterThan
mapNameFun "not-equals"       = Just HiFunNotEquals
mapNameFun "if"               = Just HiFunIf
mapNameFun "length"           = Just HiFunLength
mapNameFun "to-upper"         = Just HiFunToUpper
mapNameFun "to-lower"         = Just HiFunToLower
mapNameFun "reverse"          = Just HiFunReverse
mapNameFun "trim"             = Just HiFunTrim
mapNameFun "list"             = Just HiFunList
mapNameFun "range"            = Just HiFunRange
mapNameFun "fold"             = Just HiFunFold
mapNameFun "pack-bytes"       = Just HiFunPackBytes
mapNameFun "unpack-bytes"     = Just HiFunUnpackBytes
mapNameFun "zip"              = Just HiFunZip
mapNameFun "unzip"            = Just HiFunUnzip
mapNameFun "encode-utf8"      = Just HiFunEncodeUtf8
mapNameFun "decode-utf8"      = Just HiFunDecodeUtf8
mapNameFun "serialise"        = Just HiFunSerialise
mapNameFun "deserialise"      = Just HiFunDeserialise
mapNameFun "read"             = Just HiFunRead
mapNameFun "write"            = Just HiFunWrite
mapNameFun "mkdir"            = Just HiFunMkDir
mapNameFun "cd"               = Just HiFunChDir
mapNameFun "parse-time"       = Just HiFunParseTime
mapNameFun "rand"             = Just HiFunRand
mapNameFun "echo"             = Just HiFunEcho
mapNameFun "count"            = Just HiFunCount
mapNameFun "keys"             = Just HiFunKeys
mapNameFun "values"           = Just HiFunValues
mapNameFun "invert"           = Just HiFunInvert
mapNameFun _                  = Nothing

-- | Mapping a string to HiAction
mapNameAction :: String -> Maybe HiAction
mapNameAction "cwd" = Just HiActionCwd
mapNameAction "now" = Just HiActionNow
mapNameAction _     = Nothing

-- parens

-- | Parser for () parens
parens :: Parser a -> Parser a
parens = between (lexeme $ char '(') (char ')')

-- | Parser for [] parens
squareParens :: Parser a -> Parser a
squareParens = between (lexeme $ char '[') (char ']')

-- | Parser for [# #] parens
byteParens :: Parser a -> Parser a
byteParens = between (try $ char '[' >> space >> char '#' >> space) (char '#' >> space >> char ']')

-- | Parser for {} parens
curlyParens :: Parser a -> Parser a
curlyParens = between (lexeme $ char '{') (char '}')

-- operator helpers

-- | Make binary operator of op, converting to HiExpr with f
binaryOperator :: String -> HiFun -> Parser (HiExpr -> HiExpr -> HiExpr)
binaryOperator op f = binaryApply f <$ lexeme (string op)

-- | Make binary operator with non associativity
binaryN :: String -> HiFun -> Operator Parser HiExpr
binaryN op f = InfixN $ binaryOperator op f

-- | Make binary operator with left associativity
binaryL :: String -> HiFun -> Operator Parser HiExpr
binaryL op f = InfixL $ binaryOperator op f

-- | Make binary operator with right associativity
binaryR :: String -> HiFun -> Operator Parser HiExpr
binaryR op f = InfixR $ binaryOperator op f

-- | Make binary operator with left associativity with not followed string
binaryLNotFollowedBy :: String -> String -> HiFun -> Operator Parser HiExpr
binaryLNotFollowedBy notFollowed op f =
  InfixL $ binaryApply f <$ try (lexeme $ string op <* notFollowedBy (string notFollowed))

-- | Make function from binary operator
binaryApply :: HiFun -> HiExpr -> HiExpr -> HiExpr
binaryApply fun x y = HiExprApply (HiExprValue $ HiValueFunction fun) [x, y]

-- | Operators for makeExprParser
operators :: [[Operator Parser HiExpr]]
operators =
  [ [ binaryLNotFollowedBy "=" "/" HiFunDiv
    ,  binaryL "*" HiFunMul
    ],
    [ binaryL "+" HiFunAdd
    , binaryL "-" HiFunSub
    ],
    [ binaryN ">=" HiFunNotLessThan
    , binaryN "<=" HiFunNotGreaterThan
    , binaryN "<" HiFunLessThan
    , binaryN ">" HiFunGreaterThan
    , binaryN "==" HiFunEquals
    , binaryN "/=" HiFunNotEquals
    ],
    [ binaryR "&&" HiFunAnd
    ],
    [ binaryR "||" HiFunOr
    ]
  ]
