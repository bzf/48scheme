module Main where


import Numeric
import Data.Char hiding (isNumber, isSymbol)

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad -- For `liftM` which unravels the value inside the monad


instance Show LispVal where show = showVal
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char
             | Float Float
             | Bool Bool


symbol :: Parser Char
symbol = oneOf "!$%&*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space


parseString :: Parser LispVal
parseString = do
  char '"'
  value <- many chars
  char '"'
  return $ String value
    where chars = escapedCharacters <|> noneOf "\"\\"

escapedCharacters :: Parser Char
escapedCharacters = do
  char '\\' -- a backslash
  x <- oneOf "\\\"nrt"
  return $ case x of
             '\\' -> x
             '"' -> x
             'n' -> '\n'
             'r' -> '\r'
             't' -> '\t'


parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _    -> Atom atom

binaryDigit :: Parser Char
binaryDigit = oneOf "01"

isBinaryDigit :: Char -> Bool
isBinaryDigit c = (c == '0') || (c == '1')

readBin :: ReadS Integer
readBin = readInt 2 isBinaryDigit digitToInt

parseNumber :: Parser LispVal
parseNumber = try parseFloat
           <|> parseDecimal
           <|> parseDecimalWithPrefix
           <|> parseHex
           <|> parseOct
           <|> parseBin

parseFloat :: Parser LispVal
parseFloat = do
  first <- many1 digit
  char '.'
  second <- many1 digit
  return $ Float (fst . head . readFloat $ first ++ "." ++ second)

parseDecimal :: Parser LispVal
parseDecimal = many1 digit >>= (return . Number . read)

parseDecimalWithPrefix :: Parser LispVal
parseDecimalWithPrefix = do
  try $ string "#d"
  digits <- many1 digit
  (return . Number . read) digits

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  digits <- many1 hexDigit
  return $ Number (fst . head . readHex $ digits)

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  digits <- many1 hexDigit
  return $ Number (fst . head . readOct $ digits)

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  digits <- many1 binaryDigit
  return $ Number (fst . head . readBin $ digits)

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  value <- try (string "newline" <|> string "space")
           <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
  return $ Character $ case value of
                         "space" -> ' '
                         "newline" -> '\n'
                         otherwise -> (value !! 0)


parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> parseQuoted
        <|> do
          char '('
          list <- try parseList <|> parseDottedList
          char ')'
          return list


showVal :: LispVal -> String
showVal (String content) = "\"" ++ content ++ "\""
showVal (Character char) = show char
showVal (Atom name) = name
showVal (Number number) = show number
showVal (Float number) = show number
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List items) = "(" ++ unwordsList items ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ ")" -- " . " ++ showVal tail ")"

-- Helper function for printing a list
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- This could probably be better as a case
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Float _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [ (String, [LispVal] -> LispVal) ]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("remainder", numericBinop rem)
             , ("string?", isString)
             , ("number?", isNumber)
             , ("symbol?", isSymbol)
             ]

isString :: [LispVal] -> LispVal
isString [(String _)] = Bool True
isString _ = Bool False

isNumber :: [LispVal] -> LispVal
isNumber [(Number _)] = Bool True
isNumber [(Float _)] = Bool True
isNumber _ = Bool False

isSymbol :: [LispVal] -> LispVal
isSymbol [(Atom _)] = Bool True
isSymbol _ = Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right value -> value


main :: IO ()
main = do
  getArgs >>= print . eval . readExpr . head
