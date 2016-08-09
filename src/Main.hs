module Main where


import Numeric
import Data.Char hiding (isNumber, isSymbol)

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad -- For `liftM` which unravels the value inside the monad
import Control.Monad.Error


instance Show LispVal where show = showVal
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char
             | Float Float
             | Bool Bool

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action $ return . show

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


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
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Float _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
                     do result <- eval pred
                        case result of
                          Bool False -> eval alt
                          otherwise -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
                  (throwError $ NotFunction "Unrecognized primite function args" func)
                  ($ args)
                  (lookup func primitives)

primitives :: [ (String, [LispVal] -> ThrowsError LispVal) ]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("remainder", numericBinop rem)
             , ("string?", isString)
             , ("number?", isNumber)
             , ("symbol?", isSymbol)
             , ("symbol->string", symbolToString)
             , ("string->symbol", stringToSymbol)
             , ("=", numBoolBinop (==))
             , ("<", numBoolBinop (<))
             , (">", numBoolBinop (>))
             , ("/=", numBoolBinop (/=))
             , (">=", numBoolBinop (>=))
             , ("<=", numBoolBinop (<=))
             , ("&&", boolBoolBinop (&&))
             , ("||", boolBoolBinop (||))
             , ("string=?", strBoolBinop (==))
             , ("string<?", strBoolBinop (<))
             , ("string>?", strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))
             ]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                  then throwError $ NumArgs 2 args
                                  else do left <- unpacker $ args !! 0
                                          right <- unpacker $ args !! 1
                                          return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool


isString :: [LispVal] -> ThrowsError LispVal
isString [(String _)] = return $ Bool True
isString _ = return $ Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [(Number _)] = return $ Bool True
isNumber [(Float _)] = return $ Bool True
isNumber _ = return $ Bool False

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [(Atom _)] = return $ Bool True
isSymbol _ = return $ Bool False

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [(Atom val)] = return $ String val
symbolToString [val] = return $ val

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [(String val)] = return $ Atom val
stringToSymbol [val] = return $ val

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> throwError $ Parser err
                   Right value -> return value


main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show (readExpr (args !! 0) >>= eval)
  putStrLn $ extractValue $ trapError evaled
