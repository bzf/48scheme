module Main where


import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad -- For `liftM` which unravels the value inside the monad


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool deriving (Show)


symbol :: Parser Char
symbol = oneOf "!#$%&*+-/:<=>?@^_~"


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
             _ -> Atom atom


parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit


parseExpr :: Parser LispVal
parseExpr = parseString <|> parseAtom <|> parseNumber


readExpr :: String -> String
readExpr input = case parse (parseExpr) "list" input of
  Left err -> "No match: " ++ show err
  Right value -> "Found value: " ++ show value


main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr
