module Main where

import Control.Monad
import Control.Monad.Except
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

-- Define a type for parser output
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

-- Parser for symbols allowed in Scheme identifiers
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Recognise spaces, and ignore them by carrying Unit type with Parser
spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return (String x)

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return
    ( case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom
    )

parseNumber :: Parser LispVal
parseNumber = fmap (Number . read) (many1 digit)

parseList :: Parser LispVal
parseList = fmap List (sepBy parseExpr spaces)

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return (DottedList head tail)

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  char '\''
  return (List [Atom "quote", x])

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Atom a) = a
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List list) = "(" ++ unwordsList list ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal

-- Entry point to parse an expression
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String ("No match: " ++ show err)
  Right val -> val

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func (map eval args)

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) (lookup func primitives)

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNumFromLispVal params

unpackNumFromLispVal :: LispVal -> Integer
unpackNumFromLispVal (Number n) = n
unpackNumFromLispVal (String s) =
  let parsed = reads s :: [(Integer, String)]
   in if null parsed
        then 0
        else fst $ head parsed
unpackNumFromLispVal (List [n]) = unpackNumFromLispVal n
unpackNumFromLispVal _ = 0

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
