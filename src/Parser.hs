module Parser where

import Control.Applicative (many)
import Control.Monad (void)
import Text.Parsec (anyChar, char, digit, endOfLine, noneOf, parse, sepBy, string, try, (<|>))
import Text.Parsec.Char (spaces)
import Text.Parsec.Combinator (choice, many1, manyTill)
import Text.Parsec.String (Parser)
import Types

--------------------------------------------------------------------------------

separator :: Parser ()
separator = do
  void $ try comment <|> try spaces
  return ()

comment :: Parser ()
comment = do
  void $ char ';'
  void $ manyTill anyChar endOfLine

--------------------------------------------------------------------------------

openParens :: Parser ()
openParens = do
  void separator
  void $ char '('
  void separator

closedParens :: Parser ()
closedParens = do
  void separator
  void $ char ')'
  void separator

--------------------------------------------------------------------------------

readNumber = rd <$> (plus' <|> minus <|> number)
  where
    rd = read :: String -> Double
    plus' = char '+' *> number
    minus = (:) <$> char '-' <*> number
    number = many1 digit

pNumber :: Parser Expression
pNumber = do
  void separator
  num <- readNumber
  void separator
  return $ Number $ num

pBoolean :: Parser Expression
pBoolean =
  do
    void separator
    try
      ( do
          string "#t"
          return $ Boolean True
      )
    <|> try
      ( do
          string "#f"
          return $ Boolean False
      )

pString :: Parser Expression
pString = do
  void $ char '"'
  str <- many $ noneOf "\""
  void $ char '"'
  return $ String str

pOperator :: Parser Expression
pOperator = do
  o <-
    choice
      [ try (string "+"),
        try (string "*"),
        try (string "/"),
        try (string "-"),
        try (string "="),
        try (string ">="),
        try (string "<="),
        try (string ">"),
        try (string "<")
      ]
  void separator
  return $ Operator o

pSymbol :: Parser Expression
pSymbol = do
  void separator
  smb <- many1 $ noneOf "\n\t ()'\""
  void separator
  return $ Symbol smb

pQuote :: Parser Expression
pQuote = do
  void separator
  void $ char '\''
  expr <- pExpression
  return $ Quote expr

pList :: Parser Expression
pList = do
  void openParens
  exprs <- many pExpression
  void closedParens
  return $ List exprs

--------------------------------------------------------------------------------

pExpression :: Parser Expression
pExpression =
  try pNumber
    <|> try pString
    <|> try pBoolean
    <|> try pOperator
    <|> try pSymbol
    <|> try pQuote
    <|> try pList

pExpr :: Parser Expression
pExpr = do
  expr <- pExpression
  return $ expr

pForm :: Parser Expression
pForm = try pExpr
