module Types (
    Expression(
        Number,
        Boolean,
        Operator,
        Symbol,
        String,
        Quote,
        List),
    Form (FExpr),
    Program (Program),
  ) where

import qualified Data.List as L
import Data.Map()

--------------------------------------------------------------------------------

data Expression = Number Double
                | Boolean Bool
                | Operator String
                | Symbol String
                | String String
                | Quote Expression
                | List [Expression]
                  deriving (Eq, Ord)

instance Show Expression where
  show (Number n) = "Number " ++ show n
  show (String s) = "String " ++ show s
  show (Boolean b) = "Boolean " ++ if b then "true" else "false"
  show (Symbol s) = "Symbol " ++ show s
  show (Quote e) = "'" ++ show e
  show (List exprs) = "List " ++ "(" ++ L.unwords (map show exprs) ++ ")"
  show (Operator c) = "Operator " ++ show c

--------------------------------------------------------------------------------

data Form = FExpr Expression
  deriving (Eq, Show)

data Program = Program [Form]


