module Types (
    Variables,
    Expression(
        Number,
        Boolean,
        Operator,
        Symbol,
        String,
        Quote,
        List),
    initVars,
  ) where

import qualified Data.List as L
import Data.Map hiding (map)

--------------------------------------------------------------------------------

data Expression = Number Double
                | Boolean Bool
                | Operator String
                | Symbol String
                | String String
                | Quote Expression
                | List [Expression]
                  deriving (Eq, Ord)

isInt x = x == fromInteger (round x)

instance Show Expression where
  show (Number n) = if isInt n then show (round n) else show n
  show (String s) = show s
  show (Boolean b) = if b then "#t" else "#f"
  show (Symbol s) = show s
  show (Quote e) = show e
  show (List exprs) = "(" ++ L.unwords (map show exprs) ++ ")"
  show (Operator c) = show c

--------------------------------------------------------------------------------

type Variables = Map String Expression

initVars :: Variables
initVars = fromList []


