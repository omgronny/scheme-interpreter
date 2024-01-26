module Types
  ( Variables,
    Expression
      ( Number,
        Boolean,
        Operator,
        Symbol,
        String,
        Quote,
        List,
        Lambda
      ),
    EvaledExpression
      ( EvaledNumber,
        EvaledBoolean,
        EvaledOperator,
        EvaledSymbol,
        EvaledString,
        EvaledQuote,
        EvaledList,
        EvaledLambda
      ),
    initVars,
    initVar,
  )
where

import qualified Data.List as L
import Data.Map hiding (map)

--------------------------------------------------------------------------------

data Expression
  = Number Double
  | Boolean Bool
  | Operator String
  | Symbol String
  | String String
  | Quote Expression
  | List [Expression]
  | Lambda (Expression, [Expression])
  deriving (Eq, Ord)

data EvaledExpression
  = EvaledNumber Double
  | EvaledBoolean Bool
  | EvaledOperator String
  | EvaledSymbol String
  | EvaledString String
  | EvaledQuote Expression
  | EvaledList [Expression]
  | EvaledLambda (Expression, [Expression])
  deriving (Eq, Ord)

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

instance Show Expression where
  show (Number n) = if isInt n then show (round n) else show n
  show (String s) = show s
  show (Boolean b) = if b then "#t" else "#f"
  show (Symbol s) = show s
  show (Quote e) = show e
  show (List exprs) = "(" ++ L.unwords (map show exprs) ++ ")"
  show (Operator c) = show c
  show (Lambda _) = "lambda"

instance Show EvaledExpression where
  show (EvaledNumber n) = if isInt n then show (round n) else show n
  show (EvaledString s) = show s
  show (EvaledBoolean b) = if b then "#t" else "#f"
  show (EvaledSymbol s) = show s
  show (EvaledQuote e) = show e
  show (EvaledList exprs) = "(" ++ L.unwords (map show exprs) ++ ")"
  show (EvaledOperator c) = show c
  show (EvaledLambda _) = "lambda"

--------------------------------------------------------------------------------

type Variables = Map String Expression

initVars :: [Variables]
initVars =  [fromList []]

initVar :: Variables
initVar =  fromList []
