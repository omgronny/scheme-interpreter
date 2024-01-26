module Types
  ( Variables,
    Expression
      ( Number,
        Boolean,
        Operator,
        Symbol,
        String,
        Quote,
        List
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
    getExpr,
    getEvaledExpr
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

instance Show EvaledExpression where
  show (EvaledNumber n) = if isInt n then show (round n) else show n
  show (EvaledString s) = show s
  show (EvaledBoolean b) = if b then "#t" else "#f"
  show (EvaledSymbol s) = show s
  show (EvaledQuote e) = show e
  show (EvaledList exprs) = "(" ++ L.unwords (map show exprs) ++ ")"
  show (EvaledOperator c) = show c
  show (EvaledLambda _) = "lambda"


getEvaledExpr :: Expression -> EvaledExpression
getEvaledExpr (Number x) = EvaledNumber x
getEvaledExpr (Symbol x) = EvaledSymbol x
getEvaledExpr (String x) = EvaledString x
getEvaledExpr (Quote x) = EvaledQuote x
getEvaledExpr (List x) = EvaledList x
getEvaledExpr (Boolean x) = EvaledBoolean x
getEvaledExpr (Operator x) = EvaledOperator x

getExpr :: EvaledExpression -> Expression
getExpr (EvaledNumber x) = Number x
getExpr (EvaledSymbol x) = Symbol x
getExpr (EvaledString x) = String x
getExpr (EvaledQuote x) = Quote x
getExpr (EvaledList x) = List x
getExpr (EvaledBoolean x) = Boolean x
getExpr (EvaledOperator x) = Operator x
getExpr (EvaledLambda _) = error "internal: bad conversion"

--------------------------------------------------------------------------------

type Variables = Map String EvaledExpression

initVars :: [Variables]
initVars =  [fromList []]

initVar :: Variables
initVar =  fromList []
