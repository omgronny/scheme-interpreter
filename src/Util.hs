module Util where

import Types

import Data.Map (insert, lookup, delete, fromList)

--------------------------------------------------------------------------------

getSymbol :: Expression -> String
getSymbol (Symbol str) = str
getSymbol _ = error "getSymbol on non-string"

getNumber :: EvaledExpression -> Double
getNumber (EvaledNumber num) = num
getNumber _ = error "getNumber on NaN"

maybeGetNumber :: Expression -> Maybe Double
maybeGetNumber (Number num) = Just num
maybeGetNumber _ = Nothing

maybeGetBool :: Expression -> Maybe Bool
maybeGetBool (  Boolean b) = Just b
maybeGetBool _ = Nothing

getBool :: EvaledExpression -> Bool
getBool (EvaledBoolean bool) = bool
getBool _ = True

getBoolOr :: EvaledExpression -> Bool -> Bool
getBoolOr (EvaledBoolean bool) _ = bool
getBoolOr _ var = var

--------------------------------------------------------------------------------

lookupVariable :: [Variables] -> String -> Maybe EvaledExpression
lookupVariable [] _ = Nothing
lookupVariable (h:hs) name = case Data.Map.lookup name h of
    Nothing -> lookupVariable hs name
    Just value -> Just value

defineVariable :: [Variables] -> String -> EvaledExpression -> [Variables]
defineVariable [] _ _ = []
defineVariable (h:hs) name value = (insert name value h) : hs

setVariable :: [Variables] -> String -> EvaledExpression -> [Variables]
setVariable [] _ _ = []
setVariable (h:hs) name value = case Data.Map.lookup name h of
    Nothing -> h : (setVariable hs name value)
    Just _ -> (insert name value h) : hs
