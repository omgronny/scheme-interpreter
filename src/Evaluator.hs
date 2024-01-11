module Evaluator where

import Types

import Data.Map (insert, lookup, fromList)

--------------------------------------------------------------------------------

compareList :: (Ord a) => (a -> a -> Bool) -> [a] -> Bool
compareList _ [] = True
compareList f list = Prelude.and $ zipWith f list (tail list)

getSymbol :: Expression -> String
getSymbol (Symbol str) = str
getSymbol _ = error "getSymbol on non-string"

getNumber :: Maybe Expression -> Double
getNumber Nothing = error "getNumber on Nothing"
getNumber (Just (Number num)) = num
getNumber (Just _) = error "getNumber on NaN"

maybeGetNumber :: Expression -> Maybe Double
maybeGetNumber (Number num) = Just num
maybeGetNumber _ = Nothing

maybeGetBool :: Expression -> Maybe Bool
maybeGetBool (Boolean bool) = Just bool
maybeGetBool _ = Nothing

getBoolOr :: Expression -> Bool -> Bool
getBoolOr (Boolean bool) _ = bool
getBoolOr _ var = var

evalAndGetNumber vars = getNumber . snd . eval vars

checkValue checker hs = do
    if not $ length hs == 1 then Nothing else do
        let val = checker $ head hs
        case val of
            Nothing -> Just $ Boolean False
            Just _ -> Just $ Boolean True

checkBoolList _ [] _ = Just $ Boolean True
checkBoolList vars (h:hs) isAnd = do
    let evaled = snd $ eval vars h
    case evaled of
        Nothing -> Nothing
        Just ev -> do
            if isAnd
                then do
                    let con = getBoolOr ev True
                    if not con then Just ev else do
                        if null hs then Just ev else (checkBoolList vars hs isAnd)
                else do
                    let con = getBoolOr ev True
                    if con then Just ev else do
                        if null hs then Just (Boolean False) else (checkBoolList vars hs isAnd)

--------------------------------------------------------------------------------

eval :: Variables -> Expression -> (Variables, Maybe Expression)

eval v (Number num) = (v, Just (Number num))
eval v (String str) = (v, Just (String str))
eval v (Boolean bool) = (v, Just (Boolean bool))

eval v (Operator _) = (v, Nothing) -- operator itself is an error
eval v (Symbol smb) = (v, Data.Map.lookup smb v)

eval v (Quote _) = (v, Nothing) -- TODO

eval v (List []) = (v, Just (List []))
eval vars (List list) = do
    case simpleEval vars (List list) of
        Just value -> (vars, Just value)
        Nothing -> complexEval vars (List list)

simpleEval :: Variables -> Expression -> Maybe Expression
simpleEval vars (List (h:hs)) = do
    case h of

        Operator "+" -> Just $ Number $ sum (map (evalAndGetNumber vars) hs)
        Operator "*" -> Just $ Number $ product (map (evalAndGetNumber vars) hs)
        Operator "-" -> Just $ Number $ foldl (-) (evalAndGetNumber vars $ head hs) (map (evalAndGetNumber vars) (tail hs))
        Operator "/" -> Just $ Number $ foldl (/) (evalAndGetNumber vars $ head hs) (map (evalAndGetNumber vars) (tail hs))

        Operator ">" -> Just $ Boolean $ compareList (>) hs -- TODO eval it
        Operator ">=" -> Just $ Boolean $ compareList (>=) hs
        Operator "<" -> Just $ Boolean $ compareList (<) hs
        Operator "<=" -> Just $ Boolean $ compareList (<=) hs
        Operator "=" -> Just $ Boolean $ compareList (==) hs

        Symbol "number?" -> checkValue maybeGetNumber hs
        Symbol "boolean?" -> checkValue maybeGetBool hs

        Symbol "max" -> Just $ Number $ foldl max (evalAndGetNumber vars $ head hs) (map (evalAndGetNumber vars) (tail hs))
        Symbol "min" -> Just $ Number $ foldl min (evalAndGetNumber vars $ head hs) (map (evalAndGetNumber vars) (tail hs))
        Symbol "abs" -> if length hs > 1 then Nothing else do
                let num = maybeGetNumber $ head hs
                case num of
                    Nothing -> Nothing
                    Just n -> Just $ Number (abs n)

        Symbol "not" -> if length hs > 1 then Nothing else do
                let bool = maybeGetBool $ head hs
                case bool of
                    Nothing -> Just $ Boolean False
                    Just b -> Just $ Boolean (not b)

        Symbol "and" -> checkBoolList vars hs True
        Symbol "or" -> checkBoolList vars hs False

        Symbol "define" -> Nothing
        Symbol "set!" -> Nothing

        Symbol smb -> Data.Map.lookup smb vars

        _ -> Nothing
simpleEval _ _ = Nothing

complexEval :: Variables -> Expression -> (Variables, Maybe Expression)
complexEval vars (List (h:hs)) = do
    case h of
        Symbol "define" -> if not $ length hs == 2 then (vars, Nothing) else do
            let name = getSymbol $ head hs
            case snd $ eval vars (hs!!1) of
                Nothing -> (vars, Nothing)
                Just value -> (insert name value vars, Just value)

        Symbol "set!" -> if not $ length hs == 2 then (vars, Nothing) else do
            let name = getSymbol $ head hs

            case Data.Map.lookup name vars of
                Just _ -> do
                    case snd $ eval vars (hs!!1) of
                        Nothing -> (vars, Nothing)
                        Just value -> (insert name value vars, Just value)
                Nothing -> (vars, Nothing)

        _ -> (vars, Nothing)
complexEval vars _ = (vars, Nothing)