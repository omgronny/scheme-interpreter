module Evaluator where

import Types

import Data.Map (insert, lookup, delete, fromList)

--------------------------------------------------------------------------------

compareList vars f list = doCompareList f (map (evalAndGetNumber vars) list)

doCompareList :: (Ord a) => (a -> a -> Bool) -> [a] -> Bool
doCompareList _ [] = True
doCompareList f list = Prelude.and $ zipWith f list (tail list)

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

fillParamsInFunction :: Variables -> Expression -> [Expression] -> Variables
fillParamsInFunction oldVars (List []) [] = oldVars
fillParamsInFunction oldVars (List (n:ns)) (v:vs) = do
    let (evalVars, evaledV) = eval oldVars v
    case evaledV of
        Nothing -> oldVars
        Just v' -> do
            let appended = insert (getSymbol n) v' evalVars
            fillParamsInFunction appended (List ns) vs
fillParamsInFunction oldVars _ _ = oldVars

evalAndGetNumber :: Variables -> Expression -> Double
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

evalLines :: Variables -> [Expression] -> (Variables, Maybe Expression)
evalLines vars [] = (vars, Nothing)
evalLines vars [h] = eval vars h
evalLines vars (l:ls) = do
    let (vars', _) = eval vars l
    evalLines vars' ls

eval :: Variables -> Expression -> (Variables, Maybe Expression)

eval v (Number num) = (v, Just (Number num))
eval v (String str) = (v, Just (String str))
eval v (Boolean bool) = (v, Just (Boolean bool))

eval v (Operator _) = (v, Nothing) -- operator itself is an error
eval v (Lambda _) = (v, Nothing) -- lambda itself is an error

eval v (Symbol smb) = (v, Data.Map.lookup smb v)

eval v (Quote q) = (v, Just q)

eval v (List []) = (v, Just (List []))
eval vars (List list) = do
    case simpleEval vars (List list) of
        Just value -> (vars, Just value)
        Nothing -> complexEval vars (List list)

simpleEval :: Variables -> Expression -> Maybe Expression
simpleEval vars (List list@(h:hs)) = do
    case h of

        Operator "+" -> Just $ Number $ sum (map (evalAndGetNumber vars) hs)
        Operator "*" -> Just $ Number $ product (map (evalAndGetNumber vars) hs)
        Operator "-" -> Just $ Number $ foldl (-) (evalAndGetNumber vars $ head hs) (map (evalAndGetNumber vars) (tail hs))
        Operator "/" -> Just $ Number $ foldl (/) (evalAndGetNumber vars $ head hs) (map (evalAndGetNumber vars) (tail hs))

        Operator ">" -> Just $ Boolean $ compareList vars (>) hs
        Operator ">=" -> Just $ Boolean $ compareList vars (>=) hs
        Operator "<" -> Just $ Boolean $ compareList vars (<) hs
        Operator "<=" -> Just $ Boolean $ compareList vars (<=) hs
        Operator "=" -> Just $ Boolean $ compareList vars (==) hs

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

        Symbol "lambda" -> Just $ Lambda list

        Symbol "set-car!" -> Nothing
        Symbol "set-cdr!" -> Nothing
        Symbol "car" -> Nothing
        Symbol "cdr" -> Nothing
        Symbol "define" -> Nothing
        Symbol "set!" -> Nothing
        Symbol "if" -> Nothing

        Symbol _ -> evalSymbol vars (List list)

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

        Symbol "set-car!" -> do
            let name = getSymbol (head hs)
            case Data.Map.lookup name vars of
                Nothing -> (vars, Nothing)
                Just val -> case val of
                    List (_:ls) -> do
                        case snd $ eval vars (hs!!1) of
                            Nothing -> (vars, Nothing)
                            Just evaled -> do
                                let newList = List (evaled : ls)
                                let newVars = insert name newList (delete name vars)
                                (newVars, Just evaled)
                    _ -> (vars, Nothing)

        Symbol "car" -> do
            let (newVars, evaled) = eval vars (head hs)
            case evaled of
                Nothing -> (newVars, Nothing)
                Just value -> case value of
                    List (l:_) -> (newVars, Just l)
                    _ -> (newVars, Nothing)

        Symbol "set-cdr!" -> do
            let name = getSymbol (head hs)
            case Data.Map.lookup name vars of
                Nothing -> (vars, Nothing)
                Just val -> case val of
                    List (l:_) -> do
                        case snd $ eval vars (hs!!1) of
                            Nothing -> (vars, Nothing)
                            Just evaled -> do
                                let newList = List [l, evaled]
                                let newVars = insert name newList (delete name vars)
                                (newVars, Just evaled)
                    _ -> (vars, Nothing)

        Symbol "cdr" -> do
            let (newVars, evaled) = eval vars (head hs)
            case evaled of
                Nothing -> (newVars, Nothing)
                Just value -> case value of
                    List (_:ls) -> (newVars, Just (head ls))
                    _ -> (newVars, Nothing)

        Symbol "if" -> if not $ length hs == 3 then (vars, Nothing) else do
            let (newVars, cond) = eval vars (head hs)
            case cond of
                Nothing -> (newVars, Nothing)
                Just cond' -> do
                    let passed = case cond' of
                            Boolean bool -> bool
                            _ -> True

                    if passed
                        then eval newVars (hs!!1)
                        else eval newVars (hs!!2)

        _ -> (vars, Nothing)
complexEval vars _ = (vars, Nothing)

evalSymbol :: Variables -> Expression -> Maybe Expression
evalSymbol vars (List (h:hs)) = do
    -- h – name, hs – params to call
    let value = Data.Map.lookup (getSymbol h) vars
    case value of
        Nothing -> Nothing
        Just val -> case val of
            Lambda (_:lhs) -> do
                let vars' = fillParamsInFunction vars (head lhs) hs
                snd $ evalLines vars' (tail lhs)
            _ -> value
evalSymbol _ _ = Nothing