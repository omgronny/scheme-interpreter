module Evaluator where

import Types
import Util

import Data.Map (insert, lookup, delete, fromList)

--------------------------------------------------------------------------------

compareList :: [Variables] -> (Double -> Double -> Bool) -> [Expression] -> IO Bool
compareList vars f list = do
    a <- sequence (map (evalAndGetNumber vars) list)
    return $ doCompareList f a

doCompareList :: (a -> a -> Bool) -> [a] -> Bool
doCompareList _ [] = True
doCompareList _ [_] = True
doCompareList f list@(_:ls) = Prelude.and $ zipWith f list ls

--------------------------------------------------------------------------------

fillParamsInFunction :: [Variables] -> Expression -> [Expression] -> IO [Variables]
fillParamsInFunction oldVars (List []) [] = return oldVars
fillParamsInFunction oldVars (List (n:ns)) (v:vs) = do
    (evalVars, evaledV) <- eval oldVars v
    let appended = defineVariable evalVars (getSymbol n) evaledV
    fillParamsInFunction appended (List ns) vs
fillParamsInFunction oldVars _ _ = return oldVars

--------------------------------------------------------------------------------

evalAndGetNumber :: [Variables] -> Expression -> IO Double
evalAndGetNumber vars exprIO = do
    (_, evaled) <- eval vars exprIO
    return $ getNumber evaled

checkValue :: (a1 -> Maybe a2) -> [a1] -> Expression
checkValue _ [] = error "internal"
checkValue checker (h:_) = do
    let val = checker h
    case val of
        Nothing -> Boolean False
        Just _ -> Boolean True


checkBoolList :: [Variables] -> [Expression] -> Bool -> IO ([Variables], Expression)
checkBoolList v [] _ = return $ (v, Boolean True)
checkBoolList vars (h:hs) isAnd = do
    (_, evaled) <- eval vars h
    if isAnd
        then do
            let con = getBoolOr evaled True
            if not con then return (vars, evaled) else do
                if null hs then return (vars, evaled) else (checkBoolList vars hs isAnd)
        else do
            let con = getBoolOr evaled True
            if con then return (vars, evaled) else do
                if null hs then return $ (vars, Boolean False) else (checkBoolList vars hs isAnd)

--------------------------------------------------------------------------------

foldList :: [Variables] -> [Expression] -> (Double -> Double -> Double) -> IO ([Variables], Expression)
foldList _ [] _ = error "internal error: empty list"
foldList vars (h:hs) f = do
    a <- sequence (map (evalAndGetNumber vars) hs)
    b <- evalAndGetNumber vars h
    return $ (vars, Number $ foldl f b a)

foldCompareList :: [Variables] -> [Expression] -> (Double -> Double -> Bool) -> IO ([Variables], Expression)
foldCompareList vars hs f = do
    a <- compareList vars f hs
    return $ (vars, Boolean $ a)

--------------------------------------------------------------------------------

evalAbs :: a -> [Expression] -> IO (a, Expression)
evalAbs _ [] = error "syntax error: abs"
evalAbs vars (h:_) = return $ (vars, Number $ abs (getNumber h))

evalNot :: a -> [Expression] -> IO (a, Expression)
evalNot _ [] = error "syntax error: not"
evalNot vars (h:_) = do
    let bool = getBool h
    return $ (vars, Boolean $ not bool)


--------------------------------------------------------------------------------

evalLines :: [Variables] -> [Expression] -> IO ([Variables], Expression)
evalLines _ [] = error "evalLines on empty list"
evalLines vars [h] = eval vars h
evalLines vars (l:ls) = do
    (vars', _) <- eval vars l
    evalLines vars' ls

eval :: [Variables] -> Expression -> IO ([Variables], Expression)

eval v (Number num) = return $ (v, (Number num))
eval v (String str) = return $ (v, (String str))
eval v (Boolean bool) = return $ (v, (Boolean bool))

eval _ (Operator _) = error "operator error" -- operator itself is an error
eval _ (Lambda _) = error "internal lambda error" -- lambda itself is an error

eval v (Symbol smb) = return $ (v, case lookupVariable v smb of
    Nothing -> error "unknown name"
    Just var -> var)

eval v (Quote q) = return $ (v, q)

eval v (List []) = return $ (v, (List []))
eval vars (List list) = doEval vars (List list)

doEval :: [Variables] -> Expression -> IO ([Variables], Expression)
doEval vars (List list@(h:hs)) = do
    case h of
        Operator "+" -> foldList vars hs (+)
        Operator "*" -> foldList vars hs (*)
        Operator "-" -> foldList vars hs (-)
        Operator "/" -> foldList vars hs (/)

        Operator ">" -> foldCompareList vars hs (>)
        Operator ">=" -> foldCompareList vars hs (>=)
        Operator "<" -> foldCompareList vars hs (<)
        Operator "<=" -> foldCompareList vars hs (<=)
        Operator "=" -> foldCompareList vars hs (==)

        Symbol "number?" -> return $ (vars, checkValue maybeGetNumber hs)
        Symbol "boolean?" -> return $ (vars, checkValue maybeGetBool hs)

        Symbol "max" -> foldList vars hs max
        Symbol "min" -> foldList vars hs min

        Symbol "abs" -> evalAbs vars hs
        Symbol "not" -> evalNot vars hs

        Symbol "and" -> checkBoolList vars hs True
        Symbol "or" -> checkBoolList vars hs False

        Symbol "lambda" -> return $ (vars, Lambda (h, hs))

        Symbol "define" -> if not $ length hs == 2 then error "" else do
            let name = getSymbol $ head hs
            (_, evaled) <- eval vars (hs!!1)
            return (defineVariable vars name evaled, evaled)

        Symbol "set!" -> if not $ length hs == 2 then error "" else do
            let name = getSymbol $ head hs

            case lookupVariable vars name of
                Just _ -> do
                    (_, evaled) <- eval vars (hs!!1)
                    return (setVariable vars name evaled, evaled)
                Nothing -> error "unknown name"

        Symbol "set-car!" -> do
            let name = getSymbol (head hs)
            case lookupVariable vars name of
                Nothing -> error "unknown name"
                Just val -> case val of
                    List (_:ls) -> do
                        (_, evaled) <- eval vars (hs!!1)
                        let newList = List (evaled : ls)
                        let newVars = setVariable vars name newList
                        return (newVars, evaled)
                    _ -> error ""

        Symbol "car" -> do
            (newVars, evaled) <- eval vars (head hs)
            case evaled of
                List (l:_) -> return (newVars, l)
                _ -> error ""

        Symbol "set-cdr!" -> do
            let name = getSymbol (head hs)
            case lookupVariable vars name of
                Nothing -> error "unknown name"
                Just val -> case val of
                    List (l:_) -> do
                        (_, evaled) <- eval vars (hs!!1)
                        let newList = List [l, evaled]
                        let newVars = setVariable vars name newList
                        return (newVars, evaled)
                    _ -> error ""

        Symbol "cdr" -> do
            (newVars, evaled) <- eval vars (head hs)
            case evaled of
                List (_:l:_) -> return (newVars, l)
                _ -> error ""

        Symbol "if" -> if not $ length hs == 3 then error "" else do
            (newVars, cond) <- eval vars (head hs)
            let passed = case cond of
                    Boolean bool -> bool
                    _ -> True

            if passed
                then eval newVars (hs!!1)
                else eval newVars (hs!!2)

        Symbol _ -> evalSymbol vars (List list)

        _ -> error ""
doEval _ _ = error ""

evalSymbol :: [Variables] -> Expression -> IO ([Variables], Expression)
evalSymbol vars (List (h:hs)) = do
    -- h – name, hs – params to call
    case lookupVariable vars (getSymbol h) of
        Nothing -> error "unknown name"
        Just val -> case val of
            Lambda (_, lh:lhs) -> do
                vars' <- fillParamsInFunction (initVar : vars) lh hs
                (evaledV, expr) <- evalLines vars' lhs
                return ((tail evaledV), expr)
            _ -> return (vars, val)
evalSymbol _ _ = error ""
