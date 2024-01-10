# Лабораторная работа 3

## Реализация

### Реализация методов интерполяции: линейного и Лагранжа

```haskell
linearInterpolation :: [Double] -> [Double] -> [Double] -> [Double]
linearInterpolation inputX inputY pointsToPredict = doLinearInterpolation inputX inputY pointsToPredict (getIndexesForPoints inputX pointsToPredict)

doLinearInterpolation :: [Double] -> [Double] -> [Double] -> [Int] -> [Double]
doLinearInterpolation inputX inputY pointsToPredict indexesForPointsToPredict =
  map predict (zip pointsToPredict indexesForPointsToPredict)
  where
    predict :: (Double, Int) -> Double
    predict (point, index) = do
      let a = (inputY !! (index + 1) - inputY !! index) / (inputX !! (index + 1) - inputX !! (index))
      let b = inputY !! index - a * inputX !! index
      a * point + b

--------------------------------------------------------------------------------

lagrangiaInterpolation :: [Double] -> [Double] -> [Double] -> [Double]
lagrangiaInterpolation inputX inputY pointsToPredict =
  map predict pointsToPredict
  where
    -- L(x) = sum_i (yi * li(x))
    predict :: Double -> Double
    predict x = sum (map (calcCoef x) (zip inputX inputY))

    -- li(x) = mul_j ( (x - xi) / (xi - xj) )
    calcCoef :: Double -> (Double, Double) -> Double
    calcCoef x (inpX, inpY) = inpY * (foldl (foldSkip x inpX) 1 inputX) / (foldl (foldSkip inpX inpX) 1 inputX)

    foldSkip :: Double -> Double -> Double -> Double -> Double
    foldSkip x forbid fl xi
      | forbid == xi = fl
      | otherwise = fl * (x - xi)
```

### Реализация функций ввода-вывода

```haskell
readPoint :: IO (Maybe (Double, Double))
readPoint = catch doReadPoint handler

doReadPoint :: IO (Maybe (Double, Double))
doReadPoint = do
  line <- getLine

  let doubles = map T.unpack (T.splitOn (T.pack ":") (T.pack line))
  return (Just (read (doubles !! 0) :: Double, read (doubles !! 1) :: Double))

handler :: IOError -> IO (Maybe (Double, Double))
handler _ = return Nothing

--------------------------------------------------------------------------------

writePoints :: String -> [Double] -> [Double] -> IO ()
writePoints method a b
  | null a = putStr ""
  | otherwise = do
      putStr method
      putStr ": x: "
      printf "%.3f" (head a)
      putStr "; y: "
      printf "%.3f" (head b)
      putStr "\n"
      writePoints method (tail a) (tail b)
```

### Реализация основной функции интерполяции

```haskell
run windowSize freq methods windowX windowY inBegin = do
  inp <- readPoint
  case inp of
    Just (pointX, pointY) -> doRun (windowX ++ [pointX]) (windowY ++ [pointY]) pointX
    Nothing -> do
      let pointsToPredict = generatePointsToEnd (head windowX) (windowX !! (length windowX - 1)) freq
      interpolateAndPrint methods windowX windowY pointsToPredict
  where
    doRun :: [Double] -> [Double] -> Double -> IO ()
    doRun windowX' windowY' pointX
      | length windowX' < windowSize = run windowSize freq methods windowX' windowY' inBegin
      | otherwise = do
          let pointsToPredict =
                if inBegin
                  then generatePointsFromBegin (head windowX') pointX freq
                  else [(head windowX' + pointX) / 2]
          interpolateAndPrint methods windowX' windowY' pointsToPredict

          run windowSize freq methods (tail windowX') (tail windowY') False

    interpolateAndPrint :: [String] -> [Double] -> [Double] -> [Double] -> IO ()
    interpolateAndPrint [] _ _ _ = putStr ""
    interpolateAndPrint (hMethod : tMethods) windowX' windowY' pointsToPredict = do
      let result = getMethod hMethod windowX' windowY' pointsToPredict
      writePoints hMethod pointsToPredict result
      interpolateAndPrint tMethods windowX' windowY' pointsToPredict
```

## Вывод программы

```bash
$ cat test/data/test1 | ./interpolate  --step=0.5 --window=3 --method=linear
linear: x: 0.500; y: 2.500
linear: x: 1.000; y: 3.000
linear: x: 1.500; y: 3.500
linear: x: 2.500; y: 9.250
linear: x: 3.000; y: 14.500
linear: x: 3.500; y: 19.750
```

```bash
$ cat test/data/test1 | ./interpolate  --step=0.5 --window=3 --method=lagrangia
lagrangia: x: 0.500; y: 0.719
lagrangia: x: 1.000; y: 0.625
lagrangia: x: 1.500; y: 1.719
lagrangia: x: 2.500; y: 9.250
lagrangia: x: 3.000; y: 14.500
lagrangia: x: 3.500; y: 19.750
```

```bash
$ cat test/data/test1 | ./interpolate  --step=0.5 --window=3 --method=lagrangia --method=linear
lagrangia: x: 0.500; y: 0.719
lagrangia: x: 1.000; y: 0.625
lagrangia: x: 1.500; y: 1.719
linear: x: 0.500; y: 2.500
linear: x: 1.000; y: 3.000
linear: x: 1.500; y: 3.500
lagrangia: x: 2.500; y: 9.250
lagrangia: x: 3.000; y: 14.500
lagrangia: x: 3.500; y: 19.750
linear: x: 2.500; y: 9.250
linear: x: 3.000; y: 14.500
linear: x: 3.500; y: 19.750
```
