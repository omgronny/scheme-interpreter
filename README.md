# Интерпретатор языка scheme на haskell

## Build and run

```bash
cabal build
cabal run scheme-interpreter-exe --verbose=0
```

## Scheme: Примеры вычислений

```
> (+ 1 2 3)
6
```

```
> (+ 1 (+ 3 4 5))
13
```

```
> (and (= 2 2) (> 2 1))
#t
```

```
> (and (= 2 2) (> 2 1))
#t
```

```
> (define x 3)
> x
3
```

