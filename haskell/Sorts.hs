module Sorts where

msort :: Ord a => [a] -> [a]
msort = mergeAll . sequences
  where
    sequences (a:b:xs)
      | a > b     = descending b [a]  xs
      | otherwise = ascending  b (a:) xs
    sequences xs  = [xs]

    descending a as bs@(b:bs')
      | a > b          = descending b (a:as) bs'
    descending a as bs = (a:as): sequences bs

    ascending a as bs@(b:bs')
      | a <= b        = ascending b (\ys -> as (a:ys)) bs'
    ascending a as bs = as [a]: sequences bs

    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = merge a b: mergePairs xs
    mergePairs xs       = xs

    merge as@(a:as') bs@(b:bs')
      | a > b     = b:merge as  bs'
      | otherwise = a:merge as' bs
    merge [] bs   = bs
    merge as []   = as

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort (filter (<=x) xs) ++ [x] ++ qsort (filter (>x) xs)

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs) where
  insert x []     = [x]
  insert x ys@(y:ys') | x > y     = y : insert x ys'
                      | otherwise = x : ys

alternateList n = concat $ zipWith (\x y -> [x,y]) [1..(div n 2)] [(div n 2 + 1)..n]
