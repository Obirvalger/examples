module Main where

import Lib

{-@ type Pos  = {x : Int | x > 0 && x < 5} @-}

{-@ n :: Pos @-}
n :: Int
n = 3

{-@ localPort :: NotRootPort @-}
localPort :: Int
localPort = 4330

main :: IO ()
main = print (n, localPort)
