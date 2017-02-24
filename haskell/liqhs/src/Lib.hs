module Lib
    ( someFunc
    ) where

{-@
    type NotRootPort = {p : Int | 1024 <= p && p <= 65535}
@-}

someFunc :: IO ()
someFunc = putStrLn "someFunc"
