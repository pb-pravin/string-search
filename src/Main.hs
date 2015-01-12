module Main where

import           Criterion
import           Criterion.Main

import           Search.Naive.String

main :: IO ()
main = defaultMain
        [ env (readFile "texts/tomsawyer.txt") $ \f ->
            bgroup "Tom Sayer (412k)"
                [ bench "String" $ whnf (\p -> find f p) "Hello"
                ]
        , env (readFile "texts/huckfinn.txt") $ \f ->
            bgroup "Huck Finn (596k)"
                [ bench "String" $ whnf (\p -> find f p) "Hello"
                ]
        ]
