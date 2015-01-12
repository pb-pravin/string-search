{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Criterion
import           Criterion.Main
import qualified Data.ByteString         as BS

import qualified Search.Naive.ByteString as ByteString
import qualified Search.Naive.String     as String

main :: IO ()
main = defaultMain
        [ env (createEnv "texts/tomsawyer.txt") $ \(s, bs) ->
            bgroup "Tom Sayer (412k)"
                [ bench "String"     $ whnf (\p -> String.find s p) "Hello"
                , bench "ByteString" $ whnf (\p -> ByteString.find bs p) "Hello"
                ]
        , env (createEnv "texts/huckfinn.txt") $ \(s, bs) ->
            bgroup "Huck Finn (596k)"
                [ bench "String"     $ whnf (\p -> String.find s p) "Hello"
                , bench "ByteString" $ whnf (\p -> ByteString.find bs p) "Hello"
                ]
        ]

createEnv :: String -> IO (String, BS.ByteString)
createEnv filename = do
        s  <- readFile filename
        bs <- BS.readFile filename
        return (s, bs)
