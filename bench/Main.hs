{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Criterion
import           Criterion.Main
import qualified Data.ByteString         as BS

import qualified Search.Naive.ByteString as ByteString
import qualified Search.Naive.String     as String

main :: IO ()
main = defaultMain
        [ env (createEnv "texts/huckfinn.txt") $ \(s, _) ->
            bgroup "String"
                [ bench "find"     $ whnf (String.find s) "Huck Finn"
                , bench "findOne"  $ whnf (String.findOne s) "Huck Finn"
                , bench "contains" $ whnf (String.contains s) "Huck Finn"
                ]
        , env (createEnv "texts/huckfinn.txt") $ \(_, bs) ->
            bgroup "ByteString"
                [ bench "find"     $ whnf (ByteString.find bs) "Huck Finn"
                , bench "findOne"  $ whnf (ByteString.findOne bs) "Huck Finn"
                , bench "contains" $ whnf (ByteString.contains bs) "Huck Finn"
                ]
        ]

createEnv :: String -> IO (String, BS.ByteString)
createEnv filename = do
        s  <- readFile filename
        bs <- BS.readFile filename
        return (s, bs)
