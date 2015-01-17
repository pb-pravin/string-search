{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Criterion
import           Criterion.Main
import qualified Data.ByteString         as BS
import qualified Data.Text               as T
import qualified Data.Text.IO            as TIO

import qualified Search.Naive.ByteString as ByteString
import qualified Search.Naive.String     as String
import qualified Search.Naive.Text       as Text

main :: IO ()
main = defaultMain
        [ env (createEnv "texts/huckfinn.txt") $ \(s, _, _) ->
            bgroup "String"
                [ bench "find"     $ whnf (String.find s) "Huck Finn"
                , bench "findOne"  $ whnf (String.findOne s) "Huck Finn"
                , bench "contains" $ whnf (String.contains s) "Huck Finn"
                ]
        , env (createEnv "texts/huckfinn.txt") $ \(_, bs, _) ->
            bgroup "ByteString"
                [ bench "find"     $ whnf (ByteString.find bs) "Huck Finn"
                , bench "findOne"  $ whnf (ByteString.findOne bs) "Huck Finn"
                , bench "contains" $ whnf (ByteString.contains bs) "Huck Finn"
                ]
        , env (createEnv "texts/huckfinn.txt") $ \(_, _, t) ->
            bgroup "Text"
                [ bench "find"     $ whnf (Text.find t) "Huck Finn"
                , bench "findOne"  $ whnf (Text.findOne t) "Huck Finn"
                , bench "contains" $ whnf (Text.contains t) "Huck Finn"
                ]
        ]

createEnv :: String -> IO (String, BS.ByteString, T.Text)
createEnv filename = do
        s  <- readFile filename
        bs <- BS.readFile filename
        t  <- TIO.readFile filename
        return (s, bs, t)
