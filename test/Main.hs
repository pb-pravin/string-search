{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString         as BS
import qualified Data.Text.IO            as TIO

import qualified Search.Naive.ByteString as ByteString
import qualified Search.Naive.String     as String
import qualified Search.Naive.Text       as Text

main :: IO ()
main = do
        s  <- readFile "texts/huckfinn.txt"
        bs <- BS.readFile "texts/huckfinn.txt"
        t  <- TIO.readFile "texts/huckfinn.txt"

        let sres = String.find s "Huck Finn"
        putStrLn "====String===="
        putStrLn $ "Count:   " ++ show (length sres)
        putStrLn $ "Entries: " ++ show sres

        let bres = ByteString.find bs "Huck Finn"
        putStrLn "====ByteString===="
        putStrLn $ "Count:   " ++ show (length bres)
        putStrLn $ "Entries: " ++ show bres

        let tres = Text.find t "Huck Finn"
        putStrLn "====Text===="
        putStrLn $ "Count:   " ++ show (length tres)
        putStrLn $ "Entries: " ++ show tres
