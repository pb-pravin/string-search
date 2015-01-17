{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString         as BS

import qualified Search.Naive.ByteString as ByteString
import qualified Search.Naive.String     as String

main :: IO ()
main = do
        s  <- readFile "texts/huckfinn.txt"
        bs <- BS.readFile "texts/huckfinn.txt"

        let sres = String.find s "Huck Finn"
        putStrLn $ "Count:   " ++ show (length sres)
        putStrLn $ "Entries: " ++ show sres

        let bres = ByteString.find bs "Huck Finn"
        putStrLn $ "Count:   " ++ show (length bres)
        putStrLn $ "Entries: " ++ show bres
