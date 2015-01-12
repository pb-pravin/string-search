module Search.Naive.ByteString
    ( find
    , findOne
    , contains
    ) where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import           Data.Word

type Corpus  = ByteString
type Pattern = ByteString

find :: Corpus -> Pattern -> [Int]
find c p = let (_, _, _, is) = BS.foldl' match (0, 0, Nothing, []) c
            in reverse is
    where pl = BS.length p - 1
          match :: (Int, Int, Maybe Int, [Int]) -> Word8 -> (Int, Int, Maybe Int, [Int])
          match (ci, pi, Nothing, is) cv
            | BS.index p pi == cv = case compare pi pl of
                                        LT -> (ci + 1, pi + 1, Just ci, is)
                                        EQ -> undefined
                                        GT -> undefined
            | otherwise           = (ci + 1, 0, Nothing, is)
          match (ci, pi,  Just i, is) cv
            | BS.index p pi == cv = case compare pi pl of
                                        LT -> (ci + 1, pi + 1,  Just i,   is)
                                        EQ -> (ci + 1,      0, Nothing, i:is)
                                        GT -> undefined
            | otherwise           = (ci + 1, 0, Nothing, is)

findOne :: Corpus -> Pattern -> Maybe Int
findOne c p = case take 1 (find c p) of []    -> Nothing
                                        (i:_) -> Just i

contains :: Corpus -> Pattern -> Bool
contains c p = case findOne c p of Nothing -> False
                                   Just  _ -> True
