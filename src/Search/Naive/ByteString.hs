module Search.Naive.ByteString
    ( find
    , findOne
    , contains
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

type Corpus  = ByteString
type Pattern = ByteString

find :: Corpus -> Pattern -> [Int]
find c p = find' c p 0 []
    where find' c p i is | BS.length c > 0 = if (BS.take (BS.length p) c) == p
                                                then find' (BS.tail c) p (i + 1) (i:is)
                                                else find' (BS.tail c) p (i + 1) is
                         | otherwise       = reverse is

findOne :: Corpus -> Pattern -> Maybe Int
findOne c p = case take 1 (find c p) of []    -> Nothing
                                        (i:_) -> Just i

contains :: Corpus -> Pattern -> Bool
contains c p = case findOne c p of Nothing -> False
                                   Just  _ -> True
