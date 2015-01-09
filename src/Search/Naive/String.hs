module Search.Naive.String
    ( find
    , findOne
    , contains
    ) where

import           Data.Int

type Corpus  = String
type Pattern = String

find :: Corpus -> Pattern -> [Int64]
find = find' 0 []
    where find' _ is [] _ = reverse is
          find' n is c  p = if match c p then find' (n + 1) (n:is) (tail c) p
                                         else find' (n + 1) is     (tail c) p
          match (c:cs) (p:ps) | p == c    = match cs ps
                              | otherwise = False
          match _      _                  = True

findOne :: Corpus -> Pattern -> Maybe Int64
findOne c p = case take 1 (find c p) of []    -> Nothing
                                        (i:_) -> Just i

contains :: Corpus -> Pattern -> Bool
contains c p = case findOne c p of Nothing -> False
                                   Just  _ -> True
