module Search.Naive.Text
    ( find
    , findOne
    , contains
    ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude   hiding (pi)

type Corpus  = Text
type Pattern = Text

find :: Corpus -> Pattern -> [Int]
find c p = let (_, _, _, is) = T.foldl' match (0, 0, Nothing, []) c
            in reverse is
    where pl = T.length p - 1
          match (ci, pi, Nothing, is) cv
            | T.index p pi == cv = case compare pi pl of
                                    LT -> (ci + 1, pi + 1, Just ci, is)
                                    EQ -> undefined
                                    GT -> undefined
            | otherwise         = (ci + 1, 0, Nothing, is)
          match (ci, pi, Just i, is) cv
            | T.index p pi == cv = case compare pi pl of
                                    LT -> (ci + 1, pi + 1,  Just i,   is)
                                    EQ -> (ci + 1,      0, Nothing, i:is)
                                    GT -> undefined
            | otherwise          = (ci + 1, 0, Nothing, is)

findOne :: Corpus -> Pattern -> Maybe Int
findOne c p = case take 1 (find c p) of []    -> Nothing
                                        (i:_) -> Just i

contains :: Corpus -> Pattern -> Bool
contains c p = case findOne c p of Nothing -> False
                                   Just  _ -> True
