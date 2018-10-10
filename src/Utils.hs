module Utils where

import           Data.Maybe (listToMaybe)
import           Data.List (group, sort)

--------------------------------------------------------------------------------

frequency :: Ord a => [a] -> [(Int,a)]
frequency list = map (\l -> (length l, head l)) (group (sort list))

--------------------------------------------------------------------------------

maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

--------------------------------------------------------------------------------

nchars :: (Show a) => a -> Int
nchars = length . show

