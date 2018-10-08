module Utils where

import           Data.List    (group, sort)

--------------------------------------------------------------------------------

frequency :: Ord a => [a] -> [(Int,a)]
frequency list = map (\l -> (length l, head l)) (group (sort list))

--------------------------------------------------------------------------------

nchars :: (Show a) => a -> Int
nchars = length . show

