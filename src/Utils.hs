module Utils where

import           Types
import           Data.Maybe (listToMaybe)
import           Data.List (group, sort)
import           Text.Regex.TDFA ((=~))

--------------------------------------------------------------------------------

frequency :: Ord a => [a] -> [(Int,a)]
frequency list = map (\l -> (length l, head l)) (group (sort list))

--------------------------------------------------------------------------------

maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

--------------------------------------------------------------------------------

nchars :: (Show a) => a -> Int
nchars = length . show

--------------------------------------------------------------------------------

matches :: Regexp -> String -> Bool
matches (Regexp r) x = x =~ r

--------------------------------------------------------------------------------

ifNotEmpty :: ([a] -> b) -> b -> [a] -> b
ifNotEmpty _ fallback [] = fallback
ifNotEmpty f _        xs = f xs
