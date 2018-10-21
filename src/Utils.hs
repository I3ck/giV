module Utils where

import           Types
import           Data.Maybe (listToMaybe)
import           Data.List (group, sort, intercalate)
import           Data.List.Split (splitOn)
import qualified Data.Text as T
import           Text.Regex.TDFA ((=~))
import           Text.Regex.TDFA.Text ()

--------------------------------------------------------------------------------

frequency :: Ord a => [a] -> [(Int,a)]
frequency list = map (\l -> (length l, head l)) (group (sort list))

--------------------------------------------------------------------------------

maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

--------------------------------------------------------------------------------

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _   (Just x) = Right x
maybeToEither err Nothing  = Left err

--------------------------------------------------------------------------------

nchars :: (Show a) => a -> Int
nchars = length . show

--------------------------------------------------------------------------------

matches :: Regexp -> T.Text -> Bool
matches (Regexp r) x = x =~ r

--------------------------------------------------------------------------------

ifNotEmpty :: ([a] -> b) -> b -> [a] -> b
ifNotEmpty _ fallback [] = fallback
ifNotEmpty f _        xs = f xs

--------------------------------------------------------------------------------

indentedNewLines :: Int -> String -> String
indentedNewLines n = replace "\n" ("\n" ++ spaces)
  where
    spaces = replicate n ' '

--------------------------------------------------------------------------------

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace needle with target = intercalate with . splitOn needle $ target

