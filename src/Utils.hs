module Utils where

import           Types
import qualified Data.Maybe      as M
import qualified Data.List       as L
import qualified Data.List.Split as S
import qualified Data.Text       as T
import           Text.Regex.TDFA ((=~))
import           Text.Regex.TDFA.Text ()

--------------------------------------------------------------------------------

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = valueIfNoRemainder =<< (M.listToMaybe . reads $ s)
  where
    valueIfNoRemainder (x, rem) | null rem  = Just x
                                | otherwise = Nothing

--------------------------------------------------------------------------------

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _   (Just x) = Right x
maybeToEither err Nothing  = Left err

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
replace needle with = L.intercalate with . S.splitOn needle

--------------------------------------------------------------------------------

tryReadVersion :: T.Text -> Maybe Version
tryReadVersion x = do
  let dotSplits = T.splitOn "." x
  if length dotSplits == 3
  then Version
    <$> maybeRead (T.unpack $ dotSplits !! 0)
    <*> maybeRead (T.unpack $ dotSplits !! 1)
    <*> maybeRead (T.unpack $ dotSplits !! 2)
    <*> pure 0
  else Nothing

--------------------------------------------------------------------------------

ignoreFirstIncrement :: [Change] -> [Change]
ignoreFirstIncrement []              = []
ignoreFirstIncrement (NoChange : xs) = xs
ignoreFirstIncrement (Fix      : xs) = xs
ignoreFirstIncrement (Feature  : xs) = xs
ignoreFirstIncrement (Breaking : xs) = xs
ignoreFirstIncrement (SetTo v  : xs) = SetTo v : xs

