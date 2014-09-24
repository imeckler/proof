module Utils where

import Data.Char

stripRight :: String -> String
stripRight = reverse . dropWhile isSpace . reverse

stripLeft :: String -> String
stripLeft = dropWhile isSpace

strip :: String -> String
strip = stripRight . stripLeft

may :: b -> Maybe a -> (a -> b) -> b
may x m f = maybe x f m
