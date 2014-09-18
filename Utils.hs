module Utils where

import Data.Char

stripRight :: String -> String
stripRight = reverse . dropWhile isSpace . reverse
