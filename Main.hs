{-# LANGUAGE LambdaCase #-}
module Main where

import Types
import Parse

import System.Environment
import Text.Parsec.String

main :: IO ()
main = do
  (path:_) <- getArgs
  parseFromFile document path >>= \case
    Left err -> print err
    Right doc -> print doc
