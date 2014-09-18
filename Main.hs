{-# LANGUAGE LambdaCase #-}
module Main where

import Parse

import System.Environment
import Text.Parsec.String

import Compile
import Control.Monad.Except

import qualified Data.Text.IO as T

main :: IO ()
main = do
  (path:_) <- getArgs
  parseFromFile document path >>= \case
    Left err  -> print err
    Right doc -> do
      case runExcept (compile doc) of
        Left err -> putStrLn err
        Right docTxt -> T.putStrLn docTxt

