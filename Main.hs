{-# LANGUAGE LambdaCase, NamedFieldPuns, OverloadedStrings #-}
module Main where

import Parse

import Text.Parsec.String

import Compile
import Control.Monad
import Control.Monad.Except

import qualified Data.Text.IO as T

import Options.Applicative hiding (Parser)
import qualified Options.Applicative as O

import Paths_proof (getDataFileName)

data AppData = AppData 
  { filePath :: FilePath
  }

optParser :: O.Parser AppData
optParser = AppData
  <$> argument str (metavar "FILE" <> help "Path to the input proof file")

opts :: ParserInfo AppData
opts = info (helper <*> optParser)
  ( fullDesc
  <> progDesc "Compile FILEPATH to html"
  <> header "proof - a markup language for structured mathematics")

loadResources :: IO Resources
loadResources =
  Resources <$> mapM readFile' ["src/css/proof.css"]
            <*> mapM readFile' ["lib/js/jquery.min.js", "src/js/proof.js"]
  where readFile' = getDataFileName >=> T.readFile

main :: IO ()
main = do
  AppData { filePath } <- execParser opts
  parseFromFile document filePath >>= \case
    Left err  -> print err
    Right doc -> do
      res <- loadResources
      case runExcept (compile res doc) of
        Left err -> putStrLn err
        Right docTxt -> T.putStrLn docTxt

