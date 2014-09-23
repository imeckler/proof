{-# LANGUAGE LambdaCase, NamedFieldPuns, OverloadedStrings #-}
module Main where

import Parse
import Text.Parsec.String
import Compile
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import qualified Data.Text.IO as T
import Options.Applicative hiding (Parser)
import qualified Options.Applicative as O
import Text.LaTeX.Base.Parser
import TranslateTex (translate)
import DecoratedTex (decorate)
import Paths_proof (getDataFileName)
import Types

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

data Format = Proof | Tex deriving (Show)

fileFormat s = case ext s of { "proof" -> Right Proof; "tex" -> Right Tex; e -> Left e }
  where ext = reverse . takeWhile (/= '.') . reverse

outputPath     = (++ ".html") . stripExtension
stripExtension = reverse . tail' . dropWhile (/= '.') . reverse
  where tail' = \case {[] -> []; (_:xs) -> xs}

main' reader filePath = do
  fmap runExcept (reader filePath) >>= \case
    Left err -> putStrLn err
    Right doc -> do
      res <- loadResources
      case runExcept (compile res doc) of
        Left err     -> putStrLn err
        Right docTxt -> T.writeFile (outputPath filePath) docTxt

proofReader :: FilePath -> IO (Except String RawDocument)
proofReader = fmap (ExceptT . pure . left show)  . parseFromFile document

texReader :: FilePath -> IO (Except String RawDocument)
texReader = 
  fmap (ExceptT . pure . left show >=> decorate >=> translate) . parseLaTeXFile

main :: IO ()
main = do
  AppData { filePath } <- execParser opts
  case fileFormat filePath of
    Right Proof -> main' proofReader filePath
    Right Tex   -> main' texReader filePath
    Left  e     -> putStrLn $ "Unknown file type: " ++ e

