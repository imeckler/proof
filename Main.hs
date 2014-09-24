{-# LANGUAGE LambdaCase, NamedFieldPuns, OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Prelude hiding (FilePath)
import Data.String (fromString)
import Data.Maybe (fromMaybe)
import Parse
import Text.Parsec.String
import Compile
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.Catch
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative hiding (Parser)
import Text.LaTeX.Base.Parser
import TranslateTex (translate)
import DecoratedTex (decorate)
import Filesystem as F
import Filesystem.Path.CurrentOS
import Types
import Paths_proof (getDataFileName)

data AppData = AppData 
  { inputPath :: FilePath
  , outputDir :: Maybe FilePath
  }

data Format = Proof | Tex deriving (Show)

opts :: ParserInfo AppData
opts = info (helper <*> optParser)
  ( fullDesc
  <> progDesc "Compile FILEPATH to html"
  <> header "proof - a markup language for structured mathematics")
  where
  optParser = AppData
    <$> argument path (metavar "FILE" <> help "Path to the input proof file")
    <*> optional (option path (
          long "output" <>
          short 'o' <>
          metavar "OUTPUTDIR" <>
          help "Path of output directory. Default is '.'"))
    where
    path :: Monad m => String -> m FilePath
    path = liftM fromString . str

loadResources :: IO Resources
loadResources =
  Resources <$> mapM readDataFile ["src/css/proof.css"]
            <*> mapM readDataFile ["lib/js/jquery.min.js", "src/js/proof.js"]
  where readDataFile = getDataFileName >=> T.readFile

fileFormat :: Monad m => FilePath -> Err m Format
fileFormat s = case fromMaybe "no extension" (extension s) of 
  "proof" -> return Proof
  "tex"   -> return Tex
  e       -> throwError (T.unpack e)

outputPath :: FilePath -> FilePath
outputPath p = replaceExtension p "html"

proofReader :: MonadIO m => FilePath -> Err m RawDocument
proofReader = ExceptT . liftIO . fmap (left show) . parseFromFile document . encodeString

texReader :: (MonadIO m, Functor m) => FilePath -> Err m RawDocument
texReader = (ExceptT . liftIO . fmap (left show) . parseLaTeXFile . encodeString) >=> decorate >=> translate

-- `copyDirectory src dst` works as follows:
-- Let src = initialpath/dir. Then src gets moved to dst/dir assuming src
-- and dst are both directories
copyDirectory :: (MonadIO m, MonadCatch m) => FilePath -> FilePath -> Err m ()
copyDirectory = \src dst -> do
  e <- liftIO ((&&) <$> isDirectory src <*> isDirectory dst)
  if e then go src dst else throwError "Directories do not exist" -- TODO: Better error
  where
  leafName     = last . splitDirectories
  copyError p  = throwError ("Error copying file \"" ++ show p ++ "\"")

  go :: (MonadIO m, MonadCatch m) => FilePath -> FilePath -> Err m ()
  go src dst = do
    catch (liftIO $ createDirectory False dst')
      (\(_::IOError) -> throwError ("Directory \"" ++ show dst' ++ "\" already exists."))
    fs <- liftIO $ listDirectory src
    forM_ fs $ \p ->
      liftIO (isFile p) >>= \case
        True  -> liftIO (copyFile p (dst' </> filename p)) `catch` (\(_::IOError) -> copyError p)
        False -> go p dst'

    where dst' = dst </> leafName src

compileAndOutput :: (MonadIO m, MonadCatch m, Applicative m) => FilePath -> FilePath -> Err m ()
compileAndOutput inputPath outputDir =
  fileFormat inputPath >>= \case
    Proof -> go proofReader
    Tex   -> go texReader
  where

  go reader = do
    let p        = outputDir </> addExtension (basename inputPath) "proofpkg"
        htmlPath = p </> "index.html"
    html <- join (compile <$> liftIO loadResources <*> reader inputPath)
    -- TODO: Dangerous to remove directories
    liftIO $ removeTree p
    liftIO $ createDirectory False p
    liftIO (getDataFileName "lib") >>= \l -> copyDirectory (decodeString l) p
    liftIO $ T.writeFile (encodeString htmlPath) html

main :: IO ()
main = do
  AppData { inputPath, outputDir } <- execParser opts
  let out = fromMaybe (directory inputPath) outputDir
  runExceptT (compileAndOutput inputPath out) >>= \case
    Left e -> putStrLn e
    Right _ -> return ()

