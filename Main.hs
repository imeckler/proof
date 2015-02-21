{-# LANGUAGE LambdaCase, NamedFieldPuns, OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Prelude hiding (FilePath)
import Data.String (fromString)
import Data.Maybe (fromMaybe)
import Compile
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.Catch
import Control.Concurrent (threadDelay)
import qualified Data.Text.IO as T
import Options.Applicative hiding (Parser)
import Text.LaTeX.Base.Parser
import TranslateTex (translate)
import DecoratedTex (decorate)
import Filesystem as F
import Filesystem.Path.CurrentOS
import System.FSNotify
import Types
import Paths_proof (getDataFileName)

data AppData = AppData 
  { inputPath :: FilePath
  , outputDir :: Maybe FilePath
  , watch     :: Bool
  }

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
    <*> switch (long "watch" <> short 'w' <> help "Recompile on file-change")
    where
    path :: ReadM FilePath
    path = eitherReader (Right . fromString)

outputPath :: FilePath -> FilePath
outputPath p = replaceExtension p "html"

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

loadResources :: (MonadIO m, MonadCatch m, Applicative m) => Err m Resources
loadResources =
  Resources <$> mapM readDataFile ["src/css/proof.css"]
            <*> mapM readDataFile ["lib/js/jquery.min.js", "src/js/proof.js"]
  where
  readDataFile = (`catch` (\(_::IOError) ->throwError "Could not read data files"))
               . liftIO . (T.readFile <=< getDataFileName)

pkgPath :: FilePath -> FilePath -> FilePath
pkgPath inputPath outputDir = outputDir </> addExtension (basename inputPath) "proofpkg"

compileAndOutput :: (MonadIO m, MonadCatch m, Applicative m) => FilePath -> FilePath -> Err m ()
compileAndOutput inputPath outputDir = do
  let p        = pkgPath inputPath outputDir
      htmlPath = p </> "index.html"
  html <- join (compile <$> loadResources <*> texReader inputPath)
  -- TODO: Dangerous to remove directories. Remove when you merge

  liftIO $ do
    isDirectory p >>= flip when (removeTree p)
    createDirectory False p

  liftIO (getDataFilePath "lib") >>= \l -> copyDirectory l p
  liftIO $ do
    forM_ ["src/js/proof.js", "src/css/proof.css"] $ \q ->
      getDataFilePath q >>= \q' -> copyFile q' (p </> filename q)
    T.writeFile (encodeString htmlPath) html
  where
  getDataFilePath = fmap decodeString . getDataFileName . encodeString

main :: IO ()
main = do
  AppData { inputPath, outputDir, watch } <- execParser opts
  run inputPath outputDir
  when watch (setupWatch inputPath outputDir)
  where
  setupWatch inputPath outputDir =
    withManager $ \wm -> void $ do
      watchDir wm (directory inputPath) fEvent $ \_ -> do
        putStr "File changed. Recompiling..."
        run inputPath outputDir
        putStrLn "Done."
      forever $ threadDelay maxBound
    where
    fEvent = \case { Modified p _ -> p == inputPath; _ -> False }

  run inputPath outputDir =
    runExceptT (compileAndOutput inputPath out) >>= \case
      Left e  -> putStrLn e
      Right _ -> return () 
    where out = fromMaybe (directory inputPath) outputDir

