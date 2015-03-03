module Main
  ( main
  ) where

import Cauterize.JavaScript

import Options.Applicative

import System.FilePath.Posix
import System.Directory

import qualified Cauterize.Specification as Spec
import qualified Cauterize.Meta as Meta
import qualified Data.Text.Lazy.IO as T

import Paths_caut_javascript_ref

data CautJSOpts = CautJSOpts
  { specFile :: FilePath
  , metaFile :: FilePath
  , outputDirectory :: FilePath
  } deriving (Show)

optParser :: Parser CautJSOpts
optParser = CautJSOpts
  <$> strOption
    ( long "spec"
   <> metavar "SPEC_PATH"
   <> help "Cauterize specification file"
    )
  <*> strOption
    ( long "meta"
   <> metavar "META_PATH"
   <> help "Cauterize meta file"
    )
  <*> strOption
    ( long "output"
   <> metavar "OUTPUT_PATH"
   <> help "Directory to save output files"
    )

options :: ParserInfo CautJSOpts
options = info (optParser <**> helper)
            ( fullDesc
           <> progDesc "Translate a Cauterize specification and meta file into a JavaScript library."
            )

runWithOptions :: (CautJSOpts -> IO ()) -> IO ()
runWithOptions fn = execParser options >>= fn

main :: IO ()
main = runWithOptions caut2js

caut2js :: CautJSOpts -> IO ()
caut2js (CautJSOpts { specFile = spec, metaFile = meta, outputDirectory = out }) = do
  copyFiles
  renderFiles
  where
    copyFiles = return ()
    renderFiles = return ()
