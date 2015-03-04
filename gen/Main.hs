{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main
  ( main
  ) where

import Cauterize.JavaScript

import Data.Data
import Options.Applicative
import System.Directory
import System.FilePath.Posix
import Text.Hastache
import Text.Hastache.Context

import qualified Cauterize.Specification as Spec
import qualified Cauterize.Meta as Meta
import qualified Data.Text.Lazy as T
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

main :: IO ()
main = runWithOptions caut2js

runWithOptions :: (CautJSOpts -> IO ()) -> IO ()
runWithOptions fn = execParser options >>= fn

caut2js :: CautJSOpts -> IO ()
caut2js (CautJSOpts { specFile = specPath, metaFile = metaPath, outputDirectory = outPath }) = createGuard outPath $ do
  spec <- Spec.parseFile specPath
  meta <- Meta.parseFile metaPath

  case spec of
    Left es -> error $ show es
    Right s' ->
      case meta of
        Left em -> error $ show em
        Right m' -> generateOutput s' m' outPath

generateOutput spec meta out = do
  copyFiles
  renderFiles
  where
    specName = T.unpack . Spec.specName $ spec
    libName = "lib" ++ specName
    libFileName = libName ++ ".js"

    renderFiles = do
      tc_tmpl <- getDataFileName "templates/test_client_tmpl.js"
      lib_tmpl <- getDataFileName "templates/lib_tmpl.js"

      renderTo spec meta tc_tmpl (out `combine` "test_client.js")
      renderTo spec meta lib_tmpl (out `combine` libFileName)

    copyFiles = do
      ca <- getDataFileName "support/cauterize.js"
      md <- getDataFileName "support/meta_decoder.js"
      copyFile ca (out `combine` "cauterize.js")
      copyFile md (out `combine` "meta_decoder.js")

createGuard :: FilePath -> IO () -> IO ()
createGuard out go = do
  fe <- doesFileExist out
  de <- doesDirectoryExist out

  if fe
    then error $ "Error: " ++ out ++ " is a file."
    else if de
          then go
          else createDirectory out >> go

data JSCtx = JSCtx
  { jscLibName :: T.Text
  , jscMeta :: JSMetaCtx
  } deriving (Show, Eq, Data, Typeable)

data JSMetaCtx = JSMetaCtx
  { jsmLengthWidth :: Integer
  , jsmTypeWidth :: Integer
  } deriving (Show, Eq, Data, Typeable)

mkJsCtx :: Spec.Spec -> Meta.Meta -> JSCtx
mkJsCtx spec meta = JSCtx
  { jscLibName = Spec.specName spec
  , jscMeta =
      JSMetaCtx
        { jsmLengthWidth = Meta.metaDataLength meta
        , jsmTypeWidth = Meta.metaTypeLength meta
        }
  }

renderTo :: Spec.Spec -> Meta.Meta -> FilePath -> FilePath -> IO ()
renderTo spec meta templatePath destPath = do
  template <- T.readFile templatePath
  cfg <- mkCfg
  rendered <- hastacheStr cfg (encodeStr $ T.unpack template)
                              (mkGenericContext $ mkJsCtx spec meta)
  T.writeFile destPath rendered
  where
    mkCfg = do
      tpath <- getDataFileName "templates/"
      return $ defaultConfig { muEscapeFunc = id
                             , muTemplateFileDir = Just tpath } :: IO (MuConfig IO)

