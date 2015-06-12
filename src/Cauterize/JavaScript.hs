{-# LANGUAGE OverloadedStrings #-}
module Cauterize.JavaScript
  ( caut2js
  ) where

import Cauterize.JavaScript.Options

import Control.Monad
import System.Directory
import System.FilePath.Posix

import Cauterize.JavaScript.Files
import Cauterize.JavaScript.TestClientTemplate
import Cauterize.JavaScript.LibTemplate
import qualified Data.ByteString as B
import qualified Cauterize.Specification as Spec
import qualified Data.Text.Lazy as T

caut2js :: CautJSOpts -> IO ()
caut2js (CautJSOpts { specFile = specPath, outputDirectory = outPath }) = createGuard outPath $ do
  spec <- Spec.parseFile specPath

  case spec of
    Left es -> error $ show es
    Right s' -> generateOutput s' outPath

generateOutput :: Spec.Spec -> FilePath -> IO ()
generateOutput spec out = do
  createDirIfNotExist $ out `combine` "libcaut"
  createDirIfNotExist $ out `combine` "libcaut" `combine` "prototypes"

  copyFiles
  renderFiles
  where
    specName = T.unpack . Spec.specName $ spec
    libName = "lib" ++ specName
    libFileName = libName ++ ".js"

    renderFiles = do
      -- lib_tmpl <- getDataFileName "templates/lib_tmpl.js"

      writeFile (out `combine` "test_client.js") (testClientFromSpec spec)
      writeFile (out `combine` libFileName) (libFromSpec spec)
      -- renderTo spec lib_tmpl (out `combine` libFileName)

    copyFiles = forM_ allFiles $ \(path, bs) -> B.writeFile (out `combine` path) bs

createGuard :: FilePath -> IO () -> IO ()
createGuard out go = createDirIfNotExist out >> go

createDirIfNotExist :: FilePath -> IO ()
createDirIfNotExist out = do
  fe <- doesFileExist out
  de <- doesDirectoryExist out

  if fe
    then error $ "Error: " ++ out ++ " is a file."
    else unless de $ createDirectory out
