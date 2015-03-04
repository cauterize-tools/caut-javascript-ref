{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main
  ( main
  ) where

import Cauterize.JavaScript

import Data.Data
import Data.Word
import Options.Applicative
import System.Directory
import System.FilePath.Posix
import Text.Hastache
import Text.Hastache.Context

import qualified Cauterize.Common.Types as C
import qualified Cauterize.FormHash as H
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

generateOutput :: Spec.Spec -> Meta.Meta -> FilePath -> IO ()
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
  , jscTypes :: [JSTypeCtx]
  } deriving (Show, Eq, Data, Typeable)

data JSMetaCtx = JSMetaCtx
  { jsmLengthWidth :: Integer
  , jsmTypeWidth :: Integer
  } deriving (Show, Eq, Data, Typeable)

data JSTypeInfo = JSTypeInfo
  { jstPrototype :: T.Text, jstName :: T.Text, jstHash :: [Word8], jstSize :: JSTSizeCtx
  } deriving (Show, Eq, Data, Typeable)

data JSBIClassification
  = JSBIUnsigned
  | JSBISigned
  | JSBIFloat
  | JSBIBool
  deriving (Show, Eq, Data, Typeable)

data JSTypeCtx
  = JSTBuiltIn
      { jstDetail :: JSTypeInfo
      , builtinWidth :: Integer
      , builtinClassification :: JSBIClassification
      }
  | JSTSynonym { jstDetail :: JSTypeInfo }
  | JSTArray { jstDetail :: JSTypeInfo }
  | JSTVector { jstDetail :: JSTypeInfo }
  | JSTRecord { jstDetail :: JSTypeInfo }
  | JSTCombination { jstDetail :: JSTypeInfo }
  | JSTUnion { jstDetail :: JSTypeInfo }
  deriving (Show, Eq, Data, Typeable)

data JSTSizeCtx = JSTSizeCtx { jstMinSize :: Integer, jstMaxSize :: Integer }
  deriving (Show, Eq, Data, Typeable)

mkJsCtx :: Spec.Spec -> Meta.Meta -> JSCtx
mkJsCtx spec meta = JSCtx
  { jscLibName = Spec.specName spec
  , jscMeta =
      JSMetaCtx
        { jsmLengthWidth = Meta.metaDataLength meta
        , jsmTypeWidth = Meta.metaTypeLength meta
        }
  , jscTypes = map mkJsType (Spec.specTypes spec)
  }

mkJsType :: Spec.SpType -> JSTypeCtx
mkJsType t =
  case t of
    Spec.BuiltIn { Spec.unBuiltIn = b }
      -> JSTBuiltIn { jstDetail = mkTypeInfo "builtin"
                    , builtinWidth = Spec.maxSize t
                    , builtinClassification = classify b
                    }
    Spec.Synonym {}
      -> JSTSynonym { jstDetail = mkTypeInfo "synonym" }
    Spec.Array {}
      -> JSTArray { jstDetail = mkTypeInfo "array" }
    Spec.Vector {}
      -> JSTVector { jstDetail = mkTypeInfo "vector" }
    Spec.Record {}
      -> JSTRecord { jstDetail = mkTypeInfo "record" }
    Spec.Combination {}
      -> JSTCombination { jstDetail = mkTypeInfo "combination" }
    Spec.Union {}
      -> JSTUnion { jstDetail = mkTypeInfo "union" }
  where
    mkTypeInfo p =
      JSTypeInfo
        { jstName = Spec.typeName t
        , jstHash = H.hashToBytes . Spec.spHash $ t
        , jstSize = JSTSizeCtx { jstMinSize = Spec.minSize t, jstMaxSize = Spec.maxSize t }
        , jstPrototype = p
        }

    classify (C.TBuiltIn C.BIu8)  = JSBIUnsigned
    classify (C.TBuiltIn C.BIu16) = JSBIUnsigned
    classify (C.TBuiltIn C.BIu32) = JSBIUnsigned
    classify (C.TBuiltIn C.BIu64) = JSBIUnsigned
    classify (C.TBuiltIn C.BIcu8)  = JSBIUnsigned
    classify (C.TBuiltIn C.BIcu16) = JSBIUnsigned
    classify (C.TBuiltIn C.BIcu32) = JSBIUnsigned
    classify (C.TBuiltIn C.BIs8)  = JSBISigned
    classify (C.TBuiltIn C.BIs16) = JSBISigned
    classify (C.TBuiltIn C.BIs32) = JSBISigned
    classify (C.TBuiltIn C.BIs64) = JSBISigned
    classify (C.TBuiltIn C.BIf32) = JSBIFloat
    classify (C.TBuiltIn C.BIf64) = JSBIFloat
    classify (C.TBuiltIn C.BIbool) = JSBIBool

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

