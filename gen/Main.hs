{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main
  ( main
  ) where

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
      bi <- getDataFileName "support/builtin_lib.js"
      copyFile ca (out `combine` "cauterize.js")
      copyFile bi (out `combine` "builtin_lib.js")

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
  { jstPrototype :: T.Text
  , jstConstructor :: T.Text
  , jstName :: T.Text
  , jstHash :: [Word8]
  , jstSize :: JSTSizeCtx
  } deriving (Show, Eq, Data, Typeable)

data JSBuiltIn
  = JSU8 | JSU16 | JSU32 | JSU64
  | JSS8 | JSS16 | JSS32 | JSS64
  | JSF32 | JSF64
  | JSCu8 | JSCu16 | JSCu32
  | JSBool
  deriving (Show, Eq, Data, Typeable)

data JSTypeCtx
  = JSTBuiltIn { jstDetail :: JSTypeInfo, jstBIInstance :: JSBuiltIn }
  | JSTSynonym { jstDetail :: JSTypeInfo, jstSynnedCtor :: T.Text }
  | JSTArray { jstDetail :: JSTypeInfo, jstArrayRefCtor :: T.Text, jstArrayLen :: Integer }
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
    Spec.BuiltIn { Spec.unBuiltIn = (C.TBuiltIn b) }
      -> JSTBuiltIn { jstDetail = mkTypeInfo "builtin"
                    , jstBIInstance = biConv b }
    Spec.Synonym { Spec.unSynonym = (C.TSynonym { C.synonymRepr = r } ) }
      -> JSTSynonym { jstDetail = mkTypeInfo "synonym"
                    , jstSynnedCtor = nameToConstructor (T.pack . show $ r)}
    Spec.Array { Spec.unArray = (C.TArray { C.arrayRef = ar, C.arrayLen = al }) }
      -> JSTArray { jstDetail = mkTypeInfo "array"
                  , jstArrayRefCtor = nameToConstructor ar
                  , jstArrayLen = al }
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
        , jstConstructor = nameToConstructor $ Spec.typeName t
        }

    biConv C.BIu8  = JSU8
    biConv C.BIu16 = JSU16
    biConv C.BIu32 = JSU32
    biConv C.BIu64 = JSU64
    biConv C.BIcu8 = JSCu8
    biConv C.BIcu16 = JSCu16
    biConv C.BIcu32 = JSCu32
    biConv C.BIs8 = JSS8
    biConv C.BIs16 = JSS16
    biConv C.BIs32 = JSS32
    biConv C.BIs64 = JSS64
    biConv C.BIf32 = JSF32
    biConv C.BIf64 = JSF64
    biConv C.BIbool = JSBool

nameToConstructor :: T.Text -> T.Text
nameToConstructor n =
  let parts = T.splitOn "_" n
      caped = map T.toTitle parts
  in T.concat caped

renderTo :: Spec.Spec -> Meta.Meta -> FilePath -> FilePath -> IO ()
renderTo spec meta templatePath destPath = do
  template <- T.readFile templatePath
  cfg <- mkCfg
  rendered <- hastacheStr cfg (encodeStr $ T.unpack template)
                              (mkGenericContext $ mkJsCtx spec meta)
  T.writeFile destPath rendered
  where
    mkCfg = do
      tpath <- getDataFileName "templates/sub"
      return $ defaultConfig { muEscapeFunc = id
                             , muTemplateFileDir = Just tpath } :: IO (MuConfig IO)

