{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Cauterize.JavaScript
  ( caut2js
  ) where

import Cauterize.JavaScript.Options

import Control.Monad
import Data.Data
import Data.Word
import System.Directory
import System.FilePath.Posix
import Text.Hastache
import Text.Hastache.Context

import qualified Cauterize.Common.Types as C
import qualified Cauterize.FormHash as H
import qualified Cauterize.Specification as Spec
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Paths_caut_javascript_ref

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
      tc_tmpl <- getDataFileName "templates/test_client_tmpl.js"
      lib_tmpl <- getDataFileName "templates/lib_tmpl.js"

      renderTo spec tc_tmpl (out `combine` "test_client.js")
      renderTo spec lib_tmpl (out `combine` libFileName)

    libfiles :: [String]
    libfiles = [ "buffer.js"
               , "cast.js"
               , "cauterize.js"
               , "prototypes.js"
               , "typedict.js"
               ]

    libprotfiles :: [String]
    libprotfiles = [ "carray.js"
                   , "cbuiltin.js"
                   , "ccombination.js"
                   , "crecord.js"
                   , "csynonym.js"
                   , "ctype.js"
                   , "cunion.js"
                   , "cvector.js"
                   ]

    copyFiles = do

      forM_ libfiles $ \f ->
        do n <- getDataFileName ("support/libcaut/" ++ f)
           copyFile n (out `combine` "libcaut" `combine` f)
      forM_ libprotfiles $ \f ->
        do n <- getDataFileName ("support/libcaut/prototypes/" ++ f)
           copyFile n (out `combine` "libcaut" `combine` "prototypes" `combine` f)


    -- copyFiles = do
    --   ca <- getDataFileName "support/cauterize.js"
    --   bi <- getDataFileName "support/builtin_lib.js"
    --   cb <- getDataFileName "support/caut_buffer.js"
    --   copyFile ca (out `combine` "cauterize.js")
    --   copyFile bi (out `combine` "builtin_lib.js")
    --   copyFile cb (out `combine` "caut_buffer.js")

createGuard :: FilePath -> IO () -> IO ()
createGuard out go = createDirIfNotExist out >> go

createDirIfNotExist :: FilePath -> IO ()
createDirIfNotExist out = do
  fe <- doesFileExist out
  de <- doesDirectoryExist out

  if fe
    then error $ "Error: " ++ out ++ " is a file."
    else unless de $ createDirectory out

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

data JSTFieldInfo = JSTDataField
                    { jstdfName :: T.Text
                    , jstdfIndex :: Integer
                    , jstdfRefCtor :: T.Text }
                  | JSTEmptyField
                    { jstefName :: T.Text
                    , jstefIndex :: Integer }
  deriving (Show, Eq, Data, Typeable)

data JSTypeCtx
  = JSTBuiltIn { jstDetail :: JSTypeInfo, jstBIInstance :: JSBuiltIn }
  | JSTSynonym { jstDetail :: JSTypeInfo, jstSynnedCtor :: T.Text }
  | JSTArray { jstDetail :: JSTypeInfo, jstArrayRefCtor :: T.Text, jstArrayLen :: Integer }
  | JSTVector { jstDetail :: JSTypeInfo, jstVectorRefCtor :: T.Text, jstVectorMaxLen :: Integer, jstVectorLenWidth :: Integer  }
  | JSTRecord { jstDetail :: JSTypeInfo, jstRecordFields :: [JSTFieldInfo] }
  | JSTCombination { jstDetail :: JSTypeInfo, jstCombinationFields :: [JSTFieldInfo], jstCombinationFlagsWidth :: Integer }
  | JSTUnion { jstDetail :: JSTypeInfo, jstUnionFields :: [JSTFieldInfo], jstUnionTagWidth :: Integer }
  deriving (Show, Eq, Data, Typeable)

data JSTSizeCtx = JSTSizeCtx { jstMinSize :: Integer, jstMaxSize :: Integer }
  deriving (Show, Eq, Data, Typeable)

mkJsCtx :: Spec.Spec -> JSCtx
mkJsCtx spec = JSCtx
  { jscLibName = Spec.specName spec
  , jscMeta =
      JSMetaCtx
        { jsmLengthWidth = Spec.unLengthTagWidth . Spec.specLengthTagWidth $ spec
        , jsmTypeWidth = Spec.unTypeTagWidth . Spec.specTypeTagWidth $ spec
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
    Spec.Vector { Spec.unVector = (C.TVector { C.vectorRef = vr, C.vectorMaxLen = vml })
                , Spec.lenRepr = (Spec.LengthRepr { Spec.unLengthRepr = lr } ) }
      -> JSTVector { jstDetail = mkTypeInfo "vector"
                   , jstVectorRefCtor = nameToConstructor vr
                   , jstVectorMaxLen = vml
                   , jstVectorLenWidth = C.builtInSize lr }
    Spec.Record { Spec.unRecord = (C.TRecord { C.recordFields = C.Fields rfs }) }
      -> JSTRecord { jstDetail = mkTypeInfo "record"
                   , jstRecordFields = map mkFieldInfo rfs }
    Spec.Combination { Spec.unCombination = (C.TCombination { C.combinationFields = C.Fields cfs })
                     , Spec.flagsRepr = (Spec.FlagsRepr fr) }
      -> JSTCombination { jstDetail = mkTypeInfo "combination"
                        , jstCombinationFields = map mkFieldInfo cfs
                        , jstCombinationFlagsWidth = C.builtInSize fr }
    Spec.Union { Spec.unUnion = (C.TUnion { C.unionFields = C.Fields ufs })
               , Spec.tagRepr = (Spec.TagRepr tr) }
      -> JSTUnion { jstDetail = mkTypeInfo "union"
                  , jstUnionFields = map mkFieldInfo ufs
                  , jstUnionTagWidth = C.builtInSize tr }
  where
    mkTypeInfo p =
      JSTypeInfo
        { jstName = Spec.typeName t
        , jstHash = H.hashToBytes . Spec.spHash $ t
        , jstSize = JSTSizeCtx { jstMinSize = Spec.minSize t, jstMaxSize = Spec.maxSize t }
        , jstPrototype = p
        , jstConstructor = nameToConstructor $ Spec.typeName t
        }

    mkFieldInfo C.Field { C.fName = n, C.fRef = r, C.fIndex = i } =
      JSTDataField { jstdfName = n, jstdfRefCtor = nameToConstructor r, jstdfIndex = i }
    mkFieldInfo C.EmptyField { C.fName = n, C.fIndex = i } =
      JSTEmptyField { jstefName = n, jstefIndex = i }

    biConv C.BIu8  = JSU8
    biConv C.BIu16 = JSU16
    biConv C.BIu32 = JSU32
    biConv C.BIu64 = JSU64
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

renderTo :: Spec.Spec -> FilePath -> FilePath -> IO ()
renderTo spec templatePath destPath = do
  template <- T.readFile templatePath
  cfg <- mkCfg
  rendered <- hastacheStr cfg (encodeStr $ T.unpack template)
                              (mkGenericContext $ mkJsCtx spec)
  T.writeFile destPath rendered
  where
    mkCfg = do
      tpath <- getDataFileName "templates/sub"
      return $ defaultConfig { muEscapeFunc = id
                             , muTemplateFileDir = Just tpath } :: IO (MuConfig IO)

