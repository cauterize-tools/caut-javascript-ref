{-# LANGUAGE QuasiQuotes #-}
module Cauterize.JavaScript.LibTemplate
  ( libFromSpec
  ) where

import Data.String.Interpolate
import Data.Text.Lazy (unpack)
import Data.List.Split
import Data.List (intercalate)
import Data.Char
import Data.Word
import Numeric

import qualified Cauterize.Specification as S
import qualified Cauterize.Common.Types as S
import qualified Cauterize.FormHash as S

libFromSpec :: S.Spec -> String
libFromSpec spec = unlines $ concat [ [libPreamble]
                                    , libTypes spec
                                    , [ libPostamble spec ] ]

libPreamble :: String
libPreamble = [i|
'use strict';

var prot = require('./libcaut/prototypes.js');

/* Define all type information. */
var libTypes = {};
|]

libPostamble :: S.Spec -> String
libPostamble spec = [i|
Object.freeze(libTypes);

var metaInfo = {
  lengthWidth: #{(S.unLengthTagWidth . S.specLengthTagWidth) spec},
  typeWidth: #{(S.unTypeTagWidth . S.specTypeTagWidth) spec},
};
Object.freeze(metaInfo);

var specInfo = {
  types: libTypes,
  metaInfo: metaInfo,
};
Object.freeze(specInfo);

exports.SpecificationInfo = specInfo;
|]

libTypes :: S.Spec -> [String]
libTypes spec = map libType (S.specTypes spec)

libType :: S.SpType -> String
libType t =
  let n = unpack $ S.typeName t
      h = hashToBytes (S.spHash t)
      ctor = nameToConstructor n
  in [i|
function #{ctor}(elems) { prot.#{jsProt t}.call(this, elems); }
(function () {
  var typeHash = [#{h}];
  var typeSize = { min: #{S.minSize t}, max: #{S.maxSize t} };
  prot.#{jsMkType n ctor t}
}());
libTypes['#{n}'] = #{ctor};
|]

jsProt :: S.SpType -> String
jsProt S.BuiltIn {} = "CBuiltIn"
jsProt S.Synonym {} = "CSynonym"
jsProt S.Array {} = "CArray"
jsProt S.Vector {} = "CVector"
jsProt S.Record {} = "CRecord"
jsProt S.Union {} = "CUnion"
jsProt S.Combination {} = "CCombination"

jsMkType :: String -> String -> S.SpType -> String
jsMkType _ ctor S.BuiltIn {} = [i|
  mk#{ctor}(#{ctor}, typeHash, typeSize);
|]
jsMkType n ctor S.Synonym { S.unSynonym = (S.TSynonym { S.synonymRepr = r } ) } =
  let refCtor = nameToConstructor (show r)
  in [i|
  mkSynonym(#{ctor}, '#{n}', #{refCtor}, typeHash, typeSize);
|]
jsMkType n ctor S.Array { S.unArray = (S.TArray { S.arrayRef = ar, S.arrayLen = al }) } =
  let refCtor = nameToConstructor (unpack ar)
  in [i|
  mkArray(#{ctor}, '#{n}', #{refCtor}, #{show al}, typeHash, typeSize);
|]

nameToConstructor :: String -> String
nameToConstructor n =
  let strCap [] = []
      strCap (x:xs) = toUpper x : xs
      parts = splitOn "_" n
      caped = map strCap parts
  in concat caped

hashToBytes :: S.FormHash -> String
hashToBytes h = let bs = S.hashToBytes h
                in bytesToCSV bs

bytesToCSV :: [Word8] -> String
bytesToCSV bs = intercalate "," $ fmap showByte bs
  where
    showByte :: Word8 -> String
    showByte b = let s = showHex b ""
                 in case length s of
                      2 -> "0x" ++ map toUpper s
                      1 -> "0x0" ++ map toUpper s
                      _ -> error "This should be impossible."
