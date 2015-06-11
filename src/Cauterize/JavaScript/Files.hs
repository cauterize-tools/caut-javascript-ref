{-# LANGUAGE TemplateHaskell #-}
module Cauterize.JavaScript.Files
  ( allFiles
  ) where

import qualified Data.ByteString as B
import Data.FileEmbed

allFiles :: [(FilePath, B.ByteString)]
allFiles =
  [ ("libcaut/prototypes/carray.js",       $(embedFile "data/support/libcaut/prototypes/carray.js"))
  , ("libcaut/prototypes/cbuiltin.js",     $(embedFile "data/support/libcaut/prototypes/cbuiltin.js"))
  , ("libcaut/prototypes/ccombination.js", $(embedFile "data/support/libcaut/prototypes/ccombination.js"))
  , ("libcaut/prototypes/crecord.js",      $(embedFile "data/support/libcaut/prototypes/crecord.js"))
  , ("libcaut/prototypes/csynonym.js",     $(embedFile "data/support/libcaut/prototypes/csynonym.js"))
  , ("libcaut/prototypes/ctype.js",        $(embedFile "data/support/libcaut/prototypes/ctype.js"))
  , ("libcaut/prototypes/cunion.js",       $(embedFile "data/support/libcaut/prototypes/cunion.js"))
  , ("libcaut/prototypes/cvector.js",      $(embedFile "data/support/libcaut/prototypes/cvector.js"))
  , ("libcaut/buffer.js",                  $(embedFile "data/support/libcaut/buffer.js"))
  , ("libcaut/cast.js",                    $(embedFile "data/support/libcaut/cast.js"))
  , ("libcaut/cauterize.js",               $(embedFile "data/support/libcaut/cauterize.js"))
  , ("libcaut/prototypes.js",              $(embedFile "data/support/libcaut/prototypes.js"))
  , ("libcaut/typedict.js",                $(embedFile "data/support/libcaut/typedict.js"))
  , ("test_libcaut.js",                    $(embedFile "data/support/test_libcaut.js"))
  ]
