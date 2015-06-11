{-# LANGUAGE TemplateHaskell #-}
module Cauterize.JavaScript.Files
  ( allFiles
  ) where

import qualified Data.ByteString as B
import Data.FileEmbed

allFiles :: [(FilePath, B.ByteString)]
allFiles = $(embedDir "data/support")
