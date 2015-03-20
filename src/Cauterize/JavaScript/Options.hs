module Cauterize.JavaScript.Options
  ( CautJSOpts(..)
  ) where

data CautJSOpts = CautJSOpts
  { specFile :: FilePath
  , metaFile :: FilePath
  , outputDirectory :: FilePath
  } deriving (Show)
