module Cauterize.JavaScript.Options
  ( CautJSOpts(..)
  ) where

data CautJSOpts = CautJSOpts
  { specFile :: FilePath
  , outputDirectory :: FilePath
  } deriving (Show)
