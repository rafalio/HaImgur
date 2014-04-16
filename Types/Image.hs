{-# LANGUAGE DeriveGeneric #-}

module Types.Image where

import GHC.Generics

data Image = Image {
  id               :: String,
  title            :: String,
  description      :: String,
  datetime         :: Int,
  views            :: Int,
  link             :: String
} deriving (Show, Generic)
