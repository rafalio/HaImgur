{-# LANGUAGE DeriveGeneric #-}

module Types.Album where

import GHC.Generics
import qualified Types.Image as I

data Album = Album {
  id              :: String,
  title           :: Maybe String,
  description     :: Maybe String,
  datetime        :: Int,
  cover           :: String,
  cover_width     :: Int,
  cover_height    :: Int,
  account_url     :: String,
  privacy         :: String,
  layout          :: String,
  views           :: Int,
  link            :: String,
  images_count    :: Int,
  images          :: [I.Image]
} deriving (Show, Generic)
