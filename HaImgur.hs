{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
 
module HaImgur (
  getAlbumById,
  getImageById,
) where
 
import Data.ByteString.Lazy as L
import Data.ByteString.Char8 as B 
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Control.Monad
import Data.Aeson
import Control.Applicative

import Types.Image
import Types.Album

-- Auto-derive JSON Parsers
instance FromJSON Image 
instance ToJSON Image 

instance FromJSON Album
instance ToJSON Album

data ImgurResponse a = ImgurResponse {
  status :: Int,
  contents :: a,
  success :: Bool
} deriving (Show)

instance FromJSON a => FromJSON (ImgurResponse a) where
  parseJSON (Object v) = ImgurResponse <$> v .: "status" <*>
                                           v .: "data" <*>
                                           v .: "success"
  parseJSON _          = mzero

clientID1     = "cc062e3c0145e31"

albumBaseURL = "https://api.imgur.com/3/album/"
imageBaseURL = "https://api.imgur.com/3/image/"

getImageAPIuri imgId   = imageBaseURL ++ imgId
getAlbumAPIuri albumId = albumBaseURL ++ albumId

headers = [ (hAuthorization, (B.pack "Client-ID ") `B.append` (B.pack clientID1)) ]
sec2micro = (*1000000)

getData :: FromJSON a => L.ByteString -> Either String a
getData b = contents <$> (eitherDecode b :: FromJSON a => Either String (ImgurResponse a))

getImage :: L.ByteString -> Either String Image
getImage = getData

getAlbum :: L.ByteString -> Either String Album
getAlbum = getData


data Imgur = Imgur {
  clientID :: String
} deriving (Show)

getAlbumById = (liftM getAlbum) . downloadResource . getAlbumAPIuri
getImageById = (liftM getImage) . downloadResource . getImageAPIuri

downloadResource :: String -> IO L.ByteString
downloadResource url = do
  request' <- parseUrl url
  let request = request' { requestHeaders = headers, 
                           responseTimeout = Just . sec2micro $ 10 }
  (withManager $ httpLbs request) >>= return . responseBody
