{- |
   Module     : DB
   Copyright  : Copyright (C) 2016 Syed Rafee
   License    : BSD3

   Maintainer : Syed Rafee s.rafee@se13.qmul.ac.uk
   Stability  : provisional
   Portability: portable

Code for extracting json data from an API. 

Written by Syed Rafee s.rafee@se13.qmul.ac.uk
-}
{-# LANGUAGE DeriveGeneric #-}
module JsonExtractor  where
import Data.Aeson
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Type
import Type2

-- Instances to convert our type to/from JSON.

instance FromJSON JsonObject
instance ToJSON JsonObject
instance FromJSON Sources
instance ToJSON Sources
instance FromJSON Sizes
instance ToJSON Sizes
instance FromJSON JsonarticleObject
instance ToJSON JsonarticleObject
instance FromJSON News
instance ToJSON News


-- | URL that points to the remote JSON file, in case
--   you have it.
jsonURL :: String
jsonURL = "https://newsapi.org/v1/sources?language=en"


getarticleJson ::String ->  IO B.ByteString
getarticleJson sourceUrl = simpleHttp sourceUrl

-- Read the remote copy of the JSON file.
getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL


-- | Get JSON data and decode it 
getObject :: IO (Maybe JsonObject)
getObject = do
 
 d <- (eitherDecode <$> getJSON) :: IO (Either String JsonObject)
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 case d of
  Left err -> do 
               putStrLn err
               return Nothing
  Right ps -> return (Just ps)

-- | Get JSON data and decode it
getjsonarticleObject :: B.ByteString -> IO (Maybe JsonarticleObject)
getjsonarticleObject bytestring = do
 
 d <- (eitherDecode <$> (return bytestring)) :: IO (Either String JsonarticleObject)
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 case d of
  Left err -> do 
               putStrLn err
               return Nothing
  Right ps -> return (Just ps)

-- | Get haskell data and encode it
getJson a = do

 d <- (encode <$> a) 
 return d


