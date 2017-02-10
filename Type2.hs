{-# LANGUAGE DeriveGeneric #-}
module Type2 where
import Data.Text hiding (map)
import GHC.Generics
data JsonarticleObject = 
   JsonarticleObject { status :: String
              , source :: String
              , sortBy :: String
              , articles :: [News]
              } deriving (Eq, Show, Generic)
data News = 
   News { author :: String
        , title :: String
        , description :: String
        , url :: String
        , urlToImage :: String
        , publishedAt :: Maybe String
        } deriving (Eq, Show, Generic)
