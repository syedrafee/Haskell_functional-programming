{- |
   Module     : DB
   Copyright  : Copyright (C) 2016 Syed Rafee
   License    : BSD3

   Maintainer : Syed Rafee s.rafee@se13.qmul.ac.uk
   Stability  : provisional
   Portability: portable

Code for creating data types . 

Written by Syed Rafee s.rafee@se13.qmul.ac.uk
-}
{-# LANGUAGE DeriveGeneric #-}
module Type where
import Data.Text hiding (map)
import GHC.Generics

data JsonObject = 
   JsonObject { status :: String
              , sources :: [Sources]
              } deriving (Eq, Show, Generic)

data Sources = 
   Sources { id :: String
           , name :: String
           , description :: String
           , url :: String
           , category :: String
           , language :: String
           , country :: String
           , urlsToLogos :: Sizes
           } deriving (Eq, Show, Generic)


data Sizes = 
   Sizes { small :: String
         , medium :: String
         , large :: String
         } deriving (Eq, Show, Generic)


