{- |
   Module     : DB
   Copyright  : Copyright (C) 2016 Syed Rafee
   License    : BSD3

   Maintainer : Syed Rafee s.rafee@se13.qmul.ac.uk
   Stability  : provisional
   Portability: portable

Code for accessing a sqlite3 database. 

Written by Syed Rafee s.rafee@se13.qmul.ac.uk
-}
module DB where 

import Database.HDBC
import Database.HDBC.Sqlite3
import qualified Type as T
import qualified Type2 as T2
-- | creates connection to the database
dbConnect :: IO Connection
dbConnect = do
   conn <- connectSqlite3 "sources.db"
   initialiseDB conn  
   return conn

-- | Initialises the database creating tables
initialiseDB :: IConnection conn => conn -> IO ()
initialiseDB conn = do
   tables <- getTables conn


   if not (elem "sourceList" tables) then do 
       run conn "CREATE TABLE sourceList (id TEXT,name TEXT, description TEXT, url TEXT, category TEXT,language TEXT, country TEXT)" []

       commit conn
       putStrLn "sourceList table has created"
   else
      return ()
        

   if not (elem "logosizeList" tables) then do 
       run conn "CREATE TABLE logosizeList ( small TEXT, medium TEXT, large TEXT,logoid TEXT)" []
       commit conn
       putStrLn "logosizeList table has created"   
     
   else
      return () 

   if not (elem "articles" tables) then do 
       run conn "CREATE TABLE articles (source TEXT, author TEXT,title TEXT, description TEXT, url TEXT, urlToImage TEXT, publishedAt TEXT)" []
       commit conn
       putStrLn "logosizeList table has created"  

   else
      return ()

 
   


-- | inserting values to the sourceList table
insertDB :: IConnection conn => conn -> T.Sources -> IO [Integer]
insertDB conn sources = do
   let query1 = "INSERT INTO sourceList VALUES (?,?,?,?,?,?,?)"
   let record1 = [toSql.(T.id) $ sources
                , toSql.(T.name) $ sources
                , toSql.(T.description) $ sources
                , toSql.(T.url) $ sources
                , toSql.(T.category) $ sources 
                , toSql.(T.language) $ sources
                , toSql.(T.country) $ sources
                ]

   let query2 = "INSERT INTO logosizeList VALUES (?,?,?,?)"
   let sizes = T.urlsToLogos sources
   let record2 = [ toSql.T.small $ sizes
                , toSql.T.medium $ sizes
                , toSql.T.large $ sizes
                , toSql.T.id $ sources
                ]

   rows1 <- (run conn query1 record1)
   rows2 <-  (run conn query2 record2)
   commit conn
   return [rows1, rows2]
-- | inserts values to the article table
insertarticleDB :: IConnection conn => conn -> String-> T2.News -> IO Integer 
insertarticleDB conn src news  = do
               let query3 = "INSERT INTO articles VALUES (?,?,?,?,?,?,?)"
               let record3 = [ toSql src
                             , toSql.T2.author $ news
                             , toSql.T2.title $ news
                             , toSql.T2.description $ news
                             , toSql.T2.url $ news
                             , toSql.T2.urlToImage $ news
                             , toSql.T2.publishedAt $ news 
                             ]
               rows3 <- (run conn query3 record3)
               commit conn
               return rows3
-- | retrieves data from article table
authorDB :: IConnection conn => conn -> IO [T2.News]
authorDB conn = do 
       let query3 = "SELECT articles.source,articles.author,articles.title,articles.description, articles.url, articles.urlToImage, articles.publishedAt FROM articles JOIN sourceList Where articles.source = sourceList.id "
       r3 <- quickQuery' conn query3 []
       return $ map convRow3 r3

-- | convertion of the sql type to haskell type
convRow3 :: [SqlValue] -> T2.News
convRow3 [sqlSrc,sqlAuthor, sqlTitle, sqlDescription, sqlUrl, sqlUrlToImage, sqlPublishedAt] =  T2.News {T2.author = aAuthor
                      , T2.title = aTitle
                      , T2.description = aDescription
                      , T2.url = aUrl
                      , T2.urlToImage = aUrltoimage
                      , T2.publishedAt = Just apublishedat
      
                      }
             where aAuthor = case fromSql sqlAuthor of
                         Just x -> x
                         Nothing -> "NULL"   
                   aTitle = case fromSql sqlTitle of
                         Just x -> x
                         Nothing -> "NULL"
                   aDescription = case fromSql sqlDescription of
                         Just x -> x
                         Nothing -> "NULL"
                   aUrl = case fromSql sqlUrl of
                         Just x -> x
                         Nothing -> "NULL"
                   aUrltoimage = case fromSql sqlUrlToImage of
                         Just x -> x
                         Nothing -> "NULL"
                   apublishedat = case fromSql sqlPublishedAt of
                         Just x -> x
                         Nothing -> "NULL" 
                  


-- | get all the columns from both of the tables
allArticles :: IConnection conn => conn -> IO [T.Sources]
allArticles conn =
    do let query = "SELECT sourceList.id, sourceList.name, sourceList.description, sourceList.url, sourceList.category, sourceList.language, sourceList.country, logosizeList.small, logosizeList.medium, logosizeList.large FROM sourceList JOIN logosizeList Where sourceList.id =  logosizeList.logoid"
       r <- quickQuery' conn query []
       return $ map convRow r
-- | convertion of the sql type to haskell type
convRow :: [SqlValue] -> T.Sources
convRow [sqlId, sqlName, sqlDescription, sqlUrl, sqlCategory, sqlLanguage, sqlCountry, sqlSmall, sqlMedium, sqlLarge] =  T.Sources { T.id = srcid
                      , T.name = srcname
                      , T.description = srcdescription
                      , T.url = srcurl
                      , T.category = srccategory
                      , T.language = srclanguage
                      , T.country = srccountry
                      , T.urlsToLogos = T.Sizes { T.small = srclogosmall, T.medium = srclogomedium, T.large = srclogolarge }
                      }
             where srcid = case fromSql sqlId of
                         Just x -> x
                         Nothing -> "NULL"   
                   srcname = case fromSql sqlName of
                         Just x -> x
                         Nothing -> "NULL"
                   srcdescription = case fromSql sqlDescription of
                         Just x -> x
                         Nothing -> "NULL"
                   srcurl = case fromSql sqlUrl of
                         Just x -> x
                         Nothing -> "NULL"
                   srccategory = case fromSql sqlCategory of
                         Just x -> x
                         Nothing -> "NULL"
                   srclanguage = case fromSql sqlLanguage of
                         Just x -> x
                         Nothing -> "NULL" 
                   srccountry = case fromSql sqlCountry of
                         Just x -> x
                         Nothing -> "NULL" 
                   srclogosmall = case fromSql sqlSmall of
                         Just x -> x
                         Nothing -> "NULL"
                   srclogomedium = case fromSql sqlMedium of
                         Just x -> x
                         Nothing -> "NULL"
                   srclogolarge = case fromSql sqlLarge of
                         Just x -> x
                         Nothing -> "NULL"

      
