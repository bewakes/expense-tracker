{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Models where


import           Data.Aeson
import           Database.Persist.Postgresql (SqlPersistT, runMigration,
                                              runSqlPool)
import           Database.Persist.TH         (mkMigrate, mkPersist,
                                              persistLowerCase, share,
                                              sqlSettings)
import           GHC.Generics

share [ mkPersist sqlSettings, mkMigrate "migrateAll" ] [persistLowerCase|
User
    name String
    email String
    deriving Show
Category
    name String
    userId UserId
    deriving Show
Expense
    name String
    amount Int
    categoryId CategoryId
    userId UserId
    deriving Show
|]

instance FromJSON User where
    parseJSON = withObject "User" $ \ v ->
        User <$> v .: "name"
             <*> v .: "email"

instance ToJSON User where
  toJSON (User name email) =
    object [ "name" .= name
           , "email"  .= email
           ]

instance FromJSON Expense where
    parseJSON = withObject "Expense" $ \v ->
        Expense <$> v .: "name"
                <*> v .: "amount"
                <*> v .: "categoryId"
                <*> v .: "userId"

instance ToJSON Expense where
    toJSON (Expense name amount categoryId userId) =
        object [ "name" .= name
               , "amount" .= amount
               , "categoryId" .= categoryId
               , "userId" .= userId
               ]
