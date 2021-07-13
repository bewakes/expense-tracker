{-# LANGUAGE TypeApplications #-}
module Handler.User where

import qualified Data.Text                       as T
import qualified Database.Esqueleto.Experimental as E
import           Import

getUserR :: Handler Html
getUserR = error "not implemented"

getAllUsers :: DB [Entity User]
getAllUsers = selectList [] []

getUserQueryR :: Text -> Handler Value
getUserQueryR qry = do
    users <- runDB $ queryUser qry
    returnJson $ map toJsonUser users

toJsonUser :: Entity User -> Value
toJsonUser (Entity k u) = object
    [ "email" .= userEmail u
    , "id" .= E.fromSqlKey k
    , "firstName" .= userFirstName u
    , "lastName" .= userLastName u
    ]

queryUser :: Text -> DB [Entity User]
queryUser qry = E.select $ do
    usr <- E.from $ E.Table @User
    let likefname = usr E.^. UserFirstName `E.ilike` E.val concated
        likelname = usr E.^. UserLastName `E.ilike` E.val concated
        likeemail = usr E.^. UserEmail `E.ilike` E.val concated
    E.where_ $ likefname E.||. likelname  E.||. likeemail
    return usr
        where concated = T.concat ["%", qry, "%"]
