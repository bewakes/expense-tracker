module Handler.User where

import           Import

getUserR :: Handler Html
getUserR = do
    users <- runDB getAllUsers
    defaultLayout $ do
        setTitle "All Users"
        [whamlet|<h1>Users
            <ul>
                $forall (Entity _ user) <- users
                    <li>
                        #{fromMaybe "--" (userFirstName user)}
                        #{fromMaybe "--" (userLastName user)}
        |]

getAllUsers :: DB [Entity User]
getAllUsers = selectList [] []
