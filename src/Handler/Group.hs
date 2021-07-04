module Handler.Group where

import           Import
import Yesod.Form.Bootstrap3

groupForm :: AForm Handler Group
groupForm = Group
         <$> areq textField (bfs ("Name" :: Text)) Nothing
         <*> areq textField (bfs ("Description" :: Text)) Nothing


getGroupR :: Handler Html
getGroupR = do
    groups <- runDB getAllGroups
    defaultLayout $(widgetFile "groups/list")

getGroupNewR :: Handler Html
getGroupNewR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm groupForm
    groups <- runDB getAllGroups
    defaultLayout $ do
        $(widgetFile "groups/new")

postGroupNewR :: Handler Html
postGroupNewR = do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm groupForm
    case res of
      FormSuccess group -> do
                groupId <- runDB $ insert group
                redirect $ GroupDetailR groupId
      _ -> defaultLayout $(widgetFile "groups/new")

getGroupDetailR :: GroupId -> Handler Html
getGroupDetailR groupId = do
    group <- runDB $ get404 groupId
    defaultLayout $(widgetFile "groups/detail")

postGroupR :: Handler Html
postGroupR = undefined

getAllGroups :: DB [Entity Group]
getAllGroups = selectList [] []
