module Handler.Group where

import           Import
import Yesod.Form.Bootstrap3

groupForm :: UserId -> AForm Handler Group
groupForm uid = Group
         <$> areq textField (bfs ("Name" :: Text)) Nothing
         <*> areq textField (bfs ("Description" :: Text)) Nothing
         <*> lift (liftIO getCurrentTime)
         <*> pure uid


getGroupR :: Handler Html
getGroupR = do
    groups <- runDB getAllGroups
    defaultLayout $(widgetFile "groups/list")

getGroupNewR :: Handler Html
getGroupNewR = do
    uid <- maybeAuthId
    case uid of
      Nothing -> permissionDenied "You are not authorized for this page"
      Just usrid -> do
        (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ groupForm usrid
        defaultLayout $ do
            $(widgetFile "groups/new")

postGroupNewR :: Handler Html
postGroupNewR = do
    uid <- maybeAuthId
    case uid of
      Nothing -> permissionDenied "You are not authorized for this page"
      Just usrid -> do
        ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ groupForm usrid
        case res of
          FormSuccess grp -> do
                    groupId <- runDB $ insert grp
                    redirect $ GroupDetailR groupId
          _ -> defaultLayout $(widgetFile "groups/new")

getGroupDetailR :: GroupId -> Handler Html
getGroupDetailR groupId = do
    grp <- runDB $ get404 groupId
    defaultLayout $(widgetFile "groups/detail")

postGroupR :: Handler Html
postGroupR = error "Hi there"

getAllGroups :: DB [Entity Group]
getAllGroups = selectList [] []
