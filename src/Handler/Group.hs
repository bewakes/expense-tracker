{-# LANGUAGE TypeApplications #-}
module Handler.Group where

import           Import
import Yesod.Form.Bootstrap3
import qualified Database.Esqueleto.Experimental as E

groupForm :: UserId -> AForm Handler Group
groupForm uid = Group
         <$> areq textField (bfs ("Name" :: Text)) Nothing
         <*> areq textField (bfs ("Description" :: Text)) Nothing
         <*> lift (liftIO getCurrentTime)
         <*> pure uid

getGroupR :: Handler Html
getGroupR = do
    uidMaybe <- maybeAuthId
    case uidMaybe of
      Nothing -> error "Not authorized"
      Just uid -> do
        groups <- runDB $ getAllGroups uid
        groupsMems <- mapM (runDB . getGroupMembers) groups
        let grpMems = zip groups groupsMems
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
        time <- liftIO getCurrentTime
        case res of
          FormSuccess grp -> do
                    groupId <- runDB $ insert grp
                    let usrgrp = UsersGroups {
                              usersGroupsUserId     = usrid
                            , usersGroupsGroupId    = groupId
                            , usersGroupsIsDefault  = True
                            , usersGroupsJoinedAt   = time
                            , usersGroupsRole       = SuperAdmin
                            }
                    _ <- runDB $ insert usrgrp
                    addMessageI "success" ("Group added" :: Text)
                    redirect HomeR
          _ -> defaultLayout $(widgetFile "groups/new")

getGroupDetailR :: GroupId -> Handler Html
getGroupDetailR groupId = do
    grp <- runDB $ get404 groupId
    defaultLayout $(widgetFile "groups/detail")

getGroupNewMemberR :: GroupId -> Handler Html
getGroupNewMemberR gid = do
    uidMaybe <- maybeAuthId
    case uidMaybe of
      Nothing -> redirect $ AuthR LoginR
      Just uid -> do
          -- Check if user and group exist
          _ <- runDB $ getBy404 (UniqueUserGroup uid gid)
          grp <- runDB $ get404 gid
          defaultLayout $(widgetFile "groups/new-member")

postGroupNewMemberR :: GroupId -> Handler Html
postGroupNewMemberR _ = error "Not implemented"

getAllGroups :: UserId -> DB [Entity Group]
getAllGroups uid = E.select $ do
    (usrgrp E.:& grp) <-
        E.from $  E.table @UsersGroups
        `E.InnerJoin` E.table @Group
        `E.on` (\(usrgrp E.:& grp) -> usrgrp E.^. UsersGroupsGroupId E.==. grp E.^. GroupId)
    E.where_ (usrgrp E.^. UsersGroupsUserId E.==. E.val uid)
    return grp

getGroupMembers :: Entity Group -> DB [Entity User]
getGroupMembers (Entity gk _) = E.select $ do
    (usrgrp E.:& usr) <-
        E.from $  E.table @UsersGroups
        `E.InnerJoin` E.table @User
        `E.on` (\(usrgrp E.:& usr) -> usrgrp E.^. UsersGroupsUserId E.==. usr E.^. UserId)
    E.where_ (usrgrp E.^. UsersGroupsGroupId E.==. E.val gk)
    return usr
