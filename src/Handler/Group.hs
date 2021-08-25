{-# LANGUAGE TypeApplications #-}
module Handler.Group where

import           Data.Maybe
import qualified Data.Text                       as T
import qualified Database.Esqueleto.Experimental as E
import           Import
import qualified Text.Read                       as TR
import           Utils.Data                      (Role (..))
import           Yesod.Form.Bootstrap3

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
        defaultLayout $(widgetFile "groups/new")

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
                            , usersGroupsAddedBy    = Just usrid
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

maybeToEither :: Text -> Maybe a -> Either Text a
maybeToEither msg Nothing = Left msg
maybeToEither _ (Just a)  = Right a

data PostMember = PostMember UserId Role

parseEither :: (Read a) => Text -> Text -> Either Text a
parseEither fname val = maybeToEither ("Invalid " <> fname) $ TR.readMaybe . T.unpack $ val

parseMemberPostParams :: Either Text Text ->  Either Text Text -> Either Text PostMember
parseMemberPostParams euid erole =
    PostMember
    <$> (E.toSqlKey <$> (euid >>= parseEither "user" ))
    <*> (erole >>= parseEither "role" )

postGroupNewMemberR :: GroupId -> Handler Html
postGroupNewMemberR gid = do
    uidMaybe <- maybeAuthId
    euid <- maybeToEither "No user provided" <$> lookupPostParam "userid"
    erole <- maybeToEither "No/Invalid role provided" <$> lookupPostParam "role"
    let parsed = parseMemberPostParams euid erole
        uid = fromJust uidMaybe -- TODO: This can throw errors but probably won't because this controller is only called if logged in
                                -- Omitted check to avoid nested case matches
    time <- liftIO getCurrentTime
    case parsed of
      Left err -> do
          addMessageI "error" err
          redirect GroupR
      Right (PostMember mid role) -> do
          -- Check if user can add member in group
          -- TODO: check role
          _ <- runDB $ getBy404 (UniqueUserGroup uid gid)
          let usg = UsersGroups
                  { usersGroupsUserId = mid
                  , usersGroupsGroupId = gid
                  , usersGroupsRole = role
                  , usersGroupsJoinedAt = time
                  , usersGroupsAddedBy = Just uid
                  , usersGroupsIsDefault = False
                  }
          _ <- runDB $ insert usg
          addMessageI "success" ("Successfully added group member" :: Text)
          redirect GroupR

getAllGroups :: UserId -> DB [Entity Group]
getAllGroups uid = E.select $ do
    (usrgrp E.:& grp) <-
        E.from $  E.Table @UsersGroups
        `E.InnerJoin` E.Table @Group
        `E.on` (\(usrgrp E.:& grp) -> usrgrp E.^. UsersGroupsGroupId E.==. grp E.^. GroupId)
    E.where_ (usrgrp E.^. UsersGroupsUserId E.==. E.val uid)
    return grp

getGroupMembers :: Entity Group -> DB [Entity User]
getGroupMembers (Entity gk _) = E.select $ do
    (usrgrp E.:& usr) <-
        E.from $  E.Table @UsersGroups
        `E.InnerJoin` E.Table @User
        `E.on` (\(usrgrp E.:& usr) -> usrgrp E.^. UsersGroupsUserId E.==. usr E.^. UserId)
    E.where_ (usrgrp E.^. UsersGroupsGroupId E.==. E.val gk)
    return usr
