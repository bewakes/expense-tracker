{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Category where

import qualified Database.Esqueleto.Experimental as E
import           Import
import           Utils.Data
import           Utils.Db
import           Yesod.Form.Bootstrap3

import           Handler.Home                    (getGroup)


newCategoryForm :: [(Text, GroupId)] -> UserId -> AForm Handler Category
newCategoryForm grps uid = Category
    <$> areq textField (bfs ("Category Name" :: Text)) Nothing
    <*> pure uid
    <*> areq (selectFieldList grps) (bfs ("Group" :: Text)) Nothing

getCategoryR :: Handler Html
getCategoryR = do
    uidMaybe <- maybeAuthId
    gidRaw <- lookupGetParam "group"
    let gidMaybe = E.toSqlKey . fromInteger <$> parseIntegerFromParam gidRaw
    case uidMaybe of
      Nothing -> redirect $ AuthR LoginR
      Just uid -> do
        (grp, _) <- getGroup uid gidMaybe
        categories <- runDB $ getAllGroupCategories (entityKey grp)
        defaultLayout $ do
            setTitle "Categories"
            $(widgetFile "categories/list")

getCategoryNewR :: Handler Html
getCategoryNewR = do
    uidMaybe <- maybeAuthId
    case uidMaybe of
      Nothing -> redirect $ AuthR LoginR
      Just uid -> do
        groups <- runDB $ getUserGroups uid
        let grpList = map (\(Entity k v) -> (groupName v, k)) groups
        (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ newCategoryForm grpList uid
        defaultLayout $ do
            setTitle "Create a new Category"
            $(widgetFile "categories/new")

postCategoryNewR :: Handler Html
postCategoryNewR = do
    uidMaybe <- maybeAuthId
    case uidMaybe of
      Nothing -> redirect $ AuthR LoginR
      Just uid -> do
        groups <- runDB $ getUserGroups uid
        let grpList = map (\(Entity k v) -> (groupName v, k)) groups
        ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ newCategoryForm grpList uid
        case res of
          FormSuccess category -> do
              _ <- runDB $ insert category
              addMessageI "Success" ("Category Added" :: Text)
              redirect HomeR
          _ -> defaultLayout $(widgetFile "categories/new")

