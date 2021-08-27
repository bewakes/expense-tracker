{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Category where

import qualified Database.Esqueleto.Experimental as E
import           Import
import           Utils.Db
import           Yesod.Form.Bootstrap3


newCategoryForm :: UserId -> GroupId -> AForm Handler Category
newCategoryForm uid gid = Category
    <$> areq textField (bfs ("Category Name" :: Text)) Nothing
    <*> pure uid
    <*> pure gid


editCategoryForm :: Category -> [(Text, GroupId)] -> UserId -> AForm Handler Category
editCategoryForm cat grps uid = Category
    <$> areq textField (bfs ("Category Name" :: Text)) (pure $ categoryName cat)
    <*> pure uid
    <*> areq (selectFieldList grps) (bfs ("Group" :: Text)) (pure $ categoryGroupId cat)

getCategoryR :: Handler Html
getCategoryR = loginRedirectOr $ \(UserInfo _ grp _) -> do
    categories <- runDB $ getAllGroupCategories (entityKey grp)
    defaultLayout $ do
        setTitle "Categories"
        $(widgetFile "categories/list")

getCategoryNewR :: Handler Html
getCategoryNewR = loginRedirectOr $ \(UserInfo uid grp _) -> do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ newCategoryForm uid (E.entityKey grp)
    defaultLayout $ do
        setTitle "Create a new Category"
        $(widgetFile "categories/new")

postCategoryNewR :: Handler Html
postCategoryNewR = loginRedirectOr $ \(UserInfo uid grp _)-> do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ newCategoryForm uid (E.entityKey grp)
    case res of
      FormSuccess category -> do
          _ <- runDB $ insert category
          addMessageI "Success" ("Category Added" :: Text)
          redirect HomeR
      _ -> defaultLayout $(widgetFile "categories/new")


getCategoryEditR :: CategoryId -> Handler Html
getCategoryEditR catid = loginRedirectOr $ \(UserInfo uid _ _) -> do
    maybecategory <- runDB $ getCategoryForUser catid uid
    case maybecategory of
      Nothing -> sendResponseStatus status404 (TypedContent typeHtml "<h3> Category not found </h3>")
      Just (Entity _ cat) -> do
        groups <- runDB $ getUserGroups uid
        let grpList = map (\(Entity k v) -> (groupName v, k)) groups
        (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ editCategoryForm cat grpList uid
        defaultLayout $(widgetFile "categories/edit")

postCategoryEditR :: CategoryId -> Handler Html
postCategoryEditR = undefined
