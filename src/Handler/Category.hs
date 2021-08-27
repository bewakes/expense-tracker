{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Category where

import qualified Data.Text                       as T
import qualified Database.Esqueleto.Experimental as E
import           Import
import           Utils.Db
import           Yesod.Form.Bootstrap3


newCategoryForm :: UserId -> GroupId -> AForm Handler Category
newCategoryForm uid gid = Category
    <$> areq textField (bfs ("Category Name" :: Text)) Nothing
    <*> pure uid
    <*> pure gid


editCategoryForm :: Category -> GroupId -> UserId -> AForm Handler Category
editCategoryForm cat gid uid = Category
    <$> areq textField (bfs ("Category Name" :: Text)) (pure $ categoryName cat)
    <*> pure uid
    <*> pure gid

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
postCategoryNewR = loginRedirectOr $ \(UserInfo uid grp _) -> do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ newCategoryForm uid (E.entityKey grp)
    case res of
      FormSuccess category -> do
          _ <- runDB $ insert category
          addMessageI "Success" ("Category Added" :: Text)
          redirect (CategoryR, [("groupId", T.pack $ show $ E.fromSqlKey $ E.entityKey grp)])
      _ -> defaultLayout $(widgetFile "categories/new")

getCategoryEditR :: CategoryId -> Handler Html
getCategoryEditR catid = loginRedirectOr $ \(UserInfo uid grp _) -> do
    maybecategory <- runDB $ getCategoryForUser catid uid (E.entityKey grp)
    case maybecategory of
      Nothing -> sendResponseStatus status404 (TypedContent typeHtml "<h3> Category not found </h3>")
      Just (Entity _ cat) -> do
        (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ editCategoryForm cat (E.entityKey grp) uid
        defaultLayout $(widgetFile "categories/edit")

postCategoryEditR :: CategoryId -> Handler Html
postCategoryEditR catId = loginRedirectOr $ \(UserInfo uid grp _ ) -> do
    maybecategory <- runDB $ getCategoryForUser catId uid (E.entityKey grp)
    case maybecategory of
      Nothing -> sendResponseStatus status404 (TypedContent typeHtml "<h3> Category not found </h3>")
      Just (Entity catid cat) -> do
        ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ editCategoryForm cat (E.entityKey grp) uid
        case res of
          FormSuccess category -> do
              _ <- runDB $ replace catId category
              addMessageI "Success" ("Category Updated" :: Text)
              redirect (CategoryR, [("groupId", T.pack $ show $ E.fromSqlKey $ E.entityKey grp)])
          _ -> defaultLayout $(widgetFile "categories/edit")
