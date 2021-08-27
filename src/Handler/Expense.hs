{-# LANGUAGE TypeApplications #-}

module Handler.Expense
where

import           Data.Maybe                      (fromJust)
import qualified Data.Text                       as T
import qualified Database.Esqueleto.Experimental as E
import           Import
import           Utils.Db                        (getExpenseForUser)
import           Yesod.Form.Bootstrap3


newExpenseForm :: [(Text, CategoryId)] -> GroupId -> UserId ->  AForm Handler Expense
newExpenseForm cats grpid usrid = Expense
    <$> areq (check validateAmount doubleField) (bfs ("Amount" :: Text)) Nothing
    <*> areq (selectFieldList cats) (bfs ("Category" :: Text)) Nothing
    <*> areq dayField (bfs ("Date" :: Text)) Nothing
    <*> pure []
    <*> areq textField (bfs ("Description" :: Text)) (Just " ")
    <*> pure grpid
    <*> pure usrid
    <*> lift (liftIO getCurrentTime)
        where validateAmount amt
                | amt < 0 = Left ("Amount cannot be negative" :: Text)
                | otherwise = Right amt

editExpenseForm :: Maybe Expense -> [(Text, CategoryId)] -> GroupId -> UserId ->  AForm Handler Expense
editExpenseForm mexp cats grpid usrid = Expense
    <$> areq (check validateAmount doubleField) (bfs ("Amount" :: Text)) (expenseAmount <$> mexp)
    <*> areq (selectFieldList cats) (bfs ("Category" :: Text)) (expenseCategoryId <$> mexp)
    <*> areq dayField (bfs ("Date" :: Text)) (expenseDate <$> mexp)
    <*> pure []
    <*> areq textField (bfs ("Description" :: Text)) (expenseDescription <$> mexp)
    <*> pure grpid
    <*> pure usrid
    -- <*> lift (liftIO getCurrentTime)
    <*> pure (fromJust (expenseCreatedAt <$> mexp))  -- TODO: is fromJust risky? should not be as expense is guranteed to have value
        where validateAmount amt
                | amt < 0 = Left ("Amount cannot be negative" :: Text)
                | otherwise = Right amt

getExpenseNewR :: Handler Html
getExpenseNewR = loginRedirectOr $ \(UserInfo uid grp _) -> do
    cats <- runDB $ getCategories uid
    let catList = map (\(Entity k v) -> (categoryName v, k)) cats
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ newExpenseForm catList (E.entityKey grp) uid
    defaultLayout $ do
        setTitle "Add an Expense"
        $(widgetFile "expenses/new")

postExpenseNewR :: Handler Html
postExpenseNewR = loginRedirectOr $ \(UserInfo uid grp _) -> do
    cats <- runDB $ getCategories uid
    let catList = map (\(Entity k v) -> (categoryName v, k)) cats
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ newExpenseForm catList (E.entityKey grp) uid
    case res of
      FormSuccess expense -> do
          _ <- runDB $ insert expense
          addMessageI "success" ("Expense added" :: Text)
          redirect (HomeR, [("groupId", T.pack $ show $ E.fromSqlKey $ E.entityKey grp)])
      _ -> defaultLayout $(widgetFile "expenses/new")


getExpenseEditR :: ExpenseId -> Handler Html
getExpenseEditR expId = loginRedirectOr $ \(UserInfo uid grp _) -> do
    maybeexp <- runDB $ getExpenseForUser expId uid (E.entityKey grp)
    cats <- runDB $ getCategories uid
    let catList = map (\(Entity k v) -> (categoryName v, k)) cats
    case maybeexp of
      Nothing -> sendResponseStatus status404 (TypedContent typeHtml "<h3> Expense not found </h3>")
      Just (Entity expid expense) -> do
        (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ editExpenseForm (Just expense) catList (E.entityKey grp) uid
        defaultLayout $(widgetFile "expenses/edit")

postExpenseEditR :: ExpenseId -> Handler Html
postExpenseEditR expId = loginRedirectOr $ \(UserInfo uid grp _) -> do
    maybeexp <- runDB $ getExpenseForUser expId uid (E.entityKey grp)
    cats <- runDB $ getCategories uid
    let catList = map (\(Entity k v) -> (categoryName v, k)) cats
    case maybeexp of
      Nothing -> sendResponseStatus status404 (TypedContent typeHtml "<h3> Expense not found </h3>")
      Just (Entity expid expense) -> do
        ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ editExpenseForm (Just expense) catList (E.entityKey grp) uid
        case res of
          FormSuccess expense' -> do
              _ <- runDB $ replace expId expense'
              addMessageI "Success" ("Expense Updated" :: Text)
              redirect (HomeR, [("groupId", T.pack $ show $ E.fromSqlKey $ E.entityKey grp)])
          _ -> defaultLayout $(widgetFile "expenses/edit")

-- TODO: soft delete
postExpenseDeleteR :: ExpenseId -> Handler Html
postExpenseDeleteR expId = loginRedirectOr $ \(UserInfo uid grp _) -> do
    maybeexp <- runDB $ getExpenseForUser expId uid (E.entityKey grp)
    case maybeexp of
      Nothing -> sendResponseStatus status404 (TypedContent typeHtml "<h3> Expense not found </h3>")
      Just (Entity expid _) -> do
          _ <- runDB $ delete expid
          addMessageI "Success" ("Expense Deleted" :: Text)
          redirect (HomeR, [("groupId", T.pack $ show $ E.fromSqlKey $ E.entityKey grp)])

getCategories :: UserId -> DB [Entity Category]
getCategories u = selectList [CategoryUserId ==. u] []
