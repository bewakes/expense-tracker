{-# LANGUAGE TypeApplications #-}

module Handler.Expense
where

import qualified Data.Text                       as T
import qualified Database.Esqueleto.Experimental as E
import           Import
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


getCategories :: UserId -> DB [Entity Category]
getCategories u = selectList [CategoryUserId ==. u] []
