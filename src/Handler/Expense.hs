{-# LANGUAGE TypeApplications #-}

module Handler.Expense
where

import           Import
import           Utils.Db              (getUserGroups)
import           Yesod.Form.Bootstrap3


newExpenseForm :: [(Text, CategoryId)] -> [(Text, GroupId)] -> UserId ->  AForm Handler Expense
newExpenseForm cats grps usrid = Expense
    <$> areq (check validateAmount doubleField) (bfs ("Amount" :: Text)) Nothing
    <*> areq (selectFieldList cats) (bfs ("Category" :: Text)) Nothing
    <*> areq dayField (bfs ("Date" :: Text)) Nothing
    <*> pure []
    <*> areq textField (bfs ("Description" :: Text)) (Just " ")
    <*> areq (selectFieldList grps) (bfs ("Group" :: Text)) Nothing
    <*> pure usrid
    <*> lift (liftIO getCurrentTime)
        where validateAmount amt
                | amt < 0 = Left ("Amount cannot be negative" :: Text)
                | otherwise = Right amt

getExpenseNewR :: Handler Html
getExpenseNewR = loginRedirectOr $ \(UserInfo uid _ _) -> do
    cats <- runDB $ getCategories uid
    groups <- runDB $ getUserGroups uid
    let catList = map (\(Entity k v) -> (categoryName v, k)) cats
        grpList = map (\(Entity k v) -> (groupName v, k)) groups
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ newExpenseForm catList grpList uid
    defaultLayout $ do
        setTitle "Add an Expense"
        $(widgetFile "expenses/new")

postExpenseNewR :: Handler Html
postExpenseNewR = loginRedirectOr $ \(UserInfo uid _ _) -> do
    cats <- runDB $ getCategories uid
    groups <- runDB $ getUserGroups uid
    let catList = map (\(Entity k v) -> (categoryName v, k)) cats
        grpList = map (\(Entity k v) -> (groupName v, k)) groups
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ newExpenseForm catList grpList uid
    case res of
      FormSuccess expense -> do
          _ <- runDB $ insert expense
          addMessageI "success" ("Expense added" :: Text)
          redirect HomeR
      _ -> defaultLayout $(widgetFile "expenses/new")


getCategories :: UserId -> DB [Entity Category]
getCategories u = selectList [CategoryUserId ==. u] []
