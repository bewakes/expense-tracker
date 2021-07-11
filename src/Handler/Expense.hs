{-# LANGUAGE TypeApplications #-}

module Handler.Expense
where

import           Import
import           Utils                 (getUserGroups)
import           Yesod.Form.Bootstrap3


newExpenseForm :: [(Text, CategoryId)] -> [(Text, GroupId)] -> UserId ->  AForm Handler Expense
newExpenseForm cats grps usrid = Expense
    <$> areq doubleField (bfs ("Amount" :: Text)) Nothing
    <*> areq (selectFieldList cats) (bfs ("Category" :: Text)) Nothing
    <*> areq dayField (bfs ("Date" :: Text)) Nothing
    <*> pure []
    <*> areq textField (bfs ("Description" :: Text)) Nothing
    <*> areq (selectFieldList grps) (bfs ("Group" :: Text)) Nothing
    <*> pure usrid
    <*> lift (liftIO getCurrentTime)

getExpenseNewR :: Handler Html
getExpenseNewR = do
    uid <- maybeAuthId
    case uid of
      Nothing -> redirect $ AuthR LoginR
      Just u -> do
        cats <- runDB $ getCategories u
        groups <- runDB $ getUserGroups u
        let catList = map (\(Entity k v) -> (categoryName v, k)) cats
            grpList = map (\(Entity k v) -> (groupName v, k)) groups
        (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ newExpenseForm catList grpList u
        defaultLayout $ do
            setTitle "Add an Expense"
            $(widgetFile "expenses/new")


postExpenseNewR :: Handler Html
postExpenseNewR = do
    uid <- maybeAuthId
    case uid of
      Nothing -> redirect $ AuthR LoginR
      Just u -> do
        cats <- runDB $ getCategories u
        groups <- runDB $ getUserGroups u
        let catList = map (\(Entity k v) -> (categoryName v, k)) cats
            grpList = map (\(Entity k v) -> (groupName v, k)) groups
        ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ newExpenseForm catList grpList u
        case res of
          FormSuccess expense -> do
              _ <- runDB $ insert expense
              addMessageI "success" ("Expense added" :: Text)
              redirect HomeR
          _ -> defaultLayout $(widgetFile "expenses/new")


getCategories :: UserId -> DB [Entity Category]
getCategories u = selectList [CategoryUserId ==. u] []
