module Handler.Expense
where

import           Import
import           Yesod.Form.Bootstrap3


newExpenseForm :: [(Text, CategoryId)] -> UserId ->  AForm Handler Expense
newExpenseForm cats usrid = Expense
    <$> areq doubleField (bfs ("Amount" :: Text)) Nothing
    <*> areq (selectFieldList cats) (bfs ("Category" :: Text)) Nothing
    <*> lift (liftIO getCurrentTime)
    <*> pure []
    <*> areq textField (bfs ("Description" :: Text)) Nothing
    <*> pure usrid

getExpenseNewR :: Handler Html
getExpenseNewR = do
    uid <- maybeAuthId
    case uid of
      Nothing -> redirect $ AuthR LoginR
      Just u -> do
        cats <- runDB $ getCategories u
        let catList = map (\(Entity k v) -> (categoryName v, k)) cats
        (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ newExpenseForm catList u
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
        let catList = map (\(Entity k v) -> (categoryName v, k)) cats
        ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ newExpenseForm catList u
        case res of
          FormSuccess expense -> do
              _ <- runDB $ insert expense
              addMessageI "success" ("Expense added" :: Text)
              redirect HomeR
          _ -> defaultLayout $(widgetFile "expenses/new")


getCategories :: UserId -> DB [Entity Category]
getCategories u = selectList [CategoryUserId ==. u] []