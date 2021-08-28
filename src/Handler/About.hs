module Handler.About where

import           Import
import           Yesod.Form.Bootstrap3


newFeedbackForm :: AForm Handler Feedback
newFeedbackForm = Feedback
    <$> areq textField (bfs ("First Name" :: Text)) Nothing
    <*> areq textField (bfs ("Last Name" :: Text)) Nothing
    <*> areq textField (bfs ("Feedback" :: Text)) Nothing


getAboutR :: Handler Html
getAboutR = do
    maybeuid <- maybeAuthId
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm newFeedbackForm
    defaultLayout $ do
        setTitle "About"
        $(widgetFile "about")


postAboutR :: Handler Html
postAboutR = do
    maybeuid <- maybeAuthId
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm newFeedbackForm
    case res of
      FormSuccess feedback -> do
          _ <- runDB $ insert feedback
          addMessageI "success" ("Thank you very much your feedback!" :: Text)
          redirect HomeR
      _ -> defaultLayout $ do
          setTitle "About"
          $(widgetFile "about")
