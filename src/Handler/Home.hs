{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Handler.Home where

import           Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import           Import
import qualified Prelude            as P

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo        :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

getHomeR :: Handler Html
getHomeR = do
    uid <- maybeAuthId
    expenses <- runDB $ getAllUserExpenses uid
    let total = fmap P.sum $ sequence $ map fst3 expenses
    defaultLayout $ do
        case uid of
          Nothing -> do
            setTitle "Welcome To Expense Tracker!"
            $(widgetFile "welcome")
          Just usrid -> do
              setTitle "Expenses Home"
              $(widgetFile "homepage")


getAllUserExpenses :: Maybe UserId -> DB [(E.Value Double, E.Value UTCTime, E.Value Text)]
getAllUserExpenses Nothing      = return []
getAllUserExpenses (Just uid)     = do
    E.select
           $ E.from $ \(expense `E.InnerJoin` category) -> do
                E.on $ expense ^. ExpenseCategoryId E.==. category ^. CategoryId
                E.where_ (expense ^. ExpenseUserId E.==. E.val uid)
                return
                    ( expense   ^. ExpenseAmount
                    , expense   ^. ExpenseDate
                    , category  ^. CategoryName
                    )
