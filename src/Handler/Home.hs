{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Handler.Home where

import qualified Data.Text                            as T
import           Database.Esqueleto                   ((^.))
import qualified Database.Esqueleto                   as E
import qualified Database.Esqueleto.Internal.Internal as E
import           Import
import qualified Prelude                              as P
import           Text.Read
import           Utils

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

data YearMonth = YearMonth Int Int

getHomeR :: Handler Html
getHomeR = do
    uid <- maybeAuthId
    maybeMonthraw <- lookupGetParam "month"
    maybeYearraw <- lookupGetParam "year"
    let monthMaybe = coerceMaybe $ readMaybe . T.unpack <$> maybeMonthraw
        yearMaybe = coerceMaybe $ readMaybe . T.unpack <$> maybeYearraw
        ym = YearMonth <$> yearMaybe <*> monthMaybe
    expenses <- runDB $ getAllUserExpenses uid ym
    let total = fmap P.sum $ sequence $ P.map fst3 expenses
    defaultLayout $ do
        case uid of
          Nothing -> do
            setTitle "Welcome To Expense Tracker!"
            $(widgetFile "welcome")
          Just usrid -> do
              setTitle "Expenses Home"
              $(widgetFile "homepage")


getAllUserExpenses :: Maybe UserId -> Maybe YearMonth -> DB [(E.Value Double, E.Value UTCTime, E.Value Text)]
getAllUserExpenses Nothing _       = return []
getAllUserExpenses (Just uid) ym   = do
    E.select
           $ E.from $ \(expense `E.InnerJoin` category) -> do
                E.on $ expense ^. ExpenseCategoryId E.==. category ^. CategoryId
                E.where_ (expense ^. ExpenseUserId E.==. E.val uid)
                yearMonthWhere expense
                return
                    ( expense   ^. ExpenseAmount
                    , expense   ^. ExpenseDate
                    , category  ^. CategoryName
                    )
    where month :: E.SqlExpr (E.Value UTCTime) -> E.SqlExpr (E.Value Int)
          month ts = E.unsafeSqlExtractSubField "month" ts
          year :: E.SqlExpr (E.Value UTCTime) -> E.SqlExpr (E.Value Int)
          year ts = E.unsafeSqlExtractSubField "year" ts
          yearMonthWhere expense = case ym of
                             Nothing -> E.where_ (E.val (1 :: Int) E.==. E.val (1 :: Int))
                             Just (YearMonth y m) -> do
                                 E.where_ (month (expense ^. ExpenseDate) E.==. E.val m)
                                 E.where_ (year (expense ^. ExpenseDate) E.==. E.val y)
