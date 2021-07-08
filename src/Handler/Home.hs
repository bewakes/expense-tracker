{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Handler.Home where

import qualified Data.Text                            as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Database.Esqueleto                   ((^.))
import qualified Database.Esqueleto                   as E
import qualified Database.Esqueleto.Internal.Internal as E
import           Import
import qualified Prelude                              as P
import           Text.Read
import           Utils


-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
--

getHomeR :: Handler Html
getHomeR = do
    uid <- maybeAuthId
    maybeMonthraw <- lookupGetParam "month"
    maybeYearraw <- lookupGetParam "year"
    let monthMaybe = coerceMaybe $ readMaybe . T.unpack <$> maybeMonthraw
        yearMaybe = coerceMaybe $ readMaybe . T.unpack <$> maybeYearraw
        curr_ = mkDay <$> yearMaybe <*> monthMaybe <*> Just 1
    currTime <- liftIO getCurrentTime
    day <- case curr_ of
             Nothing -> return $ utctDay currTime
             Just x  -> return x
    let (prevYear, prevMonth, _) = toGregorian $ addDays (-30) day
        (nextYear, nextMonth, _) = toGregorian $ addDays 32 day
    liftIO $ print nextYear
    liftIO $ print nextMonth
    expenses <- runDB $ getAllUserExpenses uid day
    let total = fmap P.sum $ sequence $ P.map fst3 expenses
    defaultLayout $ do
        case uid of
          Nothing -> do
            setTitle "Welcome To Expense Tracker!"
            $(widgetFile "welcome")
          Just _ -> do
              setTitle "Expenses Home"
              $(widgetFile "homepage")


getAllUserExpenses :: Maybe UserId -> Day -> DB [(E.Value Double, E.Value UTCTime, E.Value Text)]
getAllUserExpenses Nothing _       = return []
getAllUserExpenses (Just uid) utday = do
    E.select
           $ E.from $ \(expense `E.InnerJoin` category) -> do
                E.on $ expense ^. ExpenseCategoryId E.==. category ^. CategoryId
                E.where_ (expense ^. ExpenseUserId E.==. E.val uid)
                E.where_ (month (expense ^. ExpenseDate) E.==. E.val m)
                E.where_ (year (expense ^. ExpenseDate) E.==. E.val (fromIntegral y))
                return
                    ( expense   ^. ExpenseAmount
                    , expense   ^. ExpenseDate
                    , category  ^. CategoryName
                    )
    where month :: E.SqlExpr (E.Value UTCTime) -> E.SqlExpr (E.Value Int)
          month ts = E.unsafeSqlExtractSubField "month" ts
          year :: E.SqlExpr (E.Value UTCTime) -> E.SqlExpr (E.Value Int)
          year ts = E.unsafeSqlExtractSubField "year" ts
          (y, m, _) = toGregorian utday
