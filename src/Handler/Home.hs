{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
module Handler.Home where

import           Data.Aeson                           ((.=))
import qualified Data.Text                            as T
import           Data.Time.Calendar
import           Data.Time.Clock
import qualified Database.Esqueleto.Experimental      as E
import           Database.Esqueleto.Internal.Internal (unsafeSqlExtractSubField)
import           Import
import qualified Prelude                              as P
import           Text.Read
import           Utils


getPrevCurrNextMonths :: Maybe Text -> Maybe Text -> IO (Day, Day, Day)
getPrevCurrNextMonths maybeMonthraw maybeYearraw = do
    currTime <- getCurrentTime
    let monthMaybe = coerceMaybe $ readMaybe . T.unpack <$> maybeMonthraw
        yearMaybe = coerceMaybe $ readMaybe . T.unpack <$> maybeYearraw
        curr_ = mkDay <$> yearMaybe <*> monthMaybe <*> Just 1
        currMonth = case curr_ of
                Nothing -> utctDay currTime
                Just x  -> x
        prevMonth = addDays (-32) currMonth
        nextMonth = addDays 32 currMonth
    return (prevMonth, currMonth, nextMonth)


getHomeR :: Handler Html
getHomeR = do
    uid <- maybeAuthId
    mraw <- lookupGetParam "month"
    yraw <- lookupGetParam "year"
    (prev, curr, next) <- liftIO $ getPrevCurrNextMonths mraw yraw
    let (prevYear, prevMonth, _) = toGregorian prev
        (nextYear, nextMonth, _) = toGregorian next
    expenses <- runDB $ getAllUserExpenses uid curr
    let total = fmap P.sum $ sequence $ P.map fst3 expenses
    defaultLayout $ do
        case uid of
          Nothing -> do
            setTitle "Welcome To Expense Tracker!"
            $(widgetFile "welcome")
          Just _ -> do
              setTitle "Expenses Home"
              $(widgetFile "homepage")

getExpenseSummaryR :: Handler Value
getExpenseSummaryR = do
    uid <- maybeAuthId
    mraw <- lookupGetParam "month"
    yraw <- lookupGetParam "year"
    (_, curr, _) <- liftIO $ getPrevCurrNextMonths mraw yraw
    month_summary <- runDB $ getMonthAggregated uid curr
    category_summary <- runDB $ getCategoryAggregated uid curr
    let month_processed = map (extractValFromTuple_ id (fromMaybe 0)) month_summary
        cat_processed = map (extractValFromTuple_ id (fromMaybe 0)) category_summary
    returnJson $ object
        [ "category" .= toJSON cat_processed
        , "month" .= toJSON month_processed
        ]


getAllUserExpenses :: Maybe UserId -> Day -> DB [(E.Value Double, E.Value UTCTime, E.Value Text)]
getAllUserExpenses Nothing _       = return []
getAllUserExpenses (Just uid) utday =  E.select $ do
    (expense E.:& category) <-
        E.from $ E.table @Expense
        `E.InnerJoin` E.table @Category
        `E.on` (\(expense E.:& category) -> expense E.^. ExpenseCategoryId E.==. category E.^. CategoryId)
    E.where_ (expense E.^. ExpenseUserId E.==. E.val uid)
    E.where_ (month (expense E.^. ExpenseDate) E.==. E.val m)
    E.where_ (year (expense E.^. ExpenseDate) E.==. E.val (fromIntegral y))
    return
        ( expense   E.^. ExpenseAmount
        , expense   E.^. ExpenseDate
        , category  E.^. CategoryName
        )
    where month :: E.SqlExpr (E.Value UTCTime) -> E.SqlExpr (E.Value Int)
          month ts = unsafeSqlExtractSubField "month" ts
          year :: E.SqlExpr (E.Value UTCTime) -> E.SqlExpr (E.Value Int)
          year ts = unsafeSqlExtractSubField "year" ts
          (y, m, _) = toGregorian utday


type MonthDay = Int

getMonthAggregated :: Maybe UserId -> Day -> DB [(E.Value MonthDay, E.Value (Maybe Double))]
getMonthAggregated Nothing _ = return []
getMonthAggregated (Just uid) utday = E.select $ do
    expense <- E.from $ E.table @Expense
    let date' = unsafeSqlExtractSubField "day" (expense E.^. ExpenseDate)
    E.where_ (expense E.^. ExpenseUserId E.==. E.val uid)
    E.where_ (month (expense E.^. ExpenseDate) E.==. E.val m)
    E.where_ (year (expense E.^. ExpenseDate) E.==. E.val (fromIntegral y))
    E.groupBy date'
    let sum' = E.sum_ (expense E.^. ExpenseAmount)
    return
        ( date'
        , sum'
        )
    where month :: E.SqlExpr (E.Value UTCTime) -> E.SqlExpr (E.Value Int)
          month ts = unsafeSqlExtractSubField "month" ts
          year :: E.SqlExpr (E.Value UTCTime) -> E.SqlExpr (E.Value Int)
          year ts = unsafeSqlExtractSubField "year" ts
          (y, m, _) = toGregorian utday

getCategoryAggregated :: Maybe UserId -> Day -> DB[(E.Value Text, E.Value (Maybe Double))]
getCategoryAggregated Nothing _ = return []
getCategoryAggregated (Just uid) utday = E.select $ do
    (expense E.:& category) <-
        E.from $ E.table @Expense
        `E.InnerJoin` E.table @Category
        `E.on` (\(expense E.:& category) -> expense E.^. ExpenseCategoryId E.==. category E.^. CategoryId)
    E.where_ (expense E.^. ExpenseUserId E.==. E.val uid)
    E.where_ (month (expense E.^. ExpenseDate) E.==. E.val m)
    E.where_ (year (expense E.^. ExpenseDate) E.==. E.val (fromIntegral y))
    E.groupBy (category E.^. CategoryName)
    return
        ( category  E.^. CategoryName
        , E.sum_ (expense E.^. ExpenseAmount)
        )
    where month :: E.SqlExpr (E.Value UTCTime) -> E.SqlExpr (E.Value Int)
          month ts = unsafeSqlExtractSubField "month" ts
          year :: E.SqlExpr (E.Value UTCTime) -> E.SqlExpr (E.Value Int)
          year ts = unsafeSqlExtractSubField "year" ts
          (y, m, _) = toGregorian utday
