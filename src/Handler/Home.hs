{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
module Handler.Home where

import           Data.Maybe
import qualified Data.Text                       as T
import           Data.Time.Calendar
import           Data.Time.Clock
import qualified Database.Esqueleto.Experimental as E
import           Import
import qualified Prelude                         as P
import           Text.Read
import           Utils.Data
import           Utils.Db                        hiding (DB)


parseParams :: Maybe Text -> Maybe Text ->  IO (Day, Day, Day)
parseParams maybeMonthraw maybeYearraw = do
    currTime <- getCurrentTime
    let monthMaybe = maybeMonthraw >>= readMaybe . T.unpack
        yearMaybe = maybeYearraw >>= readMaybe . T.unpack
        curr_ = mkDay <$> yearMaybe <*> monthMaybe <*> Just 1
        currMonth = case curr_ of
                Nothing -> utctDay currTime
                Just x  -> x
        prevMonth = addDays (-32) currMonth
        nextMonth = addDays 32 currMonth
    return (prevMonth, currMonth, nextMonth)

getHomeR :: Handler Html
getHomeR = do
    uidMaybe <- maybeAuthId
    mraw <- lookupGetParam "month"
    yraw <- lookupGetParam "year"
    (prev, curr, next) <- liftIO $ parseParams mraw yraw
    let (prevYear, prevMonth, _) = toGregorian prev
        (nextYear, nextMonth, _) = toGregorian next
    case uidMaybe of
      Nothing -> defaultLayout $ do
        setTitle "Welcome To Expense Tracker!"
        $(widgetFile "welcome")
      _ -> loginRedirectOr $ \(UserInfo _ grp _) -> do
        expenses <- runDB $ getAllGroupExpenses (entityKey grp) curr
        let grpId = E.fromSqlKey (entityKey grp)

        let total = P.sum <$> mapM fst6 expenses
        defaultLayout $ do
          setTitle "Expenses Home"
          $(widgetFile "homepage")

getExpenseSummaryR :: Handler Value
getExpenseSummaryR = do
    mraw <- lookupGetParam "month"
    yraw <- lookupGetParam "year"
    (_, curr, _) <- liftIO $ parseParams mraw yraw
    (mgrp, _) <- getUserCurrentGroupFromParam
    case mgrp of
      Nothing -> returnJson $ object []
      Just grp -> do
        month_summary <- runDB $ getMonthAggregated (entityKey grp) curr
        category_summary <- runDB $ getCategoryAggregated (entityKey grp) curr
        let month_processed = map (extractValFromTuple_ id (fromMaybe 0)) month_summary
            cat_processed = map (extractValFromTuple_ id (fromMaybe 0)) category_summary
            total = P.sum $ P.map snd cat_processed
        returnJson $ object
            [ "category_data" .= toJSON cat_processed
            , "month_data" .= toJSON month_processed
            , "month" .= formatTime defaultTimeLocale "%B" curr
            , "total" .= total
            ]
