{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
module Utils.Db where

import           ClassyPrelude.Yesod
import qualified Database.Esqueleto.Experimental      as E
import           Database.Esqueleto.Internal.Internal (unsafeSqlExtractSubField)

import           Model

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadUnliftIO m) => ReaderT SqlBackend m a


getUserGroups :: UserId -> DB [Entity Group]
getUserGroups u = E.select $ do
    (ug E.:& grp) <-
        E.from $ E.Table @UsersGroups `E.InnerJoin` E.Table @Group
        `E.on` (\(ug E.:& grp) -> ug E.^. UsersGroupsGroupId E.==. grp E.^. GroupId)
    E.where_ (ug E.^. UsersGroupsUserId E.==. E.val u)
    E.orderBy [E.asc $ ug E.^. UsersGroupsIsDefault]
    return grp

getAllGroupExpenses :: GroupId -> Day -> DB [(E.Value Double, E.Value Day, E.Value Text, E.Value Text, E.Value (E.Key Expense), E.Value Text)]
getAllGroupExpenses gid utday =  E.select $ do
    (expense E.:& category E.:& user) <-
        E.from $ E.Table @Expense
        `E.InnerJoin` E.Table @Category
        `E.on` (\(expense E.:& category) -> expense E.^. ExpenseCategoryId E.==. category E.^. CategoryId)
        `E.InnerJoin` E.Table @User
        `E.on` (\(e E.:& _ E.:& u) -> e E.^. ExpenseCreatedBy E.==. u E.^. UserId)
    E.where_ (expense E.^. ExpenseGroupId E.==. E.val gid)
    E.where_ (month (expense E.^. ExpenseDate) E.==. E.val m)
    E.where_ (year (expense E.^. ExpenseDate) E.==. E.val (fromIntegral y))
    E.orderBy [E.asc (expense E.^. ExpenseDate)]
    return
        ( expense   E.^. ExpenseAmount
        , expense   E.^. ExpenseDate
        , category  E.^. CategoryName
        , expense E.^. ExpenseDescription
        , expense E.^. ExpenseId
        , user E.^. UserFirstName
        )
    where month :: E.SqlExpr (E.Value Day) -> E.SqlExpr (E.Value Int)
          month ts = unsafeSqlExtractSubField "month" ts
          year :: E.SqlExpr (E.Value Day) -> E.SqlExpr (E.Value Int)
          year ts = unsafeSqlExtractSubField "year" ts
          (y, m, _) = toGregorian utday


type MonthDay = Int

getMonthAggregated :: GroupId -> Day -> DB [(E.Value MonthDay, E.Value (Maybe Double))]
getMonthAggregated gid utday = E.select $ do
    expense <- E.from $ E.Table @Expense
    let date' = unsafeSqlExtractSubField "day" (expense E.^. ExpenseDate)
    E.where_ (expense E.^. ExpenseGroupId E.==. E.val gid)
    E.where_ (month (expense E.^. ExpenseDate) E.==. E.val m)
    E.where_ (year (expense E.^. ExpenseDate) E.==. E.val (fromIntegral y))
    E.groupBy date'
    let sum' = E.sum_ (expense E.^. ExpenseAmount)
    return
        ( date'
        , sum'
        )
    where month :: E.SqlExpr (E.Value Day) -> E.SqlExpr (E.Value Int)
          month ts = unsafeSqlExtractSubField "month" ts
          year :: E.SqlExpr (E.Value Day) -> E.SqlExpr (E.Value Int)
          year ts = unsafeSqlExtractSubField "year" ts
          (y, m, _) = toGregorian utday

getCategoryAggregated :: GroupId -> Day -> DB [(E.Value Text, E.Value (Maybe Double))]
getCategoryAggregated gid utday = E.select $ do
    (expense E.:& category) <-
        E.from $ E.Table @Expense
        `E.InnerJoin` E.Table @Category
        `E.on` (\(expense E.:& category) -> expense E.^. ExpenseCategoryId E.==. category E.^. CategoryId)
    E.where_ (expense E.^. ExpenseGroupId E.==. E.val gid)
    E.where_ (month (expense E.^. ExpenseDate) E.==. E.val m)
    E.where_ (year (expense E.^. ExpenseDate) E.==. E.val (fromIntegral y))
    E.groupBy (category E.^. CategoryName)
    return
        ( category  E.^. CategoryName
        , E.sum_ (expense E.^. ExpenseAmount)
        )
    where month :: E.SqlExpr (E.Value Day) -> E.SqlExpr (E.Value Int)
          month ts = unsafeSqlExtractSubField "month" ts
          year :: E.SqlExpr (E.Value Day) -> E.SqlExpr (E.Value Int)
          year ts = unsafeSqlExtractSubField "year" ts
          (y, m, _) = toGregorian utday

getAllGroupCategories :: GroupId -> DB [Entity Category]
getAllGroupCategories gid = E.select $ do
    category <- E.from $ E.Table @Category
    E.where_ (category E.^. CategoryGroupId E.==. E.val gid)
    return category

getCategoryForUser :: CategoryId -> UserId -> GroupId -> DB (Maybe (Entity Category))
getCategoryForUser catid uid gid = do
    cats <- E.select $ do
        (ug E.:& category) <-
            E.from $ E.Table @UsersGroups
            `E.InnerJoin` E.Table @Category
            `E.on` (\(ug E.:& c) -> ug E.^. UsersGroupsGroupId E.==. c E.^. CategoryGroupId)
        E.where_ (category E.^. CategoryId E.==. E.val catid)
        E.where_ (ug E.^. UsersGroupsUserId E.==. E.val uid)
        E.where_ (category E.^. CategoryGroupId E.==. E.val gid)
        return category
    case cats of
      []   -> pure Nothing
      x: _ -> pure $ Just x

getExpenseForUser :: ExpenseId -> UserId -> GroupId -> DB (Maybe (Entity Expense))
getExpenseForUser eid uid gid = do
    exps <- E.select $ do
        (ug E.:& expense) <-
            E.from $ E.Table @UsersGroups
            `E.InnerJoin` E.Table @Expense
            `E.on` (\(ug E.:& e) -> ug E.^. UsersGroupsGroupId E.==. e E.^. ExpenseGroupId)
        E.where_ (expense E.^. ExpenseId E.==. E.val eid)
        E.where_ (ug E.^. UsersGroupsUserId E.==. E.val uid)
        E.where_ (expense E.^. ExpenseGroupId E.==. E.val gid)
        return expense
    case exps of
      []   -> pure Nothing
      x: _ -> pure $ Just x
