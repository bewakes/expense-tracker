{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Models
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API

type ExpenseAPI =
         "expenses" :> Get '[JSON] [Expense]
    :<|> "expenses" :> ReqBody '[JSON] Expense :> Post '[JSON] (Maybe (Key Expense))

type UserAPI =
          "users" :> Get '[JSON] [User]
     :<|> "users" :> ReqBody '[JSON] Expense :> Post '[JSON] (Maybe (Key User))

type Api =
        "expenses" :> Get '[JSON] [Expense]
   :<|> "expenses" :> ReqBody '[JSON] Expense :> Post '[JSON] (Maybe (Key Expense))
   :<|> "users" :> Get '[JSON] [User]
   :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key User))


api :: Proxy Api
api = Proxy
