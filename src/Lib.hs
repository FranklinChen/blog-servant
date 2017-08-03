{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Data.Aeson.Types
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant



data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User

type API
  =   "users" :> Get '[JSON] [User]
  :<|> "firstUser" :> Get '[JSON] User

users :: Handler [User]
users =
  return
    [ User 1 "Isaac" "Newton"
    , User 2 "Albert" "Einstein"
    ]

firstUser :: Handler User
firstUser = head <$> users

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server
  = users
  :<|> firstUser


