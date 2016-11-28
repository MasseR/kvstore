{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings   #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Text (Text)
import Database.SQLite.Simple
import Servant
import Control.Monad.Trans (MonadIO, liftIO)

data KeyVal = KeyVal
            { key :: Key
            , value :: Text
            } deriving (Eq, Show)

type Key = Text
$(deriveJSON defaultOptions ''KeyVal)


type API = "key" :> Capture "key" Key :> Get '[JSON] [KeyVal]

startApp :: Connection -> IO ()
startApp conn = run 8080 (app conn)

app :: Connection -> Application
app conn = serve api (server conn)

api :: Proxy API
api = Proxy

getter :: MonadIO m => Connection -> Key -> m [KeyVal]
getter conn key = do
    vals <- liftIO $ queryNamed conn "select key, name from keys where name=:name" [":name" := key]
    liftIO $ print vals
    return [KeyVal k v | (k,v) <- vals]

server :: Connection -> Server API
server conn = getter conn

