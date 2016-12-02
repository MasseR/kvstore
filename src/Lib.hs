{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric   #-}
module Lib  where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.ByteString.Lazy (ByteString)
import Database.SQLite.Simple
import Servant
import Servant.Docs
import Control.Monad.Trans (liftIO)
import GHC.Generics (Generic)
import Data.Maybe (listToMaybe)
import Control.Monad.Reader
import Network.Event

data KeyVal = KeyVal
            { key :: Key
            , value :: Text
            } deriving (Eq, Show, Generic)

instance ToSample Text where
    toSamples _ = [("", "foo")]
instance ToSample KeyVal where
    toSamples _ = [("Example keyvalue", KeyVal "kalzu" "aaaa")]

instance ToCapture (Capture "key" Key) where
    toCapture _ = DocCapture "key" "Key to search with"

type Key = Text
$(deriveJSON defaultOptions ''KeyVal)


type RestAPI = "key" :> Capture "key" Key :> Get '[JSON] KeyVal
          :<|> "key" :> Capture "key" Key :> ReqBody '[JSON, PlainText] Text :> Put '[JSON] KeyVal

startApp :: R -> IO ()
startApp r = run 8080 (app r)

app :: R -> Application
app r = serve fullAPI (server r :<|> serveDocs)
    where
        serveDocs _ respond = respond $ responseLBS ok200 [plain] docsBS
        plain = ("Content-Type", "text/plain")

fullAPI :: Proxy (RestAPI :<|> "docs" :> Raw)
fullAPI = Proxy

api :: Proxy RestAPI
api = Proxy

apiDocs :: API
apiDocs = docs api

docsBS :: ByteString
docsBS = TL.encodeUtf8 . TL.pack . markdown $ apiDocs

getKey :: Key -> ReaderT R Handler KeyVal
getKey key = do
    conn <- asks dbConnection
    vals <- liftIO $ queryNamed conn "select key, name from keys where key=:key" [":key" := key]
    -- maybe (throwError err404) return (listToMaybe [KeyVal k v | (k,v) <- vals])
    maybe (undefined) return (listToMaybe [KeyVal k v | (k,v) <- vals])

putKey :: Key -> Text -> ReaderT R Handler KeyVal
putKey key val = do
    conn <- asks dbConnection
    liftIO $ withTransaction conn $ do
        executeNamed conn deleteSql [":key" := key]
        executeNamed conn insertSql [":key" := key, ":value" := val]
    publishEvent (Put key val)
    getKey key
    where
        deleteSql = "delete from keys where key=:key" 
        insertSql = "insert into keys (key, name) values (:key, :value)" 


server :: R -> Server RestAPI
server r = enter (runReaderTNat r) serverReader

serverReader :: ServerT RestAPI (ReaderT R Handler)
serverReader = getKey :<|> putKey
