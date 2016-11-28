{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric   #-}
module Lib
    ( startApp
    ) where

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
import Control.Monad.Trans (MonadIO, liftIO)
import GHC.Generics (Generic)

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


type RestAPI = "key" :> Capture "key" Key :> Get '[JSON] [KeyVal]

startApp :: Connection -> IO ()
startApp conn = run 8080 (app conn)

app :: Connection -> Application
app conn = serve fullAPI (server conn :<|> serveDocs)
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

getter :: MonadIO m => Connection -> Key -> m [KeyVal]
getter conn key = do
    vals <- liftIO $ queryNamed conn "select key, name from keys where name=:name" [":name" := key]
    liftIO $ print vals
    return [KeyVal k v | (k,v) <- vals]

server :: Connection -> Server RestAPI
server conn = getter conn

