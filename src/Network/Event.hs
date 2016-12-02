{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
module Network.Event where

import GHC.Generics (Generic)
import Data.Binary
import Data.Binary.Get (ByteOffset)
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Pipes
import qualified Pipes.Prelude as P
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Control.Monad.Reader
import Database.SQLite.Simple (Connection)

data R = R { inboundChan :: TChan Event
           , outboundChan :: TChan Event
           , dbConnection :: Connection
           }

type DecodeEvent = Either (ByteString, ByteOffset, String)
                          (ByteString, ByteOffset, Event)

data Event = Put Text Text deriving (Generic, Show)

instance Binary Event

publishEvent :: (MonadReader R m, MonadIO m) => Event -> m ()
publishEvent event = asks inboundChan >>= \c -> liftIO $ atomically $ writeTChan c event

encoder :: Monad m => Pipe Event BS.ByteString m ()
encoder = P.map (B.toStrict . encode)

decoder :: Monad m => Pipe BS.ByteString Event m ()
decoder = await >>= decodeB
    where
        decodeB b =
            case decodeOrFail (B.fromChunks [b]) of
                 Left (_partial, _offset, _error) ->
                     await >>= \b' -> decodeB (BS.concat [b,b'])
                 Right (remaining, _offset, event) -> do
                     yield event
                     decodeB (B.toStrict remaining)
