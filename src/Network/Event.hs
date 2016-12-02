{-# Language DeriveGeneric #-}
module Network.Event where

import GHC.Generics (Generic)
import Data.Binary
import Data.Binary.Get (ByteOffset)
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Pipes

type DecodeEvent = Either (ByteString, ByteOffset, String)
                          (ByteString, ByteOffset, Event)

data Event = Put Text Text deriving (Generic, Show)

instance Binary Event

type EventPipe m a = Pipe BS.ByteString Event m a

eventPipe :: Monad m => EventPipe m ()
eventPipe = await >>= decoder
    where
        decoder b =
            case decodeOrFail (B.fromChunks [b]) of
                 Left (_partial, _offset, _error) ->
                     await >>= \b' -> decoder (BS.concat [b,b'])
                 Right (remaining, _offset, event) -> do
                     yield event
                     decoder (B.toStrict remaining)
