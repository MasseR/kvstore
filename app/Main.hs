module Main where

import Database.SQLite.Simple
import Lib
import Control.Monad.Reader (runReaderT, void)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Network.Event
import qualified Network.Distributed.Server as Server

main :: IO ()
main = do
    r <- R <$> atomically newTChan <*> atomically newTChan <*> open "test.db"
    void $ forkIO (runReaderT Server.startServer r)
    startApp r
