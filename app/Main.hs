module Main where

import Database.SQLite.Simple
import Lib
import Control.Monad.Reader (runReaderT, void)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Crypto.Random
import Network.Event
import qualified Network.Distributed.Server as Server

buildR :: IO R
buildR =
    R <$> atomically newTChan <*> atomically newTChan <*> open "test.db" <*> getRandomBytes 32

main :: IO ()
main = do
    r <- buildR
    void $ forkIO (runReaderT Server.startServer r)
    startApp r
