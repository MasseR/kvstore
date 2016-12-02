module Network.Distributed.Server where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Network.Event
import Control.Monad.Reader
import Pipes.Network.TCP
import Pipes
-- import Control.Concurrent (forkIO)


startServer :: ReaderT R IO ()
startServer =
    listen HostAny "5000" $ \(s,_addr) -> forever $
    void $ ask >>= \r -> acceptFork s $ \(s',addr) -> runReaderT (server s' addr) r

server :: Socket -> SockAddr -> ReaderT R IO ()
server s addr = do
    liftIO $ putStrLn $ "Accepted connection from " ++ show addr
    chan <- asks inboundChan
    runEffect (chanToEvent chan >-> encoder >-> toSocketLazy s)

chanToEvent :: MonadIO m => TChan Event -> Producer Event m ()
chanToEvent chan = forever ((liftIO . atomically . readTChan $ chan) >>= yield)

eventToChan :: MonadIO m => TChan Event -> Consumer Event m ()
eventToChan chan = do
    event <- await
    liftIO . atomically . writeTChan chan $ event
