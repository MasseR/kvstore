module Network.Distributed.Server where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Network.Event
import Control.Monad.Reader
import Pipes.Network.TCP
import Pipes
import Control.Concurrent (forkIO)

data R = R { inboundChan :: TChan Event
           , outboundChan :: TChan Event
           }

startServer :: ReaderT R IO ()
startServer = do
    (s,_addr) <- bindSock HostAny "127.0.0.1"
    void $ ask >>= \r -> acceptFork s $ \(s',addr) -> runReaderT (server s' addr) r

server :: Socket -> SockAddr -> ReaderT R IO ()
server s addr = do
    liftIO $ putStrLn $ "Accepted connection from " ++ show addr
    chan <- asks inboundChan
    runEffect (chanToEvent chan >-> encoder >-> toSocket s)

chanToEvent :: MonadIO m => TChan Event -> Producer Event m ()
chanToEvent chan = (liftIO . atomically . readTChan $ chan) >>= yield

eventToChan :: MonadIO m => TChan Event -> Consumer Event m ()
eventToChan chan = do
    event <- await
    liftIO . atomically . writeTChan chan $ event
