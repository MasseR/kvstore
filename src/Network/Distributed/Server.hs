module Network.Distributed.Server where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Network.Event
import Control.Monad.Reader
import Pipes.Network.TCP
import Pipes

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
    chan <- asks outboundChan >>= liftIO . atomically . dupTChan
    runEffect (fromSocket s 4096 >-> eventPipe >-> eventToChan chan)

eventToChan :: MonadIO m => TChan Event -> Consumer Event m ()
eventToChan chan = do
    event <- await
    liftIO . atomically . writeTChan chan $ event
