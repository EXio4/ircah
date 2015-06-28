module IRC.Raw.Network (connectToIRC_raw,connectToIRC_raw's, nirc_read,nirc_send) where

import           IRC.Raw.Types
import           IRC.Raw.Parser
import           IRC.Raw.Serialize
import           IRC.Raw.Monad

import           Data.Monoid
import           Control.Monad
import           Control.Monad.IO.Class
import           System.IO
import           Network.Socket (socketToHandle)
import           Network.Simple.TCP
import           Control.Concurrent.Chan
import           Control.Concurrent
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import           Control.Monad.IO.Class

readerThread :: Handle -> Chan Message -> IO ()
readerThread handle ch = forever $ do
    x <- BS.hGetLine handle
    if BS.null x  -- ignoring empty lines, why this should ever happen though
    then return ()
    else case parseIRC (x <> "\n") of
         (_, Right m) -> writeChan ch m
         (remg, Left (context, msg)) -> do
             let line xs = let ln = "------------" in putStrLn (ln ++ " " ++ xs ++ " " ++ ln)
             line "Error parsing input from IRC (ignoring line)"
             line "Remaining input"
             print remg
             line "Context" 
             mapM_ (\x -> putStr "\t" >> putStrLn x) context
             line "Error message"
             putStrLn msg
             line "ORIGINAL LINE"
             print x 
             line "END"
             
writerThread :: Handle -> Chan Message -> IO ()
writerThread handle ch = forever $ do
    y <- readChan ch
    BS.hPutStr handle (serialize y)

connectToIRC_raw :: HostName -> Integer -> IRC IO a -> IO a
connectToIRC_raw hostname port irc = connectToIRC_raw's hostname port $ \i -> do
    runIRC (nirc_send i) (nirc_read i) irc
            
connectToIRC_raw's :: HostName -> Integer -> (IRC_Connection -> IO a) -> IO a
connectToIRC_raw's hostname port irc = do
        reader <- newChan
        writer <- newChan
        connect hostname (show port) $ \(socket, addr) -> do
            handle <- socketToHandle socket ReadWriteMode
            kill1 <- forkIO $ readerThread handle reader
            kill2 <- forkIO $ writerThread handle writer
            let i = IRC_Connection reader writer
            x <- irc i
            mapM_ (killThread) [kill1, kill2]
            return x
            
nirc_read :: MonadIO m => IRC_Connection -> m Message
nirc_send :: MonadIO m => IRC_Connection -> Message -> m ()
nirc_read (IRC_Connection r _)   = liftIO $ readChan  r
nirc_send (IRC_Connection _ w) m = liftIO $ writeChan w m