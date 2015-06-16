{-# LANGUAGE OverloadedStrings #-}

module IRC.Raw.Network (connectToIRC_raw) where

import           IRC.Raw.Types
import           IRC.Raw.Parser
import           IRC.Raw.Serialize

import           Data.Monoid
import           Control.Monad
import           System.IO
import           Network.Socket (socketToHandle)
import           Network.Simple.TCP
import           Control.Concurrent.Chan
import           Control.Concurrent
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)

readerThread :: Handle -> Chan Message -> IO ()
readerThread handle ch = forever $ do
    x <- BS.hGetLine handle
    if BS.null x  -- empty line? 
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

connectToIRC_raw :: HostName -> Integer -> (IRC -> IO a) -> IO a
connectToIRC_raw hostname port fn = withSocketsDo $ do
        reader <- newChan
        writer <- newChan
        connect hostname (show port) $ \(socket, addr) -> do
            handle <- socketToHandle socket ReadWriteMode
            kill1 <- forkIO $ readerThread handle reader
            kill2 <- forkIO $ writerThread handle writer
            x <- fn (IRC reader writer)
            mapM_ killThread [kill1, kill2]
            return x
            