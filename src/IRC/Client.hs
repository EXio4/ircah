{-# LANGUAGE OverloadedStrings #-}
module IRC.Client (connectToIRC, onIRC, onIRC_h) where

import           Control.Applicative
import           Control.Monad
import           Data.Function
import           IRC.Types
import           IRC.Commands
import qualified IRC.Raw               as Raw
import           Data.ByteString (ByteString)
import           Control.Concurrent
import           Control.Concurrent.Chan
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T

connectToIRC :: IRCConfig -> (IRC -> IO a) -> IO a
connectToIRC (IRCConfig network port nick sasl channels) fn = do
        Raw.connectToIRC_raw network (fromIntegral port) irc
    where irc i = do
            -- SASL LOGIN should go here
            Raw.irc_send i (command "USER"  ["x", "x", "x", "x"])
            Raw.irc_send i (command "NICK"  [T.pack nick])
            reader i channels fn
            
reader :: IRC -> [ChannelCfg] -> (IRC -> IO a) -> IO a
reader irc channels clientFn = fix $ \loop -> do
    x <- Raw.irc_read irc
    case x of 
         (Raw.Message _ _ (Raw.Command "PING") params) -> do
            Raw.irc_send irc (Raw.Message Nothing Nothing (Raw.Command "PONG") params)
            loop
         (Raw.Message _ _ (Raw.CmdNumber 376)  _) -> do
            forM_ channels $ \(ChannelCfg ch pwd) -> 
                Raw.irc_send irc (command "JOIN" (T.pack ch : (case pwd of
                                                                Nothing -> []
                                                                Just v  -> [T.pack v])))
            next 
         _ -> loop
    where next = do
            (r,w) <- (,) <$> newChan <*> newChan
            let irc'client = IRC r w
            forkIO $ forever $ do
                x <- Raw.irc_read irc
                case x of
                    (Raw.Message _ _ (Raw.Command "PING") params) ->
                        Raw.irc_send irc (Raw.Message Nothing Nothing (Raw.Command "PONG") params)
                    rest -> writeChan r rest
            forkIO $ forever (Raw.irc_send irc =<< readChan w)
            clientFn irc'client
            
            
            
onIRC_h :: IRC -> Handler IO a -> IO a
onIRC_h irc handler = do
    x <- Raw.irc_read irc
    run handler x
    
onIRC :: IRC -> (Raw.Message -> IO ()) -> [Command IO ()] -> IO ()
onIRC irc fb cmds = onIRC_h irc (Handler cmds (Fallback fb)) 