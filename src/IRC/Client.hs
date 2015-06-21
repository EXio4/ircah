module IRC.Client (connectToIRC, onIRC, onIRC_h, runSM) where

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
import           Common.StateMachine

connectToIRC :: IRCConfig -> IRC IO a -> IO a
connectToIRC (IRCConfig network port nick sasl channels) irc = do
        Raw.connectToIRC_raw network (fromIntegral port) worker
    where worker = do
            -- SASL LOGIN should go here
            cmd "USER"  ["x", "x", "x", "x"]
            cmd "NICK"  [T.pack nick]
            reader channels irc
        
reader :: Monad m => [ChannelCfg] -> IRC m a -> IRC m a
reader channels = Raw.mutateIRC Raw.irc_send recv
    where recv = do
              x <- Raw.irc_read
              case x of 
                    (Raw.Message _ _ (Raw.Command "PING") params) -> do
                        Raw.irc_send (Raw.Message Nothing Nothing (Raw.Command "PONG") params)
                    (Raw.Message _ _ (Raw.CmdNumber 376)  _) -> do
                        forM_ channels $ \(ChannelCfg ch pwd) -> 
                            Raw.irc_send (command "JOIN" (map T.pack (ch : (case pwd of
                                                                            Nothing -> []
                                                                            Just v  -> [v]))))
                    _ -> return ()
              return x

            
            
onIRC_h :: Monad m => Raw.Message -> Handler Raw.Message (IRC m) a -> IRC m a
onIRC_h x handler = run handler x
    
onIRC :: Monad m => Raw.Message -> (Raw.Message -> IRC m r) -> [Command Raw.Message (IRC m) r] -> IRC m r
onIRC msg fb cmds = onIRC_h msg (Handler cmds (Fallback fb)) 

runSM :: Monad m => SM Raw.Message (IRC m) st -> st -> IRC m b
runSM sm def = go def
    where go st = do
            x <- Raw.irc_read
            go =<< runStep sm x st