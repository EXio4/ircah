module IRC.Client (connectToIRC, connectToIRC'with, mutateIRC_cfg) where

import           Control.Applicative
import           Control.Monad
import           Data.Function
import           IRC.Types
import           IRC.Commands
import qualified IRC.Raw               as Raw
import           IRC.Raw.Types (IRC)
import           Data.ByteString (ByteString)
import           Control.Concurrent
import           Control.Concurrent.Chan
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T
import           Control.Monad.IO.Class

mutateIRC_cfg :: Monad m => IRCConfig -> IRC m a -> IRC m a
mutateIRC_cfg (IRCConfig network port (Nick nick) sasl channels) irc = do
        cmd "USER"  [nick, "x", "x", "hasky"]
        cmd "NICK"  [nick]
        reader channels irc

connectToIRC :: IRCConfig -> IRC IO a -> IO a
connectToIRC= connectToIRC'with id

connectToIRC'with :: MonadIO m => (forall a. m a -> IO a) -> IRCConfig -> IRC m a -> IO a
connectToIRC'with conv cfg@(IRCConfig network port nick sasl channels) irc = do
        Raw.connectToIRC_raw's network (fromIntegral port) $ \i ->
            conv $ Raw.runIRC (Raw.nirc_send i) (Raw.nirc_read i) (mutateIRC_cfg cfg irc)
        
reader :: Monad m => [ChannelCfg] -> IRC m a -> IRC m a
reader channels = Raw.mutateIRC Raw.irc_send recv
    where recv = do
              x <- Raw.irc_read
              case x of 
                    (Raw.Message _ _ (Raw.Command "PING") params) -> do
                        Raw.irc_send (Raw.Message Nothing Nothing (Raw.Command "PONG") params)
                    (Raw.Message _ _ (Raw.CmdNumber 376)  _) -> do
                        forM_ channels $ \(ChannelCfg (Channel ch) pwd) -> 
                            Raw.irc_send (command "JOIN" (ch : map T.pack (case pwd of
                                                                            Nothing -> []
                                                                            Just v  -> [v])))
                    _ -> return ()
              return x

