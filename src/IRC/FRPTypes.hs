{-# LANGUAGE Arrows #-}
module IRC.FRPTypes where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Wire.Core
import           FRP.Netwire
import           Control.Wire
import qualified IRC.Raw.Types as Raw
import qualified IRC.Raw       as Raw
import           IRC
import           IRC.Client
import           IRC.Commands

type Input m a = (HasTime t s) => Wire s () (Raw.IRC m) () a


ircMessage :: Monad m => Input m Raw.Message
ircMessage = mkGen_ $ \_ -> fmap Right Raw.irc_read

wireEx :: Monad m => (Raw.Message -> Raw.IRC m ()) -> Wire s () (Raw.IRC m) Raw.Message ()
wireEx irc = mkGen_ (\msg -> fmap Right $ irc msg)
            
clockSes :: MonadIO m => Session (IRC m) (Timed NominalDiffTime ()) 
clockSes = clockSession_
        
f :: Monad m => Raw.Message -> Raw.IRC m ()
f rawmsg = onIRC rawmsg
                (\_ -> return ())
                [onChannelMsg $ \user channel msg next -> do
                    case msg of
                        "!ping" -> cmd "PRIVMSG" [channel, "pong"]
                        _       -> next
                ]
loopG :: Monad m => Wire s () (IRC m) Raw.Message () -> Session (IRC m) s -> IRC m ()
loopG wire s = do
    msg        <- Raw.irc_read
    (ds, s')   <- stepSession s
    (m, wire') <- stepWire wire ds (Right msg)
    case m of
        Left  () -> return ()    -- quit?
        Right () -> loopG wire' s'