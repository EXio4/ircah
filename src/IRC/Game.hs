module IRC.Game where

import           IRC.Commands
import           IRC.Types
import qualified IRC.Raw as Raw



gameClient :: NickTracker -> Raw.IRC -> IO ()
gameClient tracker irc = do
        IRC.onIRC irc
        (\_ -> client tracker irc)
        (IRC.NickTracking.handlers tracker (\trk -> client trk irc) <>
        [IRC.onJOIN $ \user channel metadata next -> do
                let user' = userNick user
                case IRC.NickTracking.getUID tracker user' of
                     Just v | v == (uid 0)
                         -> IRC.cmd irc "WHO" [channel, "%na"]
                     _   -> IRC.cmd irc "WHO" [user'  , "%na"]
                next 
        ,IRC.onQUIT $ \user msg next -> do
                case IRC.NickTracking.getUID tracker (userNick user) of
                     Just v | v == (uid 0)
                         -> return ()
                     _   -> next   
        ,IRC.onChannelMsg $ \user channel msg next -> do
            case msg of
                 "me"   -> IRC.msg irc channel (T.pack (show (IRC.NickTracking.getUID tracker (userNick user))))
                 x      -> 
            client tracker irc
        ])
