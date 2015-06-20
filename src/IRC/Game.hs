module IRC.Game (runGame) where

import           IRC.Types
import qualified IRC.Client as IRC
import qualified IRC.Commands as IRC
import qualified IRC as IRC
import qualified IRC.Raw as Raw
import qualified IRC.NickTracking as Tracker
import           IRC.NickTracking (NickTracker, uid, UID)
import qualified Data.Bimap as BM
import           Data.Bimap (Bimap)
import           Data.Monoid
import           Control.Applicative
import qualified Data.Text as T

data Players = Players (Bimap UID Account)   -- should use Account(s) as players
    deriving (Show,Eq)

emptyPlayersList :: Players
emptyPlayersList = Players BM.empty
    
removePlayer :: UID -> Players -> Players
removePlayer uid (Players bmap) = Players $ BM.delete uid bmap

addPlayer :: UID -> Account -> Players -> Players
addPlayer uid acc (Players bmap) = Players $ BM.insert uid acc bmap
  
 
showPlayers :: NickTracker -> Players -> String
showPlayers tracker (Players bmap) = concatMap f (BM.keys bmap)
    where f uid = case Tracker.getNick tracker uid of
                    Nothing -> "Accountless " ++ show uid ++ ", "
                    Just  v -> show uid ++ " <=> " ++ show v ++ ", "
        
gameClient :: Players -> NickTracker -> Raw.IRC -> IO ()
gameClient players tracker irc = do
        IRC.onIRC irc
            (\_ -> gameClient players tracker irc)
            (Tracker.handlers tracker (\trk -> gameClient players trk irc)
                [Tracker.onAccountChange $ \trk uid new_acc next ->
                    gameClient (removePlayer uid players) trk irc
                ]
            <>
            [IRC.onJOIN $ \user channel metadata next -> do
                    let user' = userNick user
                    case Tracker.getUID tracker user' of
                        Just v | v == (uid 0)
                            -> do IRC.cmd irc "CS"  ["op", channel]
                                  IRC.cmd irc "WHO" [channel, "%na"]
                        _   -> IRC.cmd irc "WHO" [user'  , "%na"]
                    next 
            ,IRC.onQUIT $ \user msg next -> do
                    case Tracker.getUID tracker (userNick user) of
                        Just v | v == (uid 0)
                            -> return () -- bot quit
                        Just nick_uid -> gameClient (removePlayer nick_uid players) tracker irc
                        _             -> next   
            ,IRC.onPART $ \user channel msg next -> do
                    case Tracker.getUID tracker (userNick user) of
                        Just v | v == (uid 0) -> next -- bot parting
                        Just nick_uid -> gameClient (removePlayer nick_uid players) tracker irc
                        _             -> next   
            ,IRC.onKICK $ \user channel kicked msg next -> do
                    case Tracker.getUID tracker kicked of
                        Just v | v == (uid 0) -> do
                                IRC.cmd irc "JOIN" [channel] -- without trying any password, this should store the channels? but is this the right approach?
                                next -- how should we handle this? (the bot itself getting kicked, just rejoin?)
                        Just nick_uid -> gameClient (removePlayer nick_uid players) tracker irc
                        _             -> next 
            ,IRC.onChannelMsg $ \user channel msg next -> do
                case msg of
                    "!ping"    -> IRC.msg irc channel ("pong " <> userNick user)
                    "!join"    -> do
                            case Tracker.getUID tracker (userNick user) >>=
                                    \uid -> fmap (uid,) (Tracker.getAccount tracker uid) of
                                 Nothing -> do -- user w/o login
                                    IRC.msg irc channel (userNick user <> " should identify")
                                 Just (uid, acc) -> do
                                    IRC.msg irc channel (userNick user <> " joined") -- should check if there's already an user with that acc
                                    gameClient (addPlayer uid acc players) tracker irc
                    "!players" -> IRC.msg irc channel (T.pack (showPlayers tracker players))
                    x          -> return ()
                next
            ])

runGame :: Nick -> (Raw.IRC -> IO ())
runGame nick = gameClient emptyPlayersList (Tracker.defTracker nick)