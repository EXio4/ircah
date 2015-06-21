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
import           Data.Text (Text)

data Players = Players (Bimap UID Account)   -- should use Account(s) as players
    deriving (Show,Eq)

emptyPlayersList :: Players
emptyPlayersList = Players BM.empty
    
removePlayer :: UID -> Players -> Players
removePlayer uid (Players bmap) = Players $ BM.delete uid bmap

addPlayer :: UID -> Account -> Players -> Players
addPlayer uid acc (Players bmap) = Players $ BM.insert uid acc bmap
  
checkAcc :: Account -> Players -> Maybe UID
checkAcc acc (Players bmap) = BM.lookupR acc bmap
 
checkUID :: UID -> Players -> Maybe Account
checkUID uid (Players bmap) = BM.lookup uid bmap
 
checkNick :: Nick -> Players -> NickTracker -> Maybe Account
checkNick nick (Players bmap) tracker = Tracker.getUID tracker nick >>= \uid -> BM.lookup uid bmap

uidFromNick :: Nick -> Players -> NickTracker -> Maybe UID 
uidFromNick nick players tracker = checkNick nick players tracker >>= \acc -> checkAcc acc players
         
 
showPlayers :: NickTracker -> Players -> Text
showPlayers tracker (Players bmap) = loop $ map f (BM.toList bmap)
    where f (uid, acc) = case Tracker.getNick tracker uid of
                    Nothing -> T.pack (show uid) <> " (ghost, acc = " <> T.pack (show acc) <> ") "
                    Just  v -> v <> " (" <> acc <> ")"
          loop []     = ""
          loop [x]    = x
          loop (x:xs) = x <> ", " <> loop xs
          
{-
gameClient :: Players -> NickTracker -> Raw.IRC -> IO ()
gameClient players tracker irc = do
        IRC.onIRC irc
            (\_ -> gameClient players tracker irc)
            (Tracker.handlers tracker (\trk -> gameClient players trk irc)
                [Tracker.onAccountChange $ \trk uid new_acc next ->
                    gameClient (removePlayer uid players) trk irc
                ]
                <> 
            ([IRC.onJOIN $ \user channel metadata next -> do
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
                        Just nick_uid -> 
                                gameClient (removePlayer nick_uid players) tracker irc
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
                case T.words msg of
                    ["!ping"]  -> IRC.msg irc channel ("pong " <> userNick user)
                    ["!quit"]  ->
                        case uidFromNick (userNick user) players tracker of
                             Nothing  -> IRC.msg irc channel (userNick user <> ": you are not joined!")
                             Just uid -> do
                                 IRC.msg irc channel (userNick user <> " quit the game")
                                 gameClient (removePlayer uid players) tracker irc
                    ["!join"] -> do
                            case Tracker.getUID tracker (userNick user) >>=
                                    \uid -> fmap (uid,) (Tracker.getAccount tracker uid) of
                                 Nothing -> do -- user w/o login
                                    IRC.msg irc channel (userNick user <> " should identify")
                                 Just (uid, acc) -> 
                                    case checkAcc acc players of
                                         Just v -> IRC.msg irc channel (userNick user <> ": you are already joined!")
                                         Nothing -> do
                                            IRC.msg irc channel (userNick user <> " joined")
                                            gameClient (addPlayer uid acc players) tracker irc
                    ["!players"] -> IRC.msg irc channel ("Players: " <> (showPlayers tracker players))
                    x            -> return ()
                next
            ])
    
-}

runGame :: Nick -> IRC m ()
runGame nick = undefined -- gameClient emptyPlayersList (Tracker.defTracker nick)