module IRC.Game (setupPlayTracking, emptyPlayersList) where

import           IRC.Types
import qualified IRC.Client as IRC
import qualified IRC.Commands as IRC
import qualified IRC as IRC
import qualified IRC.Raw as Raw
import qualified IRC.NickTracking as Tracker
import           IRC.NickTracking (NickTracker, UID)
import qualified Data.Bimap as BM
import           Data.Bimap (Bimap)
import           Data.Monoid
import           Control.Applicative
import qualified Data.Text as T
import           Data.Text (Text)
import           Common.StateMachine (SM)
import qualified Common.StateMachine as SM

data Players = Players (Bimap UID Account)   -- should use Account(s) as players
    deriving (Show,Eq)

emptyPlayersList :: Players
emptyPlayersList = Players BM.empty
    
removePlayer :: UID -> Players -> Players
removePlayer uid (Players bmap) = Players $ BM.delete uid bmap

replacePlayer :: UID -> Account -> Players -> Players
replacePlayer uid acc (Players bmap) = Players $ BM.insert uid acc (BM.deleteR acc bmap)

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
-}

game :: Monad m => Channel -> Raw.Message -> (NickTracker, Players) -> IRC m Players
game gameChannel raw_message (tracker, players) = 
            IRC.onIRC raw_message
                (\_ -> pure players)
                [IRC.onJOIN $ \user channel metadata next -> do
                        let user' = userNick user
                        case Tracker.getUID tracker user' of
                            Just v | v == Tracker.uid 0
                                -> do IRC.cmd "CS"  ["op", channel]
                                      IRC.cmd "WHO" [channel, "%na"]
                            _   -> IRC.cmd "WHO" [user'  , "%na"]
                        next 
                ,IRC.onQUIT $ \user msg next -> leaveGameHandler (userNick user) next
                ,IRC.onPART $ \user channel msg next -> do
                        case Tracker.getUID tracker (userNick user) of
                            Just v | v == Tracker.uid 0 -> next -- bot parting
                            Just nick_uid -> leaveGameHandler_uid nick_uid next
                            _             -> next   
                ,IRC.onKICK $ \user channel kicked msg next -> do
                        case Tracker.getUID tracker kicked of
                            Just v | v == Tracker.uid 0 -> do
                                    IRC.cmd "JOIN" [channel] -- without trying any password, this should store the channels? but is this the right approach?
                                    next -- how should we handle this? (the bot got kicked, just rejoin?)
                            Just nick_uid -> leaveGameHandler_uid nick_uid next
                            _             -> next 
                ,IRC.onChannelMsg $ \user channel msg next -> do
                    let nick = userNick user
                    case T.words msg of
                        ["!ping"]  -> IRC.msg channel (nick <> ": pong!") *> next
                        ["!quit"]  -> leaveGameHandler nick next
                        ["!join"]  | channel == gameChannel -> joinGameHandler nick next 
                        ["!players"] -> IRC.msg channel ("Players: " <> showPlayers tracker players) *> next
                        x            -> next
    
                ]
    where
          joinGameHandler nick next =
              case Tracker.getUID tracker nick >>= \uid -> fmap (uid,) (Tracker.getAccount tracker uid) of
                    Nothing -> do -- user w/o login
                        IRC.msg gameChannel (nick <> " should identify")
                        next
                    Just (uid, acc) -> case checkAcc acc players of
                            Just v | v == uid -> do
                                IRC.msg gameChannel (nick <> " you are already joined")
                                next
                            Just v  -> do
                                IRC.msg gameChannel (nick <> " was already joined, replacing old player")
                                return (replacePlayer uid acc players)
                            Nothing -> do
                                IRC.msg gameChannel (nick <> " joined")
                                return (addPlayer uid acc players)
          leaveGameHandler nick next = 
                case Tracker.getUID tracker nick of
                     Nothing  -> next -- untracked nick? 
                     Just uid -> leaveGameHandler_uid uid next
          leaveGameHandler_uid uid next
                | uid == Tracker.uid 0 = next -- it's ourselves
                | otherwise = case checkUID uid players of
                        Nothing -> next -- he's not playing
                        Just _  -> case Tracker.getNick tracker uid of
                                 Nothing   -> do
                                    IRC.msg gameChannel ("invariant broken , UID " <> T.pack (show uid) <> " not found in tracker")
                                    next
                                 Just nick -> do
                                    IRC.msg gameChannel (nick <> " left the game!")
                                    return (removePlayer uid players)

setupPlayTracking :: (Applicative m, Monad m) => Channel -> SM Raw.Message (IRC m) NickTracker -> SM Raw.Message (IRC m) (NickTracker, Players)
setupPlayTracking gameChannel tracker = SM.liftF tracker (game gameChannel)
                