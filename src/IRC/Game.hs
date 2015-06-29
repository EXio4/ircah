{-# LANGUAGE NoMonomorphismRestriction #-}
module IRC.Game where

import           IRC.Types
import qualified IRC.Client as IRC
import qualified IRC.Commands as IRC
import qualified IRC as IRC
import qualified IRC.Raw as Raw
import qualified IRC.Raw as IRC (irc_read, irc_send)
import qualified IRC.NickTracking as Tracker
import           IRC.Raw.Types (IRC)
import           Common.Types
import           Data.Dynamic
import           Control.Monad
import           Data.Monoid
import           Control.Applicative
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Map.Strict as M
import qualified Data.Bimap as BM
import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.Set as S
import           Control.Lens
import           System.Random
import           CAH.Cards.Types
import           Translate (translate)
import           Control.Monad.Trans.Free
import qualified Control.Monad.Ether.Reader as ER
import qualified Control.Monad.Ether.State.Strict as ES
import           Control.Ether.TH


emptyPlayersList :: Players md
emptyPlayersList = Players BM.empty M.empty

withMap :: (Map Account md -> a) -> Players md -> a
withMap f (Players _ m) = f m
    
removePlayer :: Account -> Players md -> Players md
removePlayer acc pl@(Players bmap md) = Players (BM.deleteR acc bmap) (M.delete acc md)

-- given an UID, give that user a new account
replacePlayer :: Show md => UID -> Account -> Players md -> Players md
replacePlayer uid acc pl@(Players bmap md) = do
    let old_acc = BM.lookup uid bmap
    case old_acc of
         Nothing      -> pl -- new account, we'd need to "create" metadata for keeping invariants
         Just old_acc -> do
             let md' = M.delete old_acc md
             let pv  = M.lookup old_acc md
             case pv of
                  Nothing -> error "[BROKEN INVARIANT!] we're replacing a player, yet that player has no metadata, shouldn't happen"
                  Just pv -> Players (BM.insert uid acc (BM.deleteR acc bmap)) (M.insert acc pv md')

nickChange :: Show md => UID -> Account -> Players md -> Players md
nickChange uid acc (Players bmap md) = Players x md
    where x = BM.insert uid acc bmap
                  
addPlayer :: Show md => Nick -> Account -> md -> NickTracker -> Players md -> Players md
addPlayer nick acc meta nicktracker (Players bmap md) = do
    let player_metadata = M.lookup   acc md
    case player_metadata of
         Nothing ->
            case Tracker.getUID nick nicktracker of
                Just uid -> Players (BM.insert uid acc bmap) (M.insert acc meta md)
                Nothing  -> error "[BROKEN INVARIANT] trying to add nick w/o UID, we aren't tracking him .. (yet we have his account, somehow?)"
         Just xs -> error ("[BROKEN INVARIANT!] addPlayer assumed new player was being added, but there's some garbage \n" ++ show xs)
         
checkAcc :: Account -> Players md -> Maybe UID
checkAcc acc (Players bmap _) = BM.lookupR acc bmap
 
checkUID :: UID -> Players md -> Maybe Account
checkUID uid (Players bmap _) = BM.lookup uid bmap
 
checkNick :: Nick -> Players md -> NickTracker -> Maybe Account
checkNick nick (Players bmap _) tracker = Tracker.getUID nick tracker >>= \uid -> BM.lookup uid bmap

uidFromNick :: Nick -> Players md -> NickTracker -> Maybe UID 
uidFromNick nick players tracker = checkNick nick players tracker >>= \acc -> checkAcc acc players
         
nickFromAcc :: Account -> Players md -> NickTracker -> Maybe Nick
nickFromAcc acc p trk = checkAcc acc p >>= \uid -> Tracker.getNick uid trk
         
nickOf :: (TrackerMonad m, GameMonad r m) => Account -> m (Maybe Nick)
nickOf acc = do
    pls <- ES.gets gameState (^.players)
    trk <- ES.get  nickTracker 
    return $ nickFromAcc acc pls trk

nickPlaying :: (TrackerMonad m, GameMonad r m) => Nick -> m (Maybe Account)
nickPlaying nick = do
    acc <- accOf nick
    case acc of
         Nothing -> return Nothing
         Just acc -> do
            nick' <- nickOf acc
            case nick' of
                 Nothing -> return Nothing
                 Just _  -> return (Just acc)
    
accOf :: TrackerMonad m => Nick -> m (Maybe Account)
accOf nick = Tracker.getAccFromNick nick `liftM` ES.get nickTracker
         
getMetadata :: Account -> Players md -> Maybe md
getMetadata acc (Players _ md) = M.lookup acc md
 
showPlayers' :: NickTracker -> Players md -> [Nick]
showPlayers' tracker (Players bmap _) = map f (BM.toList bmap)
    where f (uid, Account acc) = Nick (case Tracker.getNick uid tracker of
                    Nothing -> T.pack (show uid) <> " (ghost, acc = " <> acc <> ") "
                    Just (Nick v) -> v <> " (" <> acc <> ")")
    
playersList :: (TrackerMonad m, GameMonad r m) => m [Nick]
playersList = showPlayers' `liftM` ES.get   nickTracker `ap` ES.gets gameState (^.players)
    
funpack :: [Pack] -> (Set WhiteCard, Set BlackCard)
funpack [] = (S.empty, S.empty)
funpack ((Pack _ w b):xs) = 
        (S.union w w', S.union b b')
    where ~(w', b') = funpack xs


randomNum :: (GameMonad r m, Random a) => (a, a) -> m a
randomNum xy = do
    st <- ES.get gameState
    let (v, g') = randomR xy (st^.stdGen)
    ES.put gameState (st & stdGen .~ g')
    return v
    


addPoints :: (GameMonad (Current wf) m) => Account -> Points -> m ()
addPoints acc ps = do
        ES.modify gameState (\gs -> gs & current.points %~ f)
    where f = M.insertWith (+) acc ps
    
takeCardF :: (GameMonad s m, Ord card) => Lens' (GS s) (Set card) -> Lens' (Set WhiteCard, Set BlackCard) (Set card) -> m card
takeCardF main recover = do
    let f x | S.size x < 5 = do
                cards <- liftM funpack (ER.ask cardSet)
                return (cards^.recover)
            | otherwise = return x
    x <- ES.get gameState >>= \w -> f (w^.main)
    y <- randomNum (0, S.size x - 1)
    let el = S.elemAt y x
    ES.modify gameState (\gs -> gs & main .~ S.delete el x)
    return el  
    
takeBCard :: GameMonad s m => m BlackCard
takeBCard = takeCardF blackCards _2

takeWCards :: GameMonad s m => Int -> m (Set WhiteCard)
takeWCards 0 = return S.empty
takeWCards n = do
    el <- takeCardF whiteCards _1
    S.insert el `liftM` takeWCards (n-1)
  

         
getBlackCardBeingPlayed :: GameMonad (Current wf) m => m BlackCard
getBlackCardBeingPlayed = do
    x <- ES.get gameState
    return (x^.current.blackCard)
    


initialState :: Set WhiteCard -> Set BlackCard -> GS NoGame
initialState white black = GS {
     _stdGen        = mkStdGen 42
    ,_whiteCards    = white
    ,_blackCards    = black
    ,_players       = emptyPlayersList
    ,_current       = NoGame
}

startState :: Player -> BlackCard -> [Player] -> Current WFPlayers
startState czar black rest = Current { 
      _points    = M.fromList []
     ,_czar      = czar
     ,_blackCard = black
     ,_currentB  = WFPlayers {
         _waitingFor    = S.fromList rest
        ,_alreadyPlayed = M.fromList []
     }
}

            
mainGame :: (Raw.MonadIRC m, EventsMonad m, TrackerMonad m, GameMonad x m, Functor m) => (LogicCommand -> m ()) -> Nick -> Channel -> m ()
mainGame runLogic botNick gameChannel = forever loop
    where loop = do
            raw_msg <- IRC.irc_read
            Tracker.trackerSM raw_msg
            case raw_msg of
                IRC.JOIN user (Channel channel) metadata -> do
                        let user'@(Nick nick) = userNick user
                        tracker <- ES.get nickTracker 
                        case Tracker.getUID user' tracker of
                            Just v | v == Tracker.uid 0 -> do
                                     IRC.cmd "CS" ["op", channel]
                                     IRC.cmd "WHO" [channel, "%na"]
                            _     -> IRC.cmd "WHO" [nick , "%na"]
                IRC.CHMSG user channel msg
                        | gameChannel == channel
                        , Just (cmd, args) <- cmds '!' msg -> do
                            logCmd <- cmd user channel args
                            case logCmd of
                                 Left NoError    -> return ()
                                 Left x -> toChannel x
                                 Right x -> runLogic x 
                _ -> return ()
    
          toChannel x = IRC.msg gameChannel (Message (translate x))
          toUser nick x = IRC.msg nick (Message (translate x))
          cmdList = [("ping"   , pongHandler)
                    ,("join"   , joinHandler)
                    ,("part"   , partHandler)
                    ,("players", playersHandler)
                    ]
          
          cmds prefix (Message (T.uncons -> Just (p, (T.words -> (command:params)))))
                    | p == prefix
                    = (,params) <$> lookup command cmdList
          cmds _ _  = Nothing
          
          
          joinHandler user channel args = 
              accOf (userNick user) >>= \case 
                Nothing  -> return (Left (UserNotIdentified (userNick user)))
                Just acc  -> nickPlaying (userNick user) >>= \case
                    Just _   -> return $ Left (AlreadyPlaying (userNick user))
                    Nothing -> do
                        toChannel (JoinPlayer (userNick user))
                        return $ Right (PlayerJoin acc)
                       

          partHandler user channel args = do
              accM <- nickPlaying (userNick user)
              case accM of
                   Nothing  -> return $ Left (UserNotPlaying (userNick user))
                   Just acc  -> do
                       toChannel (LeavePlayer (userNick user))
                       return $ Right (PlayerLeave acc)
                       
          playersHandler user channel args = do
              fmap (Left . PlayersList) playersList
          
          pongHandler user channel args = do
              IRC.msg channel "pong!"
              return (Left NoError)
          
game :: (Raw.MonadIRC m, EventsMonad m, TrackerMonad m, GameMonad x m, Functor m) => Nick -> Channel -> m ()
game = mainGame (const $ return ())
         
stateGame :: [Pack] -> GS NoGame
stateGame packs  = 
        let (white,black) = funpack packs
        in initialState white black
        
runGame :: IRCConfig -> [Pack] -> Game NoGame IO x -> IO x
runGame cfg packs irc0 = do
        let irc1 = ES.evalStateT  gameState   irc0 (stateGame packs)
            irc2 = ES.evalStateT  events      irc1  emptyEvents
            irc3 = ES.evalStateT  nickTracker irc2 (Tracker.defTracker (config_nick cfg))
            irc4 = ER.runReaderT  cardSet     irc3  packs
        IRC.connectToIRC cfg irc4
        
