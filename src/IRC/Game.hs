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
import           Data.Maybe
import           Control.Monad
import           Data.Monoid
import           Control.Applicative
import qualified Data.Traversable as TV
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Text.Read as TxRead
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

readMaybe :: Read a => Text -> Maybe a
readMaybe = TxRead.readMaybe . T.unpack 

emptyPlayersList :: Players md
emptyPlayersList = Players BM.empty M.empty

withMap :: (Map Account md -> a) -> Players md -> a
withMap f (Players _ m) = f m

set_lookup :: Ord a => Int -> Set a -> Maybe a
set_lookup n set | n >= 0 && n < S.size set = Just (S.elemAt n set)
                 | otherwise                = Nothing

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
          
meta_update :: (md -> md) -> Account -> Players md -> Players md
meta_update f acc (Players bmap md) = Players bmap (M.update (Just . f) acc md)
                  
addPlayer'pure :: Show md => Nick -> Account -> md -> NickTracker -> Players md -> Players md
addPlayer'pure nick acc meta nicktracker (Players bmap md) = do
    let player_metadata = M.lookup   acc md
    case player_metadata of
         Nothing ->
            case Tracker.getUID nick nicktracker of
                Just uid -> Players (BM.insert uid acc bmap) (M.insert acc meta md)
                Nothing  -> error "[BROKEN INVARIANT] trying to add nick w/o UID, we aren't tracking him .. (yet we have his account, somehow?)"
         Just xs -> error ("[BROKEN INVARIANT!] addPlayer assumed new player was being added, but there's some garbage \n" ++ show xs)

delPlayer'pure :: Account -> Players md -> Players md
delPlayer'pure acc pl@(Players bmap md) = Players (BM.deleteR acc bmap) (M.delete acc md)
         
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
    
addPlayer :: (TrackerMonad m, GameMonad r m) => Nick -> Account -> Set WhiteCard -> m ()
addPlayer nick acc cards = do
    trk <- ES.get  nickTracker
    ES.modify gameState $ \gs -> do
        gs & players %~ addPlayer'pure nick acc cards trk
        
delPlayer :: (GameMonad r m) => Nick -> Account -> m ()
delPlayer nick acc = do
    ES.modify gameState $ \gs -> do
        gs & players %~ delPlayer'pure acc

nickPlaying :: (TrackerMonad m, GameMonad r m) => Nick -> m (Maybe Account)
nickPlaying nick = do
    accM <- accOf nick
    case accM of
         Nothing  -> return Nothing
         Just acc -> fmap (acc <$) (nickOf acc)
    
accOf :: TrackerMonad m => Nick -> m (Maybe Account)
accOf nick = Tracker.getAccFromNick nick `liftM` ES.get nickTracker
         
getMetadata :: Account -> Players md -> Maybe md
getMetadata acc (Players _ md) = M.lookup acc md
 
showPlayers' :: NickTracker -> Players md -> [Nick]
showPlayers' tracker (Players bmap _) = map f (BM.toList bmap)
    where f (uid, Account acc) = Nick (case Tracker.getNick uid tracker of
                    Nothing -> T.pack (show uid) <> " (ghost, acc = " <> acc <> ") "
                    Just (Nick v) -> v <> " (" <> acc <> ")")
    
playersPrettyList :: (TrackerMonad m, GameMonad r m) => m [Nick]
playersPrettyList = showPlayers' `liftM` ES.get   nickTracker `ap` ES.gets gameState (^.players)
    
playersList :: (GameMonad r m) => m (Map Account (Set WhiteCard))
playersList = fmap (withMap id) (ES.gets gameState (^.players))
    
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
  
getTable :: GameMonad (Current wf) m => m (Player, BlackCard)
getTable = ES.gets gameState $ \gs ->
            (gs^.current.czar, gs^.current.blackCard)

getCzar :: GameMonad (Current wf) m => m Player
getCzar = fmap fst getTable
        
getCard :: GameMonad x m => Account -> Int -> m (Maybe WhiteCard)
getCard acc n = do
    mp <- fmap (withMap id) $ ES.gets gameState (^.players)
    return $ M.lookup acc mp >>= set_lookup (n-1)
            
    
holesBlackCard :: GameMonad (Current wf) m => m Int
holesBlackCard = fmap (countHoles . snd) getTable
    
removeCards :: GameMonad x m => Account -> [Int] -> m ()
removeCards acc xs = ES.modify gameState (& players %~ meta_update f acc)
    where f set = S.fromList [ w | (n,w) <- zip [1..] (S.toList set), n `notElem` xs ] 
    
        
getBlackCard :: GameMonad (Current wf) m => m BlackCard
getBlackCard = ES.gets gameState (^.current.blackCard)


initialState :: Set WhiteCard -> Set BlackCard -> GS NoGame
initialState white black = GS {
     _stdGen        = mkStdGen 42
    ,_whiteCards    = white
    ,_blackCards    = black
    ,_players       = emptyPlayersList
    ,_current       = NoGame
}

startingGameWith :: Map Player (Set WhiteCard) -> BlackCard -> Current WFPlayers
startingGameWith players black = do
    let (czar:players') = fmap fst $ M.toList players
    Current {
        _points = M.fromList []
        ,_czar  = czar
        ,_blackCard = black
        ,_currentB = WFPlayers {
              _waitingFor    = S.fromList players'
             ,_alreadyPlayed = M.fromList []
        }
    }

            
switchToCzar :: [(Integer, (Player, [WhiteCard]))] -> Current WFPlayers -> Current WFCzar
switchToCzar xs old =
    old { _currentB = WFCzar {
                _picks = M.fromList xs
            }
        }
        
    
toIRC x = do
    channel <- ER.reader gameInfo (^.gameChannel)
    case x of
         YourCardsAre nick _ ->
                IRC.notice nick    (Message (translate x))
         _ ->   IRC.msg    channel (Message (translate x))

mainGame :: (Raw.MonadIRC m, EventsMonad m, TrackerMonad m, GameMonad x m, Functor m) =>
                    (LogicCommand (ToLogicTag x) -> m ()) ->
                    [(Text, User -> Maybe Account -> Channel -> [Text] -> m (Either TextMessage (LogicCommand (ToLogicTag x))))] ->
                    m ()
mainGame runLogic mcmds = loop
    where loop = do
            raw_msg <- IRC.irc_read
            Tracker.trackerSM raw_msg
            gameChannel <- ER.reader gameInfo (^.gameChannel) 
            case raw_msg of
                IRC.JOIN user (Channel channel) metadata -> do
                        let user'@(Nick nick) = userNick user
                        tracker <- ES.get nickTracker 
                        case Tracker.getUID user' tracker of
                            Just v | v == Tracker.uid 0 -> do
                                     IRC.cmd "CS" ["op", channel]
                                     IRC.cmd "WHO" [channel, "%na"]
                            _     -> IRC.cmd "WHO" [nick , "%na"]
                        loop
                IRC.CHMSG user channel msg
                        | gameChannel == channel
                        , Just (cmd, args) <- cmds '!' msg -> do
                            accM <- accOf (userNick user)
                            logCmd <- cmd user accM channel args
                            case logCmd of
                                 Left NoError    -> loop
                                 Left x -> toIRC x >> loop
                                 Right x -> runLogic x 
                _ -> loop

          cmdList = [("ping"   , pongHandler)
                    ,("join"   , joinHandler)
                    ,("part"   , partHandler)
                    ,("players", playersHandler)
                    ,("myacc"  , myAccHandler)
                    ] ++ mcmds
          
          cmds prefix (Message (T.uncons -> Just (p, (T.words -> (command:params)))))
                    | p == prefix
                    = (,params) <$> lookup command cmdList
          cmds _ _  = Nothing
          
          
          joinHandler user acc channel args = 
              case acc of
                Nothing  -> return (Left (UserNotIdentified (userNick user)))
                Just acc  -> nickPlaying (userNick user) >>= \case
                    Just _  -> return $ Left (AlreadyPlaying (userNick user))
                    Nothing -> do
                        toIRC (JoinPlayer (userNick user))
                        return $ Right (PlayerJoin (userNick user) acc)
                       

          partHandler user _ channel args = do
              accM <- nickPlaying (userNick user)
              case accM of
                   Nothing  -> return $ Left (UserNotPlaying (userNick user))
                   Just acc  -> do
                       toIRC (LeavePlayer (userNick user))
                       return $ Right (PlayerLeave (userNick user) acc)
                       
          playersHandler user acc channel args = do
              fmap (Left . PlayersList) playersPrettyList
          
          pongHandler user acc channel args = do
              IRC.msg channel "pong!"
              return (Left NoError)
          myAccHandler user acc channel args = do
              IRC.msg channel $ "your account is " <> (Message (T.pack (show acc)))
              return (Left NoError)
        
handlePlayerTracking :: (Raw.MonadIRC m, EventsMonad m, TrackerMonad m, GameMonad x m, Functor m) => LogicCommand x' -> m ()
handlePlayerTracking lcmd = do
    case lcmd of
            PlayerJoin nick acc -> do
                cards <- takeWCards 10
                addPlayer nick acc cards
            PlayerLeave nick acc -> do
                delPlayer nick acc
            _ -> return () -- not a join/part

getPicks :: (GameMonad (Current WFPlayers) m) => m [(Integer, (Player, [WhiteCard]))]
getPicks = (zip [1..] . M.toList) <$> ES.gets gameState (^.current.currentB.alreadyPlayed)

tableHandlerC :: Monad m => User -> Maybe Account -> Channel -> [Text] -> Game (Current wf) m (Either TextMessage r)
tableHandlerC user acc channel txt = Left <$> tableHandler 
          
tableHandler :: Monad m => Game (Current wf) m TextMessage
tableHandler = do
    (czar, black) <- getTable
    czarNick <- nickOf czar
    return $ case czarNick of
         Nothing -> Debug "czar's nick wasn't found"
         Just cz -> Table cz black
          
noGame :: Monad m => Game NoGame m ()
noGame = mainGame gameHandler [("start", startHandler)]
    where startHandler user _ channel args = do
            pls <- playersPrettyList
            accM <- nickPlaying (userNick user)
            if | length pls < 3 -> return $ Left  (NotEnoughPlayers 3)
               | Just acc <- accM -> do
                   toIRC (GameStarted pls)
                   return $ Right (StartGame (userNick user) acc)
               | otherwise ->
                    return $ Left (UserNotPlaying (userNick user))
          gameHandler p = do
              handlePlayerTracking p
              pls <- playersList
              case p of
                   StartGame _ _ -> do
                       black <- takeBCard
                       changing (\s -> s & current .~ startingGameWith pls black) (do
                                toIRC =<< tableHandler
                                gameRunning)
                   _ -> noGame
              
gameRunning :: Monad m => Game (Current WFPlayers) m ()
gameRunning = mainGame gameHandler [("pick"  , pickHandler )
                                   ,("cards" , cardsHandler)
                                   ,("start" , startHandler)
                                   ,("table" , tableHandlerC)]
    where pickHandler user Nothing    channel args = do
                return (Left (UserNotPlaying (userNick user)))
          pickHandler user (Just acc) channel argsTxt = do
                waitList  <- ES.gets gameState (^.current.currentB.waitingFor)
                cardHoles <- holesBlackCard 
                czar      <- getCzar
                if | acc `S.member` waitList -> do
                        if | args <- mapMaybe readMaybe argsTxt, cardHoles == length args -> do
                                    x <- fmap TV.sequence (mapM (getCard acc) args)
                                    case x of
                                        Nothing  -> return (Left (MustPickNCards (userNick user) (fromIntegral cardHoles)))
                                        Just cds -> do
                                            removeCards acc args
                                            ES.modify gameState (\gs -> 
                                                    gs & current.currentB.waitingFor %~ S.delete acc
                                                       & current.currentB.alreadyPlayed %~ M.insert acc cds)
                                            return (Right (PlayerPick (userNick user) acc cds))
                           | otherwise -> return (Left (MustPickNCards (userNick user) (fromIntegral cardHoles)))
                   | czar == acc -> return (Left (CzarDoesn'tPlay (userNick user)))
                   | otherwise   -> return (Left (AlreadyPlayed (userNick user)))

          cardsHandler user accM channel args = do
                x <- playersList
                return . Left $
                        if | Just acc <- accM, Just cds <- M.lookup acc x ->
                              YourCardsAre (userNick user) (zip [1..] (S.toList cds))
                           | otherwise -> UserNotPlaying (userNick user)
          startHandler user acc channel args = do
                return (Left GameAlreadyBeingPlayed)
          gameHandler p = do
              handlePlayerTracking p
              pls <- playersPrettyList
              cz  <- getCzar
              waitList <- ES.gets gameState (^.current.currentB.waitingFor)
              black <- getBlackCard
              if | length pls < 3 -> do
                    toIRC (NotEnoughPlayers 3)
                    changing (\s -> s & current .~ NoGame) noGame
                    -- we deleted all the previous info, we should print the points n stuff
                 | PlayerLeave nick acc <- p , cz == acc -> do
                    toIRC (CzarLeft nick)
                    gameRunning
                 | PlayerLeave nick acc <- p, acc `S.member` waitList -> do
                     -- if we got there, it means this player ALREADY left and we didn't force the game to end, we "pass" thru
                     ES.modify gameState (& current.currentB.waitingFor %~ S.delete acc)
                 | otherwise -> return ()
              waitList <- ES.gets gameState (^.current.currentB.waitingFor) -- it might have been updated 
              if | S.null waitList -> do
                            s <- getPicks
                            forM_ s $ \(n, (p, cds)) ->
                                nickOf p >>= \case
                                    Nothing -> return ()
                                    Just nk -> toIRC (CardsPicked nk n black cds)
                            changing (fmap (switchToCzar s)) czarGame
                 | otherwise -> gameRunning
                    

czarGame :: Monad m => Game (Current WFCzar) m ()
czarGame = mainGame gameHandler [("pick"  , pickHandler )
                                ,("table" , tableHandlerC)]
    where pickHandler user (Just acc) channel args = do
                czar <- getCzar
                if | czar /= acc -> return (Left NoError)
                   | otherwise   -> do -- the czar picked
                        return (Left NoError)
          pickHandler user Nothing channel args = do
              return (Left NoError)
    
          gameHandler p = do
            czarGame
                   
                   
stateGame :: [Pack] -> GS NoGame
stateGame packs  = 
        let (white,black) = funpack packs
        in initialState white black
        
runGame :: IRCConfig -> Channel -> [Pack] -> Game NoGame IO x -> IO x
runGame cfg ch packs irc0 = do
        let gi = GameInfo {
                     _gameChannel = ch
                    ,_botNick     = config_nick cfg
                }
        let irc1 = ES.evalStateT  gameState   irc0 (stateGame packs)
            irc2 = ES.evalStateT  events      irc1  emptyEvents
            irc3 = ES.evalStateT  nickTracker irc2 (Tracker.defTracker (config_nick cfg))
            irc4 = ER.runReaderT  cardSet     irc3  packs
            irc5 = ER.runReaderT  gameInfo    irc4  gi
        IRC.connectToIRC cfg irc5
        
