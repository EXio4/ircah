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
import           Common.Commands
import           Common.Types
import           Data.Dynamic
import           Control.Monad
import           Data.Monoid
import           Control.Applicative
import qualified Data.Text as T
import           Data.Text (Text)
import           Control.Monad.State (MonadState, StateT, evalStateT)
import qualified Data.Map.Strict as M
import qualified Data.Bimap as BM
import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.Set as S
import           Control.Lens
import           System.Random
import           CAH.Cards.Types
import           Control.Monad.Trans.Free

data EV r where
    EV :: Typeable a => (a -> r) -> EV r

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
         
nickOf :: MonadState (r, Game) m => Account -> m (Maybe Nick)
nickOf acc = do
    pls <- use (state.players)
    trk (\trk -> nickFromAcc acc pls trk)

accOf :: MonadState (r, Game) m => Nick -> m (Maybe Account)
accOf nick = do
    trk (\trk -> Tracker.getAccFromNick nick trk)
         
getMetadata :: Account -> Players md -> Maybe md
getMetadata acc (Players _ md) = M.lookup acc md
 
showPlayers' :: NickTracker -> Players md -> Text
showPlayers' tracker (Players bmap _) = T.intercalate ", " (map f (BM.toList bmap))
    where f (uid, acc) = case Tracker.getNick uid tracker of
                    Nothing -> T.pack (show uid) <> " (ghost, acc = " <> T.pack (show acc) <> ") "
                    Just  v -> v <> " (" <> acc <> ")"
    
showPlayers :: (MonadState (r, Game) m) => m Text
showPlayers = showPlayers' `liftM` use (state.nickTracker) `ap` use (state.players)
    
state = _2

runEvents :: (MonadState (Events, r) m, Typeable a) => (a -> m (Maybe b)) -> m (Maybe b)
runEvents f = do
    evs <- use _1 
    let pv = popEvent evs
    case pv of
         Nothing     -> return Nothing
         Just (v,rs) -> do
             x <- f v
             _1 .= case x of 
                  Just _  -> rs
                  Nothing -> evs
             return x
    
onEvents :: (MonadState (Events, r) m) => [EV (m a)] -> m ()
onEvents [] = return ()
onEvents (EV f:xs) = runEvents (\v -> liftM Just (f v)) >> onEvents xs

funpack :: [Pack] -> (Set WhiteCard, Set BlackCard)
funpack [] = (S.empty, S.empty)
funpack ((Pack _ w b):xs) = 
        (S.union w w', S.union b b')
    where ~(w', b') = funpack xs

randomNum :: (MonadState (r, Game) m, Random a) => (a, a) -> m a
randomNum xy = do
    g <- use (state.stdGen)
    let (v, g') = randomR xy g
    state.stdGen .= g'
    return v
    
boolL'M :: MonadState s m => Getting a s a -> (a -> m Bool) -> m r -> m r -> m r
boolL'M lens cond t f = do
    x <- use lens
    c <- cond x
    if c
    then t
    else f

boolL :: MonadState s m => Getting a s a -> (a -> Bool) -> m r -> m r -> m r
boolL lens cond t f = boolL'M lens (return . cond) t f

isPlaying    :: MonadState (r, Game) m => Player -> m x -> m x -> m x
isNotPlaying :: MonadState (r, Game) m => Player -> m x -> m x -> m x
isNotPlaying = flip . isPlaying
isPlaying player = boolL (state.players) (withMap $ M.member player)

trk :: MonadState (r, Game) m => (NickTracker -> a) -> m a
trk = (`liftM` (use (state . nickTracker)))

numPlayers :: MonadState (r, Game) m => m Int
numPlayers = uses (state.players) s
    where s (Players bmap md) = do
            let (bsize, msize) = (BM.size bmap, M.size md)
            if | bsize == msize -> bsize
               | otherwise      -> error "[BROKEN INVARIANT] (found in numPlayers) internal player maps should have same length"

is :: Monad m => m a -> (a -> Bool) -> m () -> m ()
is x f r = do
    y <- f `liftM` x 
    if y
    then r
    else return ()

    
isCzar :: Monad m => Account -> IRC (StateT (Events, Game) m) r -> (Lens' (Events,Game) (Set WhiteCard) -> IRC (StateT (Events, Game) m) r) -> IRC (StateT (Events, Game) m) r
isCzar acc true false = do
    return undefined
    {-
    x <- use (state.current.table._2)
    case x of
        Just c | c == acc -> true
        _ -> do 
            let viewL x =
                    case M.lookup acc (x & state.players) of
                         Nothing -> error "broken invariant (accesing cards of non existant player)"
                         Just  x -> x
            let setL x v = state.players %~ S.insert acc v 
            let ln = lens viewL setL
            false ln -}
    

takeCardF :: (MonadState (r, Game) m, Ord card) => Lens' Game (Set card) -> Lens' (Set WhiteCard, Set BlackCard) (Set card) -> m card
takeCardF main recover = do
    let f x | S.size x < 5 = use (state.allCards.recover)
            | otherwise = return x
    x <- f =<< use (state.main)
    y <- randomNum (0, S.size x - 1)
    let el = S.elemAt y x
    state.main .= S.delete el x
    return el  
    
takeBCard :: MonadState (r, Game) m => m BlackCard
takeBCard = takeCardF blackCards _2

takeWCards :: MonadState (r, Game) m => Int -> m (Set WhiteCard)
takeWCards 0 = return S.empty
takeWCards n = do
    el <- takeCardF whiteCards _1
    S.insert el `liftM` takeWCards (n-1)
    
showBlack :: BlackCard -> Text
showBlack (BlackCard x) = T.concat (map f x)
    where f (Txt x) = x
          f VisibleHole   = "____"
          f InvisibleHole = ""
          
getBlackCardBeingPlayed :: IRC (StateT (Events,Game) m) BlackCard
getBlackCardBeingPlayed = do
    x <- use (state.current._Just._table._1)
    case x of
         Nothing -> error "no game is being played"
         Just v  -> return v
         


initialState :: Set WhiteCard -> Set BlackCard -> Nick -> Game
initialState white black nick = GS {
     _stdGen        = mkStdGen 42
    ,_whiteCards    = white
    ,_blackCards    = black
    ,_allCards      = (white,black)
    ,_players       = emptyPlayersList
    ,_nickTracker   = Tracker.defTracker nick
    ,_current       = Nothing
}

startState :: Player -> BlackCard -> [Player] -> Current
startState czar black rest = Current {
      _points        = M.fromList []
     ,_table         = (black,czar)
     ,_currS = Right (WaitingForPlayers {
         _waitingFor    = S.fromList rest
        ,_alreadyPlayed = M.fromList []
     })
}

addPoints :: Monad m => Account -> Points -> IRC (StateT (Events, Game) m) ()
addPoints acc ps = state.current._Just.points %= f
    where f = M.insertWith (+) acc ps
            
game :: forall m. Monad m => Nick -> Channel -> IRC (StateT (Events, Game) m) ()
game botNick gameChannel = forever loop
    where loop = do
            raw_msg <- IRC.irc_read
            Tracker.trackerSM raw_msg
            onEvents
                [EV $ \x -> 
                    let _ = (x :: TrackEvent)
                    in return ()
                ]
            IRC.onIRC raw_msg
                (\_ -> pure ())
                [IRC.onJOIN $ \user channel metadata next -> do
                        let user' = userNick user
                        tracker <- use (state.nickTracker)
                        case Tracker.getUID user' tracker of
                            Just v | v == Tracker.uid 0 -> do
                                     IRC.cmd "CS" ["op", channel]
                                     IRC.cmd "WHO" [channel, "%na"]
                            _     -> IRC.cmd "WHO" [user' , "%na"]
                ,IRC.onChannelMsg $ \user channel msg next -> do
                    if channel /= gameChannel
                    then next 
                    else cmds user msg next '!'
                      [("join"   , joinHandler)
                      ,("leave"  , partHandler)
                      ,("players", playersHandler)
                      ,("cards"  , cardsHandler)
                      ,("start"  , startHandler)
                      ,("status" , statusHandler)
                      ]
                ]
                
         
          newCzar :: Account -> IRC (StateT (Events,Game) m) ()
          newCzar acc = do
              x <- use state
              pls <- fmap (filter (/= acc) . map fst . withMap M.toList) $ use (state.players)
              card <- takeBCard
              case x^.current of
                   Nothing -> IRC.msg gameChannel "there's no game"
                   Just c  -> do
                       let c' = c & table .~ (card, acc)
                                  & currS .~ (Right (WaitingForPlayers {
                                          _waitingFor    = S.fromList pls
                                         ,_alreadyPlayed = M.fromList []                                        
                                       }))
                       state.current._Just .= c'
                                  
        
          nextCzar :: Account -> IRC (StateT (Events,Game) m) ()
          nextCzar currentCzar = do
              x <- use state
              case x^.current of
                   Nothing -> IRC.msg gameChannel "we're trying to assing this game a new czar, yet there's no game..."
                   Just c  -> do
                       let p = withMap (map fst . M.toList) (x^.players)
                       case (p, takeWhile (/= currentCzar) p) of
                            (_, _:nick:ys) -> newCzar nick
                            (nick:_, _)    -> newCzar nick                            
                            (_,_ )         -> IRC.msg gameChannel "we couldn't find a new czar, wut happened?"
          
          stateStep :: IRC (StateT (Events,Game) m) ()
          stateStep = do
              x <- use (state.current)
              case x of
                   Just c ->
                    case (c^.currS) of
                        Left czar | Just int <- (czar^.picked) ->
                            case M.lookup int (czar^.picks) of
                                 Nothing -> do
                                     IRC.msg gameChannel "card out of bounds"
                                 Just (acc, WhiteCard whiteCard) -> do
                                     nick <- fmap (maybe acc id) $ nickOf acc
                                     IRC.msg gameChannel (nick <> " won this round with the card " <> whiteCard)
                                     addPoints acc 1
                                     nextCzar (c^.table^._2)
                        Right players | S.null (players^.waitingFor) -> do
                            IRC.msg gameChannel "everyone picked and their cards are:"
                            let mp = zip [1..] (M.toList (players^.alreadyPlayed))
                            state.current._Just.currS .= Left (WaitingForCzar {
                                     _picks  = M.fromList mp
                                    ,_picked = Nothing
                            })
                            forM_ mp $ \(n, (p, WhiteCard c)) -> do
                                IRC.msg gameChannel (T.pack (show n) <> ": " <> c)
                        _ -> do
                            -- nothing that interests us happened
                            return ()
          
          joinPlayer :: Nick -> Account -> Set WhiteCard -> IRC (StateT (Events,Game) m) ()
          joinPlayer nick acc crds = do
                IRC.msg gameChannel (nick <> " joined")
                trk <- use (state.nickTracker)
                IRC.cmd "MODE" [gameChannel, "+v", nick]
                state.players %= addPlayer nick acc crds trk 
    
    
          replace :: Nick -> Account -> IRC (StateT (Events,Game) m) ()
          replace nick acc = do
              IRC.msg gameChannel (nick <> " replacing his previous account")
              pls <- use (state.players)
              case checkAcc acc pls of
                   Nothing -> do
                       IRC.msg gameChannel (nick <> " .. but there isn't a previous account, wtf is going on?")
                   Just uid -> do
                      state.players %= nickChange uid acc
    
          deletePlayer :: Account -> IRC (StateT (Events,Game) m) ()
          deletePlayer acc = do
               nick <- fmap (maybe ("GHOST :: " <> acc) id) (nickOf acc)
               IRC.msg gameChannel (nick <> " left")
               IRC.cmd "MODE" [gameChannel, "-v", nick]
               state.players %= removePlayer acc
               curr <- use (state.current)
               case curr of
                    Nothing -> return ()
                    Just c | c^.table._2 {- czar -} == acc ->czarFled
                    Just c | Right v <- c^.currS
                           , acc `S.member` (v^.waitingFor) ->
                                state.current._Just.currS._Right.waitingFor %= S.delete acc
                    _ -> return ()
                        -- if the player is not in the "waiting" list nor the czar, it means he already played (and we must do nothing)

          czarFled :: IRC (StateT (Events,Game) m) ()
          czarFled = do
                IRC.msg gameChannel "CZAR WENT MISSING! WHAT DO WE DO NOW?"
                        
          showTable :: IRC (StateT (Events,Game) m) ()
          showTable = do
                x <- use (state.current)
                case x of
                     Nothing -> IRC.msg gameChannel "No game is being played!"
                     Just  c -> do
                         IRC.msg gameChannel ("Czar is " <> c^.table._2)
                         IRC.msg gameChannel ("Black card: " <> showBlack (c^.table._1))
                        
                        
          pickHandler :: (User, Message) -> Account -> [Text] -> IRC (StateT (Events,Game) m) ()
          pickHandler (user,msg) acc args = do
              acc `isNotPlaying` (do
                    IRC.cmd gameChannel "you ain't playing this game"
                 ) $ (do
                    acc `isCzar` (do
                            IRC.msg gameChannel "the czar doesn't play"
                        ) $ (\cards -> do
                            x <- fmap countHoles getBlackCardBeingPlayed
                            if | length args /= x -> do
                                 IRC.msg gameChannel "not enough cards (or too many)"
                               | otherwise -> do
                                    xs <- zip [1..] <$> uses cards S.fromList
                                    case traverse (`lookup` xs) args of
                                         Nothing -> IRC.msg gameChannel "some cards weren't found"
                                         Just xs -> do
                                            return () 
                                    return ()
                        )           
                 )
          
          joinHandler :: (User, Message) -> Account -> [Text] -> IRC (StateT (Events,Game) m) ()
          joinHandler (user,msg) acc args = do
                acc `isPlaying` (do
                    x <- maybe (error "...") id `liftM` nickOf acc
                    if | x == userNick user ->
                            IRC.msg gameChannel (userNick user <> ": you are already joined!")
                       | otherwise -> 
                            replace (userNick user) acc
                 ) $ (do
                    joinPlayer (userNick user) acc =<< takeWCards 10
                 )

          partHandler :: (User, Message) -> Account -> [Text] -> IRC (StateT (Events,Game) m) ()
          partHandler (user,msg) acc args =
                acc `isPlaying` (do
                    deletePlayer acc
                  ) $ (do
                    IRC.msg gameChannel (userNick user <> ": you aren't playing!")
                  ) 

          statusHandler :: (User, Message) -> Account -> [Text] -> IRC (StateT (Events,Game) m) ()
          statusHandler (user,msg) acc args = do
                x <- use (state.current)
                case x of
                     Nothing -> IRC.msg gameChannel "no game is being played"
                     Just c  -> IRC.msg gameChannel $ c^.table._2 <> "is the czar, we're waiting for " <>
                                                        (case (c^.currS) of
                                                              Left _ -> "him"
                                                              Right v -> T.intercalate ", " (S.toList (v^.waitingFor)))
                  
          playersHandler :: (User, Message) -> Account -> [Text] -> IRC (StateT (Events,Game) m) ()
          playersHandler (user,msg) acc args = do
                let f "" = "No players"
                    f x  = "Players: " <> x
                IRC.msg gameChannel . f =<< showPlayers

          startHandler :: (User, Message) -> Account -> [Text] -> IRC (StateT (Events,Game) m) ()
          startHandler (user,msg) acc args = do
                numPlayers `is` (>= 3) $ use (state . current) >>= \case  
                        Just _  -> IRC.msg gameChannel "A game is already being played"
                        Nothing -> do
                            players <- use (state.players)
                            IRC.msg gameChannel . ("Starting game! " <>) =<< showPlayers
                            case withMap M.keys players of
                                 []     -> error "[broken invariant] we're supposed to start a game with PLAYERS!"
                                 (czar:xs) -> do
                                     blackCard <- takeBCard 
                                     state.current .= Just (startState czar blackCard xs)
                            showTable
                                     
                            
          cardsHandler :: (User, Message) -> Account -> [Text] -> IRC (StateT (Events,Game) m) ()
          cardsHandler (user,msg) acc args = do
              x <- use (state.players)
              IRC.notice (userNick user) $ case withMap (M.lookup acc) x of
                   Nothing    -> "You have no cards! are you sure you are playing?"
                   Just cards -> let csh (n, WhiteCard x) = "[" <> T.pack (show n) <> "] " <> x 
                                 in "Cards: " <> (T.intercalate ", " . fmap csh . zip [(1::Int)..] . S.elems $ cards)
                
                
          cmds :: User -> Text -> IRC (StateT (Events,Game) m) () -> Char -> [(Text, (User, Channel) -> Account -> [Text] -> IRC (StateT (Events,Game) m) r)] -> IRC (StateT (Events,Game) m) ()
          cmds usr msg next prefix cmds = 
                 case T.words msg of
                    [] -> next
                    (x:xs) ->
                        case T.uncons x of
                            Just (c, cmd_input) | c == prefix -> do
                                    acc <- accOf (userNick usr)
                                    case acc of
                                        Nothing  -> do
                                            IRC.msg gameChannel "You aren't identified!"
                                            next
                                        Just acc -> mapM_ (\f -> f (usr,msg) acc xs) [ f | (cmd,f) <- cmds, cmd == cmd_input ] 
                            _ -> next

                
stateGame :: [Pack] -> Nick -> Game
stateGame packs nick = 
        let (white,black) = funpack packs
        in initialState white black nick
        
runGame :: IRCConfig -> [Pack] -> IRC (StateT (Events, Game) IO) x -> IO x
runGame cfg packs irc = do
        IRC.connectToIRC'with (`evalStateT` ([], stateGame packs (T.pack (config_nick cfg)))) cfg irc