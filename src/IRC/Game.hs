
module IRC.Game where

import           IRC.Types
import qualified IRC.Client as IRC
import qualified IRC.Commands as IRC
import qualified IRC as IRC
import qualified IRC.Raw as Raw
import qualified IRC.Raw as IRC (irc_read, irc_send)
import           IRC.Raw.Types (IRC)
import           Data.Monoid
import           Control.Applicative
import qualified Data.Text as T
import           Data.Text (Text)
import           Control.Monad.State
import qualified Data.Map.Strict as M
import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.Set as S
import           Control.Lens
import           System.Random
import           CAH.Cards.Types
import           Control.Monad.Trans.Free

funpack :: [Pack] -> (Set WhiteCard, Set BlackCard)
funpack [] = (S.empty, S.empty)
funpack ((Pack _ w b):xs) = 
        (S.union w w', S.union b b')
    where ~(w', b') = funpack xs

boolL'M :: MonadState s m => Getting a s a -> (a -> m Bool) -> m r -> m r -> m r
boolL'M lens cond t f = do
    x <- use lens
    c <- cond x
    if c
    then t
    else f

boolL :: MonadState s m => Getting a s a -> (a -> Bool) -> m r -> m r -> m r
boolL lens cond t f = boolL'M lens (return . cond) t f

isNotPlaying player next = boolL players (M.notMember player) next (return ())
isPlaying player next = boolL players (M.member player) next (return ())

numPlayers :: MonadState Game m => m Int
numPlayers = uses players M.size
    

takeWCards :: MonadState Game m => Int -> m (Set WhiteCard)
takeWCards 0 = return S.empty
takeWCards n = do
    x <- use whiteCards
    return S.empty
    

cmds :: Monad m => User -> Text -> m () -> Char -> [(Text, (User, Channel) -> Nick -> [Text] -> m r)] -> m ()
cmds usr msg next prefix cmds =
    case T.words msg of
            [] -> next
            (x:xs) ->
                case T.uncons x of
                     Just (c, cmd_input) | c == prefix -> do
                         mapM_ (\f -> f (usr,msg) (userNick usr) xs) [ f | (cmd,f) <- cmds, cmd == cmd_input ] 
                     _ -> next


initialState :: Set WhiteCard -> Set BlackCard -> Game
initialState white black = GS {
     _stdGen        = mkStdGen 42
    ,_gameGoingOn   = False
    ,_whiteCards    = white
    ,_blackCards    = black
    ,_players       = M.fromList []
    ,_points        = M.fromList []
    ,_blackCard     = Nothing
    ,_czar          = Nothing
    ,_waitingFor    = []
    ,_alreadyPlayed = M.fromList []
}
            
game :: Monad m => Nick -> Channel -> IRC (StateT Game m) ()
game botNick gameChannel = forever loop
    where loop = do
            raw_msg <- IRC.irc_read
            IRC.onIRC raw_msg
                (\_ -> pure ())
                [IRC.onChannelMsg $ \user channel msg next -> do
                    if channel /= gameChannel
                    then next 
                    else cmds user msg next '!'
                      [("join"   , joinHandler)
                      ,("leave"  , partHandler)
                      ,("players", playersHandler)
                      ]
                ]
        
          joinHandler (user,msg) nick args = do
                 nick `isNotPlaying` do
                    crds <- takeWCards 10 -- number of cards
                    IRC.msg gameChannel (nick <> " joined")
                    players %= M.insert nick crds

          partHandler (user,msg) nick args =
                nick `isPlaying` do
                    IRC.msg gameChannel (nick <> " left")
                    players %= M.delete nick
        
          playersHandler (user,msg) nick args = 
                IRC.msg gameChannel . T.pack . show =<< use players



stateGame :: [Pack] -> Game
stateGame packs  = 
        let (white,black) = funpack packs
        in initialState white black
        
runGame :: IRCConfig -> [Pack] -> IRC (StateT Game IO) x -> IO x
runGame cfg packs irc = do
        IRC.connectToIRC'with (`evalStateT` stateGame packs) cfg irc