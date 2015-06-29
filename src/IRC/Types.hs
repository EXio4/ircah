{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module IRC.Types  where

import           Control.Lens
import           CAH.Cards.Types
import           Data.Set  (Set)
import           Data.Map  (Map)
import           Data.Bimap (Bimap)
import qualified IRC.Raw.Types as Raw
import           IRC.Raw.Monad
import           Common.Types
import           CAH.Cards.Types
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Data.Dynamic
import           Data.Text (Text)
import           Control.Applicative
import           System.Random
import           Control.Monad.Trans.Class
import qualified Control.Monad.Ether.State.Strict as ES
import qualified Control.Monad.Ether.Reader as ER
import           Control.Ether.TH

ethereal "GameState"  "gameState"
ethereal "CardSet"    "cardSet"
ethereal "Tracker"    "nickTracker"
ethereal "GameInfoT"  "gameInfo"

type TrackerMonad = ES.MonadState Tracker   NickTracker
type GameMonad r m = (ES.MonadState GameState (GS r) m, ER.MonadReader CardSet [Pack] m, ER.MonadReader GameInfoT GameInfo m, Applicative m)

data Cmd = N Int
         | S ByteString
  deriving (Show,Eq,Ord)  -- used by IRC.Commands

data User = User Nick Ident Host 
    deriving (Show,Eq)
    
newtype UID = UID Integer
    deriving (Show,Eq,Ord)
    
data TrackEvent
    = Login  Nick Account
    | Logout Nick
    | NickChange Nick Nick
    deriving (Show,Eq, Typeable)
    
data NickTracker
    = Tracker 
        !UID                 -- ^ next uid to use
        !(Bimap Nick UID)    -- ^ bimap keeping the mapping from nicks to UIDs
        !(Map UID Account)   -- ^ maps from user IDs to textual "user account", missing key means the user doesn't have a registered account
    deriving (Show,Eq)
    

data Players a = Players (Bimap UID Account) (Map Account a)
    deriving (Show,Eq)
    
userNick :: User -> Nick
userNick (User n _ _) = n
        
newtype Channel = Channel Text
    deriving (Show,Eq)
newtype Nick    = Nick    Text
    deriving (Show,Eq,Ord)
newtype Message = Message Text
    deriving (Show,Eq,Monoid)
newtype Target  = Target Text
    deriving (Show,Eq)
newtype Account = Account Text
    deriving (Show,Eq,Ord)
type Ident   = Text
type Host    = Text

data Mode = Plus  Char
          | Minus Char
    deriving (Show,Eq)
data CMode = CMode Nick Mode
    deriving (Show,Eq)
    
data SASLCfg = SASLCfg {
     sasl_username :: String
    ,sasl_password :: String
} deriving (Show,Eq)

data ChannelCfg = ChannelCfg {
     channel_name     :: Channel
    ,channel_password :: Maybe String
} deriving (Show,Eq)
    
data IRCConfig = IRCConfig {
     config_network  :: String
    ,config_port     :: Int
    ,config_nick     :: Nick
    ,config_sasl     :: Maybe SASLCfg
    ,config_channels :: [ChannelCfg]
} deriving (Show,Eq)


type Player = Account

data NatN = Nat_Zero
          | Nat_Succ NatN
    
data T_GameState {-(p :: NatN)-} a
    = T_NoGameBeingPlayed
    | T_WaitingFor a
    deriving (Typeable)
data T_Waiting
    = T_Players
    | T_Czar
    deriving (Typeable)

type family ToLogicTag (x :: *) :: T_GameState T_Waiting where
    ToLogicTag NoGame              = T_NoGameBeingPlayed
    ToLogicTag (Current WFCzar)    = T_WaitingFor T_Czar
    ToLogicTag (Current WFPlayers) = T_WaitingFor T_Players
    
data LogicCommand st  where
    PlayerJoin  :: Nick -> Account ->                LogicCommand a
    PlayerLeave :: Nick -> Account ->                LogicCommand a
    StartGame   :: Nick -> Account ->                LogicCommand T_NoGameBeingPlayed
    PlayerPick  :: Nick -> Account -> [WhiteCard] -> LogicCommand (T_WaitingFor T_Players)
    CzarPick    :: Nick -> Account -> ChoiceN     -> LogicCommand (T_WaitingFor T_Czar)
    ShowTable   ::                                   LogicCommand (T_WaitingFor a)
    ShowCards   :: Nick -> Account ->                LogicCommand (T_WaitingFor a)
    
data TextMessage
        = NoError
        | UserNotPlaying     Nick
        | UserNotIdentified  Nick
        | NotEnoughPlayers   Integer
        | AlreadyPlaying     Nick
        | GameStarted        [Nick]
        | GameAlreadyBeingPlayed
        | JoinPlayer         Nick
        | LeavePlayer        Nick
        | AlreadyPlayed      Nick
        | ReplacingOld       Nick Nick -- the first nick is the "old" one
        | MustPickNCards     Nick Integer
        | PlayersWin        [Nick] Points
        | CzarPicked         Nick Nick BlackCard [WhiteCard] Points -- first is czar
        | CzarDoesn'tPlay    Nick
        | YourCardsAre       Nick [(Integer, WhiteCard)]
        | TheCardsAre
        | CardsPicked        Nick Integer BlackCard [WhiteCard]
        | PlayersList        [Nick]
        | CzarLeft           Nick
        | Table              Nick BlackCard
        | StatusWaitingCzar  Nick 
        | StatusWaitPlayers  Nick [Nick] -- first is czar
        | Debug              Text
        
    
newtype ChoiceN = ChoiceN Integer
    deriving (Show,Eq,Ord)

newtype Points = Points Integer
    deriving (Show,Eq,Ord,Num)
    
type Game r m
    = ES.StateT  GameState (GS r) 
    ( ES.StateT  EventsTag Events
    ( ES.StateT  Tracker   NickTracker
    ( ER.ReaderT CardSet   [Pack]
    ( ER.ReaderT GameInfoT GameInfo
    (IRC m)))))
    
data GameInfo = GameInfo {
         _gameChannel :: Channel
        ,_botNick     :: Nick
} deriving (Show)
    
data GS a = GS {
            _stdGen        :: StdGen
           ,_whiteCards    :: Set WhiteCard
           ,_blackCards    :: Set BlackCard
           ,_players       :: Players (Set WhiteCard) -- ^ current players (and their cards)
           ,_current       :: a
} deriving (Show,Functor)

changing :: Monad m => (GS r -> GS r') -> Game r' m a -> Game r m a
changing f gs = do
    x <- ES.get gameState
    lift $ ES.evalStateT gameState gs (f x)

data NoGame = NoGame
    deriving (Show)

data Current w = Current {
            _points        :: Map Player Points
           ,_czar          :: Player
           ,_blackCard     :: BlackCard
           ,_currentB      :: w
} deriving (Show,Functor)

data WFCzar = WFCzar {
         _picks :: Map Integer (Player, [WhiteCard])
} deriving (Show)

data WFPlayers = WFPlayers {
        _waitingFor    :: Set Player                 -- ^ players we're waiting for
       ,_alreadyPlayed :: Map Player [WhiteCard]     -- ^ players with their picks
} deriving (Show)


makeLenses ''GS
makeLenses ''Current
makeLenses ''WFCzar
makeLenses ''WFPlayers
makeLenses ''GameInfo
