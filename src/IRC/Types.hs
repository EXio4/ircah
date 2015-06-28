{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module IRC.Types  where

import           Control.Lens
import           CAH.Cards.Types
import           Data.Set  (Set)
import           Data.Map  (Map)
import           Data.Bimap (Bimap)
import qualified IRC.Raw.Types as Raw
import           CAH.Cards.Types
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Data.Dynamic
import           Data.Text (Text)
import           Control.Applicative
import           System.Random

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
        
type Channel = Text
type Nick    = Text
type Message = Text
type Ident   = Text
type Host    = Text
type Account = Text
type Target  = Text

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
     channel_name     :: String
    ,channel_password :: Maybe String
} deriving (Show,Eq)
    
data IRCConfig = IRCConfig {
     config_network  :: String
    ,config_port     :: Int
    ,config_nick     :: String
    ,config_sasl     :: Maybe SASLCfg
    ,config_channels :: [ChannelCfg]
} deriving (Show,Eq)


type Player = Account

newtype Points = Points Integer
    deriving (Show,Eq,Ord,Num)


data Game = GS {
            _stdGen        :: StdGen
           ,_whiteCards    :: Set WhiteCard
           ,_blackCards    :: Set BlackCard
           ,_allCards      :: (Set WhiteCard, Set BlackCard) -- allCards should be part of Reader, it's an immutable tuple of _all_ cards, used when refilling
           ,_nickTracker   :: NickTracker
           ,_players       :: Players (Set WhiteCard) -- ^ current players (and their cards)
           ,_current       :: Maybe Current
} deriving (Show)

data Current = Current {
            _points        :: Map Player Points
           ,_table         :: (BlackCard,Player)         -- ^ black card and czar
           ,_currS         :: Either WaitingForCzar WaitingForPlayers
} deriving (Show)

data WaitingForCzar = WaitingForCzar {
         _picks :: Map Int (Player, WhiteCard)
        ,_picked :: Maybe Int
} deriving (Show)

data WaitingForPlayers = WaitingForPlayers {
        _waitingFor    :: Set Player                 -- ^ players we're waiting for
       ,_alreadyPlayed :: Map Player WhiteCard       -- ^ players with their picks
} deriving (Show)

makeLenses ''Game
makeLenses ''Current
makeLenses ''WaitingForCzar
makeLenses ''WaitingForPlayers