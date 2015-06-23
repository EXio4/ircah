{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module IRC.Types  where

import           Control.Lens
import           CAH.Cards.Types
import           Data.Set  (Set)
import           Data.Map  (Map)
import qualified IRC.Raw.Types as Raw
import           CAH.Cards.Types
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Data.Text (Text)
import           Control.Applicative
import           System.Random

data Cmd = N Int
         | S ByteString
  deriving (Show,Eq,Ord)  -- used by IRC.Commands

data User = User Nick Ident Host 
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


type Player = Nick

newtype Points = Points Integer
    deriving (Show,Eq,Ord,Num)


data Game = GS {
            _stdGen        :: StdGen
           ,_gameGoingOn   :: Bool
           ,_whiteCards    :: Set WhiteCard
           ,_blackCards    :: Set BlackCard
           ,_points        :: Map Player Points
           ,_players       :: Map Player (Set WhiteCard) -- ^ current players (and their cards)
           ,_blackCard     :: Maybe BlackCard            -- ^ black card on the table
           ,_czar          :: Maybe Player               -- ^ czar
           ,_waitingFor    :: [Player]                   -- ^ players we're waiting for
           ,_alreadyPlayed :: Map Player WhiteCard       -- ^ players with their picks
} deriving (Show)


makeLenses ''Game
