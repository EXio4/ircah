{-# LANGUAGE FlexibleInstances #-}

module IRC.Types (
     User(..)
    ,Channel
    ,Nick
    ,Message
    ,Ident
    ,Host
    ,Account
    ,Target
    ,Mode(..)
    ,CMode(..)
    ,SASLCfg(..)
    ,ChannelCfg(..)
    ,IRCConfig(..)
    ,Raw.IRC(..)
    ,Cmd(..)
    ,userNick
) where

import qualified IRC.Raw.Types as Raw
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Data.Text (Text)
import           Control.Applicative

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
