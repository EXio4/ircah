{-# LANGUAGE FlexibleInstances #-}

module IRC.Types (
     Command(..)
    ,Fallback(..)
    ,Handler(..)
    ,User(..)
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

data Command  m a = Command (Raw.Message -> Maybe (m a -> m a))
data Fallback m a = Fallback (Raw.Message -> m a)

data Cmd = N Int
         | S ByteString
  deriving (Show,Eq,Ord)  -- used by IRC.Commands

data Handler m a
    = Handler 
        [Command  m a] 
        (Fallback m a)

        
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

instance Applicative m => Monoid (Handler m ()) where
        mempty = Handler [] (Fallback (\msg -> pure ()))
        (Handler cm1 (Fallback fb1)) `mappend` (Handler cm2 (Fallback fb2))
            = Handler (cm1 <> cm2) (Fallback (\msg -> fb1 msg *> fb2 msg))