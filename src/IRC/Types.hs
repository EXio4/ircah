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
) where
import Data.Text (Text)
import qualified IRC.Raw.Types as Raw

data Command  m a = Command (Raw.Message -> Maybe (m a))
data Fallback m a = Fallback (Raw.Message -> m a)

data Handler m a
    = Handler 
        [Command  m a] 
        (Fallback m a)

data User = User Nick Ident Host 
    deriving (Show,Eq)
        
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
