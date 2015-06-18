module IRC.Types where

import Data.Text (Text)

data In
    = I_JOIN     User Channel
    | I_PART     User Channel (Maybe Message)
    | I_QUIT     User Channel (Maybe Message)
    | I_CMSG     User Channel        Message
    | I_CNOTICE  User Channel        Message
    | I_PMSG     User                Message
    | I_PNOTICE  User                Message
    | I_UMODE    Nick          [Mode]
    | I_CMODE    User Channel  [CMode]
    | I_ACCOUNT  Nick (Maybe Account)
    deriving (Show,Eq)
    
    
data Out
    = O_JOIN Channel
    | O_PART Channel (Maybe Text)
    | O_QUIT         (Maybe Text)
    | O_MSG    Target Text
    | O_NOTICE Target Text
    | O_CMODE  Channel [CMode]
    | O_UMODE  Nick    [Mode]
    deriving (Show,Eq)
    
type Channel = Text
type Nick    = Text
type Message = Text
type Account = Text
type Target  = Text
    
data Mode = Plus  Char
          | Minus Char
    deriving (Show,Eq)
data CMode = CMode Nick Mode
    deriving (Show,Eq)
data User = User Nick
    deriving (Show,Eq)
    
data SASLCfg = SASLCfg {
     sasl_username :: String
    ,sasl_password :: String
} deriving (Show,Eq)

data ChannelCfg = ChannelCfg {
     channel_name     :: String
    ,channel_password :: Maybe String
} deriving (Show,Eq)
    
data Config = Config {
     config_network  :: String
    ,config_port     :: Int
    ,config_nick     :: String
    ,config_sasl     :: Maybe SASLCfg
    ,config_channels :: [ChannelCfg]
} deriving (Show,Eq)