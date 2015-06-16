module IRC.Raw.Types where

import Data.ByteString (ByteString) 

data Message = Message (Maybe Tags) (Maybe Prefix) Command Params
    deriving (Show,Eq)

data Tags    = Tags [Tag]
    deriving (Show,Eq)
    
data Prefix  = ServerName Host
             | Prefix Nick (Maybe User) (Maybe Host)
    deriving (Show,Eq)
    
data Nick    = Nick ByteString
    deriving (Show,Eq)
    
data User    = User ByteString
    deriving (Show,Eq)
    
data Host    = Host ByteString
    deriving (Show,Eq)

data Command = CmdNumber Int
             | Command ByteString
    deriving (Show,Eq)
    
data Params  = Params [Param]
    deriving (Show,Eq)
    
data Param   = Param ByteString
    deriving (Show,Eq)
    
data Tag     = Tag Key (Maybe ByteString)
    deriving (Show,Eq)
    
data Key     = Key (Maybe Vendor) ByteString
    deriving (Show,Eq)
    
data Vendor  = Vendor Host
    deriving (Show,Eq)
