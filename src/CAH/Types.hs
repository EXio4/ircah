module CAH.Types where

import           CAH.Cards.Types
import           Data.Set  (Set)
import           Data.Map  (Map)
import           Data.Text (Text)
import           IRC.Types

type AllCards = (Set BlackCard, Set WhiteCard) 
type Player   = Account
newtype Points = Points Integer
    deriving (Show,Eq,Ord)
    
data EventPlayer
        = PlayCard   Account Int 
        | PickWinner Account Int
        | TellCards  Account [WhiteCard]
        | TellBlackCard BlackCard

data GameState 
    = NoGame
    | Game
        AllCards                 -- ^ the possible cards we might use
        GS                       -- ^ actual game state
        (Map Player Points)      -- ^ points
            
data GS = GS {
            players       :: [(Player, Set WhiteCard)]  -- ^ current players (and their cards)
           ,blackCard     :: BlackCard                  -- ^ black card on the table
           ,czar          :: Player                     -- ^ czar
           ,waitingFor    :: [Player]                   -- ^ players we're waiting for
           ,alreadyPlayed :: [(Player, WhiteCard)]      -- ^ players with their picks
} deriving (Show,Eq)