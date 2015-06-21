module CAH.Game where

import Common.StateMachine
import CAH.Cards
import CAH.Types
import IRC.Types
import IRC.Game


game :: Monad m => AllCards -> Channel -> SM EventPlayer (IRC m) (Players, GameState)
game cards = undefined