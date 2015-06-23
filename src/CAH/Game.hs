module CAH.Game where
import           Prelude hiding ((.), id)
import           CAH.Cards
import           CAH.Types
import           IRC.Types
import qualified IRC.Raw as Raw
import           IRC.NickTracking
import           IRC.Commands
import           IRC.FRPTypes
import           IRC.Game
import           Control.Wire
import           Text.Read
import qualified Data.Text as T
import qualified Data.Traversable as TV

toLogicCmd :: Monad m => Channel -> Wire s () (IRC m) (NickTracker, Raw.Message) EventPlayer
toLogicCmd ch
    = onC'r $ \tracker -> 
                [onChannelMsg $ \user channel msg next -> do
                    if channel /= ch
                    then next
                    else return $ case T.words msg of
                          ("!pick":cards) -> PlayCards <$> getAccFromNick tracker (userNick user) <*> TV.traverse (readMaybe . T.unpack) cards
                          ["!cards"]      -> TellCards <$> getAccFromNick tracker (userNick user)
                          _               -> Nothing
                ]

game :: Monad m => AllCards -> Channel -> Wire s () (IRC m) ((NickTracker, Players), EventPlayer) ()
game cards gameChannel = logic
    where logic = recu NoGame $ \((tracker, players), cmd) gs -> do
                    return (gs, Nothing)
                        