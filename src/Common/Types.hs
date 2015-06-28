module Common.Types (
      Handler(..)
    , Command(..)
    , Fallback(..)
    , Events
) where
import Data.Dynamic

data Handler msg m a
    = Handler 
        [Command  msg m a] 
        (Fallback msg m a)

data Command  msg m a = Command  (msg -> Maybe (m a -> m a))
data Fallback msg m a = Fallback (msg -> m a)

type Events = [Dynamic]