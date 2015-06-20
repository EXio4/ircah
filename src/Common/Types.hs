module Common.Types (
      Handler(..)
    , Command(..)
    , Fallback(..)
) where

data Handler msg m a
    = Handler 
        [Command  msg m a] 
        (Fallback msg m a)

data Command  msg m a = Command  (msg -> Maybe (m a -> m a))
data Fallback msg m a = Fallback (msg -> m a)
