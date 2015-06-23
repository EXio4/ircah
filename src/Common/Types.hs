module Common.Types (
      Handler(..)
    , Command(..)
    , Fallback(..)
    , SM(..)
) where

data Handler msg m a
    = Handler 
        [Command  msg m a] 
        (Fallback msg m a)


data Command  msg m a = Command  (msg -> Maybe (m a -> m a))
data Fallback msg m a = Fallback (msg -> m a)

-- msg would be the equivalent to the input alphabet
-- this could be just ReaderT msg (StateT state m) ()
data SM msg m state where
    SM :: (msg -> state -> m state) -> SM msg m state