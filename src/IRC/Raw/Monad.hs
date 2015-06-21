module IRC.Raw.Monad (
      IRC
    , irc_send
    , irc_read
    , runIRC 
    , mutateIRC
) where

import IRC.Raw.Types
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class
import Control.Monad

instance Functor IRCF where
    fmap f (IRC_Read g  ) = IRC_Read (f . g)
    fmap f (IRC_Send m a) = IRC_Send m (f a)
    
irc_read ::  Monad m => IRC m Message
irc_read = liftF (IRC_Read id)

irc_send :: Monad m => Message -> IRC m ()
irc_send m = liftF (IRC_Send m ())

mutateIRC :: Monad m => (Message -> IRC m ()) -> IRC m Message -> IRC m r -> IRC m r
mutateIRC write read = run
    where run i = do
            x <- lift $ runFreeT i
            case x of
                Pure a -> return a
                Free (IRC_Read r)     -> read       >>= run . r
                Free (IRC_Send msg r) -> write msg  >>  run   r
          
runIRC :: Monad m => (Message -> m ()) -> m Message -> IRC m a -> m a
runIRC write read = run
    where run i = do
            x <- runFreeT i
            case x of
                 Pure a                -> return a
                 Free (IRC_Read r)     -> read      >>= run . r
                 Free (IRC_Send msg r) -> write msg >>  run   r