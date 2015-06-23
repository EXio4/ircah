{-# LANGUAGE Arrows #-}
module IRC.FRPTypes where

import           Prelude hiding ((.), id)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Wire.Core
import           FRP.Netwire
import           Control.Wire
import qualified Data.Text     as T
import qualified IRC.Raw.Types as Raw
import qualified IRC.Raw       as Raw
import           IRC
import           IRC.Client
import           IRC.Commands

type Input m a = (HasTime t s) => Wire s () (Raw.IRC m) () a


ircMessage :: Monad m => Input m Raw.Message
ircMessage = mkGen_ $ \_ -> fmap Right Raw.irc_read

           
clockSes :: MonadIO m => Session (IRC m) (Timed NominalDiffTime ()) 
clockSes = clockSession_
        
f :: Monad m => Wire s () (IRC m) Raw.Message ()
f = mkGen_ (\rawmsg -> do
        onIRC rawmsg
                (\_ -> return $ Left ())
                [onChannelMsg $ \user channel msg next -> do
                    case msg of
                        "!ping" -> do
                            cmd "PRIVMSG" [channel, "pong"]
                            return (Right ())
                        _       -> next
                ])
            
onC'r :: Monad m => (k -> [Command Raw.Message (IRC m) (Maybe a)]) -> Wire s () (IRC m) (k, Raw.Message) a
onC'r cmds = go
    where go = mkGen_ $ \(k,msg) -> do
                x <- onIRC msg
                        (\_ -> return Nothing)
                        (cmds k)
                return $ case x of
                     Nothing -> Left ()
                     Just  v -> Right v
                     

stC'r :: Monad m => s -> (s -> [Command Raw.Message (IRC m) (Either () r, Wire s () (IRC m) Raw.Message r)]) -> Wire s () (IRC m) Raw.Message r
stC'r s0 f = go s0
    where go s =
            mkGenN $ \msg -> do
                onIRC msg
                    (\_ -> return (Left (), go s))
                    (f s)
    
stC :: Monad m => s -> (s -> [Command Raw.Message (IRC m) (Maybe (r, s))]) -> Wire s () (IRC m) Raw.Message r
stC s0 f = go s0
    where go s = 
            mkGenN $ \msg -> do
               x <- onIRC msg
                        (\_ -> return Nothing)
                        (f s)
               case x of
                    Nothing     -> return (Left (), go s)
                    Just (v,s') -> return (Right v, go s') 
         
stFeedR :: Monad m => s -> (s -> [Command Raw.Message (IRC m) (Maybe s)]) -> Wire t () (IRC m) Raw.Message s
stFeedR s0 f = go s0
    where go s = 
            mkGenN $ \msg -> do
               x <- onIRC msg
                        (\_ -> return Nothing)
                        (f s)
               case x of
                    Nothing -> return (Left (), go s)
                    Just v  -> return (Right v, go v) 
       
stFeedR'l :: Monad m => s -> (k -> s -> [Command Raw.Message (IRC m) (Maybe s)]) -> Wire t () (IRC m) (k, Raw.Message) s
stFeedR'l s0 f = go s0
    where go s = 
            mkGenN $ \(k, msg) -> do
               x <- onIRC msg
                        (\_ -> return Nothing)
                        (f k s)
               case x of
                    Nothing -> return (Left (), go s)
                    Just v  -> return (Right v, go v) 
       
recu :: (Monoid e, Monad m) => s -> (a -> s -> m (s, Maybe r)) -> Wire t e m a r
recu s0 f = go s0
    where go s =
            mkGenN $ \k -> do
                x <- f k s
                return $ case x of
                     (s', Nothing) -> (Left mempty, go s')
                     (s', Just v ) -> (Right v    , go s')
            

loopG :: Monad m => Wire s () (IRC m) () r -> Session (IRC m) s -> IRC m ()
loopG wire s = do
    (ds, s')   <- stepSession s
    (m, wire') <- stepWire wire ds (Right ())
    case m of
        Left  _  -> loopG wire' s'
        Right xs -> do
            --mapM_ Raw.irc_send xs
            loopG wire' s'