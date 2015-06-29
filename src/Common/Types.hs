{-# LANGUAGE TemplateHaskell #-}

module Common.Types (
    Events
   ,EventsTag
   ,EventsMonad
   ,emptyEvents
   ,events
   ,pushEvent
   ,popEvent
) where

import           Data.Dynamic
import qualified Control.Monad.Ether.State.Strict as ES
import           Control.Ether.TH

newtype Events = EVList [Dynamic]

ethereal "EventsTag" "events"

type EventsMonad = ES.MonadState EventsTag Events

pushEvent'pure :: Typeable a => a -> Events -> Events
pushEvent'pure x (EVList xs) = EVList $ toDyn x:xs

event_pattern :: Typeable a => Events -> Maybe (a , Events)
event_pattern (EVList []) = Nothing
event_pattern (EVList (x:xs))
                     | Just x' <- fromDynamic x
                     = Just (x', EVList xs)
event_pattern (EVList (_:_)) = Nothing

--pattern Event x xs <- (event_pattern' -> Just (x,xs))

pushEvent :: (EventsMonad m, Typeable a) => a -> m ()
pushEvent x = 
    ES.modify events (pushEvent'pure x)
    
popEvent :: (EventsMonad m, Typeable a) => m (Maybe a)
popEvent = do
    x <- ES.get events
    case event_pattern x of
         Nothing -> return Nothing
         Just (x,xs) -> do
             ES.put events xs
             return (Just x)
    
runEvents :: (EventsMonad m, Typeable a) => (a -> m (Maybe b)) -> m (Maybe b)
runEvents f = do
    x <- popEvent
    case x of
         Nothing -> return Nothing
         Just v  -> f v  
    
emptyEvents :: Events
emptyEvents = EVList [] 