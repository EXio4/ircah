module Common.StateMachine (
      SM(..)
    , runStep
    , productC
    , productCL
    , parall
    , liftF
) where

import Common.Types
import Control.Applicative

identitySM :: Applicative m => SM msg m state
identitySM = SM $ \msg state -> pure state

runStep :: SM msg m state -> msg -> state -> m state
runStep (SM f) = f

productC :: Applicative m => SM msg m s -> SM msg' m s' -> SM (msg, msg') m (s, s')
productC (SM f) (SM g) = SM $ \(msg, msg') (x,y) -> liftA2 (,) (f msg x) (g msg' y)

productCL :: Applicative m => SM msg m s -> SM msg m s' -> SM msg m (s, s')
productCL (SM f) (SM g) = SM $ \msg (x,y) -> liftA2 (,) (f msg x) (g msg y)

parall :: Monad m => SM msg m s -> SM msg m s' -> SM msg m (s, s') -> SM msg m (s, s')
parall (SM f) (SM f') (SM g)
    = SM $ \msg (x,y) -> do
            x' <- f  msg x
            y' <- f' msg y
            g msg (x',y')
    
liftF :: (Applicative m, Monad m) => SM msg m s -> (msg -> (s, s') -> m s') -> SM msg m (s, s')
liftF f g = parall f identitySM (SM (\msg (x,y) -> fmap (x,) (g msg (x,y))))