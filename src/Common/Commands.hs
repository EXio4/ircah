{-# LANGUAGE RankNTypes, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Common.Commands (
      genCommand
    , Command(..)
    , Handler(..)
    , Fallback(..)
    , UncurryFN(..)
    , CurryFN(..)
    , Curry
    , run
) where
    

import Common.Types
import Data.Monoid
import Control.Applicative
    
      
instance (Applicative m, Monoid e) => Monoid (Handler msg m e) where
        mempty = Handler [] (Fallback (\msg -> pure mempty))
        (Handler cm1 (Fallback fb1)) `mappend` (Handler cm2 (Fallback fb2))
            = Handler (cm1 <> cm2) (Fallback (\msg -> (<>) <$> fb1 msg <*> fb2 msg))
         
type family Curry pms rt where 
    Curry ()    r = r
    Curry (x,y) r = x -> Curry y r
    
class CurryFN params ret where
    curryN :: (params -> ret) -> Curry params ret
class UncurryFN params ret where
    uncurryN :: Curry params ret -> (params -> ret)
    
instance CurryFN () r where
    curryN fn = fn ()

instance (CurryFN y r) => CurryFN (x,y) r where
    curryN fn x
        = let
            f' y = fn (x,y)
          in curryN f'

instance UncurryFN () r where
    uncurryN fn = \() -> fn
    
instance UncurryFN y r => UncurryFN (x,y) r where
    uncurryN fn (x,y)
        = let
            f' = fn x
          in uncurryN f' y
                        

genCommand :: (UncurryFN a (m r -> m r)) => (msg -> Maybe a) -> Curry a (m r -> m r) -> Command msg m r
genCommand condition fn
    = Command $ \msg -> 
            case condition msg of
                 Nothing -> Nothing
                 Just v  -> Just (uncurryN fn v)


run ::  Handler msg m a -> msg -> m a
run (Handler cmds (Fallback fallback)) msg = go cmds 
    where go []     = fallback msg
          go ((Command f):fs) | Just x' <- f msg
                              = x' (go fs)
          go (_:fs) = go fs
