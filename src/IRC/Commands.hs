{-# LANGUAGE RankNTypes, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module IRC.Commands where

import           Control.Applicative
import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           IRC.Types
import qualified IRC.Raw.Types as Raw

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
                         
                         

genCommand :: (UncurryFN a (IO b)) => (Raw.Message -> Maybe a) -> Curry a (IO b) -> Command b
genCommand condition fn
    = Command $ \msg -> 
            case condition msg of
                 Nothing -> Nothing
                 Just v  -> Just (uncurryN fn v)

onPRIVMSG :: (Nick -> Channel -> Message -> IO a) -> Command a
onPRIVMSG fn = genCommand cnd fn
    where cnd (Raw.Message
                    _
                    (Just (Raw.Prefix (Raw.Nick nick) _ _))
                    (Raw.Command "PRIVMSG")
                    (Raw.Params [Raw.Param channel, Raw.Param msg]))
                = Just (T.decodeUtf8 nick , (T.decodeUtf8 channel , (T.decodeUtf8 msg , ())))
          cnd _ = Nothing
          
