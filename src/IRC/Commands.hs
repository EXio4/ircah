{-# LANGUAGE RankNTypes, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module IRC.Commands (onPRIVMSG, onChannelMsg, run, msg, notice, command) where

import           Control.Applicative
import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.ByteString (ByteString)
import           IRC.Types
import qualified IRC.Raw as Raw

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
                         
      


genCommand :: (UncurryFN a (m b)) => (Raw.Message -> Maybe a) -> Curry a (m b) -> Command m b
genCommand condition fn
    = Command $ \msg -> 
            case condition msg of
                 Nothing -> Nothing
                 Just v  -> Just (uncurryN fn v)

onCommand ::  UncurryFN a (m r)
      =>   ByteString
      ->  ([Text] -> Maybe a)
      ->  (User -> Curry a (m r))
      ->  Command m r
onCommand cmd params_fn fn = genCommand cnd fn
    where cnd (Raw.Message
                    _
                    (Just (Raw.Prefix (Raw.Nick nick) (Just (Raw.User ident)) (Just (Raw.Host host))))
                    (Raw.Command cmd_input)
                    (Raw.Params params))
                | cmd_input == cmd
                , Just rest <- params_fn (map (\(Raw.Param x) -> T.decodeUtf8 x) params)
                = Just (User (T.decodeUtf8 nick) (T.decodeUtf8 ident) (T.decodeUtf8 host)
                       ,rest)
          cnd _ = Nothing

                 
onPRIVMSG :: (User -> Target -> Message -> m a) -> Command m a
onPRIVMSG = onCommand "PRIVMSG" f
    where f [target, msg] = Just (target, (msg, ()))
          f  _            = Nothing

onChannelMsg :: (User -> Channel -> Message -> m a) -> Command m a
onChannelMsg = onCommand "PRIVMSG" f
    where f [channel , msg] | Just ('#', _) <- T.uncons channel
                            = Just (channel , (msg , ()))
          f  _              = Nothing

          
run :: Handler m a -> Raw.Message -> m a
run (Handler cmds (Fallback fallback)) msg = go cmds 
    where go []     = fallback msg
          go ((Command f):fs) | Just x' <- f msg = x'
          go (_:fs) = go fs

          
encode :: [Text] -> Raw.Params
encode ts = Raw.Params (map (Raw.Param . T.encodeUtf8) ts)
    
command :: Text -> [Text] -> Raw.Message
command cmd params = Raw.Message Nothing Nothing (Raw.Command (T.encodeUtf8 cmd)) (encode params)
    
privmsg :: Raw.IRC -> User -> Message -> IO ()
privmsg irc (User target _ _) msg = Raw.irc_send irc (command "PRIVMSG" [target, msg])

msg :: Raw.IRC -> Channel -> Message -> IO ()
msg irc t m = Raw.irc_send irc (command "PRIVMSG" [t, m])

notice :: Raw.IRC -> Channel -> Message -> IO ()
notice irc t m = Raw.irc_send irc (command "NOTICE" [t, m])

privnotice :: Raw.IRC -> User -> Message -> IO ()
privnotice irc (User target _ _) msg = Raw.irc_send irc (command "NOTICE" [target, msg])