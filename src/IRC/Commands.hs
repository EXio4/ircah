{-# LANGUAGE FlexibleContexts #-}
module IRC.Commands (
      onPRIVMSG
    , onJOIN
    , onPART
    , onKICK
    , onQUIT
    , onChannelMsg
    , cmd
    , msg
    , notice
    , command
    , onCommand
    , onCommandServerHost
    ,module Common.Commands
) where

import           Control.Applicative
import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.ByteString (ByteString)
import           Common.Commands
import           IRC.Types
import qualified IRC.Raw as Raw


onCommandServerHost ::  UncurryFN a (m r -> m r)
      =>   Cmd
      ->  ([Text] -> Maybe a)
      ->  (Host -> Curry a (m r -> m r))
      ->  Command Raw.Message m r
onCommandServerHost cmd params_fn fn = genCommand cnd fn
    where cnd (Raw.Message
                    _
                    (Just (Raw.ServerName (Raw.Host host)))
                    cmd_input
                    (Raw.Params params))
                | case (cmd, cmd_input) of
                       (N x, Raw.CmdNumber y) -> x == y
                       (S x, Raw.Command   y) -> x == y
                       (_  , _              ) -> False
                , Just rest <- params_fn (map (\(Raw.Param x) -> T.decodeUtf8 x) params)
                = Just (T.decodeUtf8 host
                       ,rest)
          cnd _ = Nothing
                 
onCommand ::  UncurryFN a (m r -> m r)
      =>   Cmd
      ->  ([Text] -> Maybe a)
      ->  (User -> Curry a (m r -> m r))
      ->  Command Raw.Message m r
onCommand cmd params_fn fn = genCommand cnd fn
    where cnd (Raw.Message
                    _
                    (Just (Raw.Prefix (Raw.Nick nick) (Just (Raw.User ident)) (Just hostp)))
                    cmd_input
                    (Raw.Params params))
                | case (cmd, cmd_input) of
                       (N x, Raw.CmdNumber y) -> x == y
                       (S x, Raw.Command   y) -> x == y
                       (_  , _              ) -> False
                , let host = case hostp of
                                Raw.ValidHost (Raw.Host h) -> h
                                Raw.InvalidHost h      -> h
                , Just rest <- params_fn (map (\(Raw.Param x) -> T.decodeUtf8 x) params)
                = Just (User (T.decodeUtf8 nick) (T.decodeUtf8 ident) (T.decodeUtf8 host)
                       ,rest)
          cnd _ = Nothing


onJOIN :: (User -> Channel -> [Text] -> m a -> m a) -> Command Raw.Message m a
onJOIN = onCommand (S "JOIN") f
    where f (channel:metadata) = Just (channel, (metadata, ()))
          f  _                 = Nothing
 
onPART :: (User -> Channel -> Maybe Text -> m a -> m a) -> Command Raw.Message m a
onPART = onCommand (S "PART") f
    where f [ch, partmsg] = Just (ch, (Just partmsg, ()))
          f [ch]          = Just (ch, (Nothing     , ()))
          f  _            = Nothing
 
onQUIT :: (User -> Maybe Text -> m a -> m a) -> Command Raw.Message m a
onQUIT = onCommand (S "QUIT") f
    where f [quitmsg] = Just (Just quitmsg, ())
          f []        = Just (Nothing     , ())
          f  _        = Nothing
   
onKICK :: (User -> Channel -> Nick -> Maybe Text -> m a -> m a) -> Command Raw.Message m a
onKICK = onCommand (S "KICK") f
    where f [ch, kicked, partmsg] = Just (ch, (kicked, (Just partmsg, ())))
          f [ch, kicked]          = Just (ch, (kicked, (Nothing     , ())))
          f  _            = Nothing

   
onPRIVMSG :: (User -> Target -> Message -> m a -> m a) -> Command Raw.Message m a
onPRIVMSG = onCommand (S "PRIVMSG") f
    where f [target, msg] = Just (target, (msg, ()))
          f  _            = Nothing

onChannelMsg :: (User -> Channel -> Message -> m a -> m a) -> Command Raw.Message m a
onChannelMsg = onCommand (S "PRIVMSG") f
    where f [channel , msg] | Just ('#', _) <- T.uncons channel
                            = Just (channel , (msg , ()))
          f  _              = Nothing


encode :: [Text] -> Raw.Params
encode ts = Raw.Params (map (Raw.Param . T.encodeUtf8) ts)


cmd :: Monad m => Text -> [Text] -> IRC m ()
cmd cmd params = Raw.irc_send (command cmd params)

command :: Text -> [Text] -> Raw.Message
command cmd params = Raw.Message Nothing Nothing (Raw.Command (T.encodeUtf8 cmd)) (encode params)
    
privmsg :: Monad m => User -> Message -> IRC m ()
privmsg (User target _ _) msg = cmd "PRIVMSG" [target, msg]

msg :: Monad m =>  Channel -> Message -> IRC m ()
msg t m = cmd "PRIVMSG" [t, m]

notice :: Monad m => Channel -> Message -> IRC m ()
notice t m = cmd "NOTICE" [t, m]

privnotice :: Monad m => User -> Message -> IRC m ()
privnotice (User target _ _) msg = cmd "NOTICE" [target, msg]