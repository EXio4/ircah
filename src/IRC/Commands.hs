{-# LANGUAGE FlexibleContexts #-}
module IRC.Commands where

import           Control.Applicative
import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.ByteString (ByteString)
import           IRC.Types
import           IRC.Raw.Monad
import           Data.String
import qualified IRC.Raw as Raw
          
commandMatchServerHost :: Cmd -> ([Text] -> Maybe a) -> Raw.Message -> Maybe (Host, a)
commandMatchServerHost cmd params_fn = cnd
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

commandMatchUser :: Cmd -> ([Text] -> Maybe a) -> Raw.Message -> Maybe (User , a)
commandMatchUser cmd params_fn = cnd
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
                = Just (User (Nick (T.decodeUtf8 nick)) (T.decodeUtf8 ident) (T.decodeUtf8 host)
                       ,rest)
          cnd _ = Nothing
          
commandMatchRaw :: (Cmd -> [Text] -> Maybe a) -> Raw.Message -> Maybe a
commandMatchRaw params_fn = cnd
    where cnd (Raw.Message
                    _
                    _
                    cmd_input
                    (Raw.Params params))
                | let cmd = case cmd_input of
                       Raw.CmdNumber y -> N y
                       Raw.Command   y -> S y         
                = params_fn cmd (map (\(Raw.Param x) -> T.decodeUtf8 x) params)


join_pattern :: Raw.Message -> Maybe (User , (Channel, [Text]))
join_pattern = commandMatchUser (S "JOIN") f
    where f (channel:metadata) = Just (Channel channel, metadata)
          f  _                 = Nothing
          
pattern JOIN user channel meta <- (join_pattern -> Just (user,(channel,meta)))
 
part_pattern :: Raw.Message -> Maybe (User , (Channel, Maybe Text))
part_pattern = commandMatchUser (S "PART") f
    where f [channel]   = Just (Channel channel, Nothing)
          f [channel,x] = Just (Channel channel, Just x )
          f  _          = Nothing

pattern PART user channel msg <- (part_pattern -> Just (user,(channel,msg)))

quit_pattern :: Raw.Message -> Maybe (User, (Channel, Maybe Text))
quit_pattern = commandMatchUser (S "QUIT") f
    where f [channel]   = Just (Channel channel, Nothing)
          f [channel,x] = Just (Channel channel, Just x )
          f _           = Nothing

pattern QUIT user channel msg <- (quit_pattern -> Just (user,(channel,msg)))
          
kick_pattern :: Raw.Message -> Maybe (User, (Channel, Nick, Maybe Text))
kick_pattern = commandMatchUser (S "QUIT") f
    where f [ch,kicked]          = Just (Channel ch, Nick kicked, Nothing      )
          f [ch,kicked,partmsg]  = Just (Channel ch, Nick kicked, Just partmsg )
          f _                    = Nothing

pattern KICK user channel kicked msg <- (kick_pattern -> Just (user,(channel,kicked,msg)))
          
channelmsg_pattern :: Raw.Message -> Maybe (User, (Channel, Message))
channelmsg_pattern = commandMatchUser (S "PRIVMSG") f
    where f [channel,msg]  | Just ('#', _) <- T.uncons channel
                           = Just (Channel channel , Message msg)
          f _              = Nothing
          
pattern CHMSG user channel msg <- (channelmsg_pattern -> Just (user,(channel,msg)))
      
privmsg_pattern :: Raw.Message -> Maybe (User, (Target,Message))
privmsg_pattern= commandMatchUser (S "PRIVMSG") f
    where f [channel,msg]  = Just (Target channel , Message msg)
          f _              = Nothing
   
pattern PRIVMSG user target msg <- (privmsg_pattern -> Just (user, (target,msg)))

nick_pattern :: Raw.Message -> Maybe (User, Nick)
nick_pattern= commandMatchUser (S "PRIVMSG") f
    where f [new_nick]  = Just (Nick new_nick)
          f _           = Nothing
   
pattern NICK user nick <- (nick_pattern -> Just (user, nick))
  
account_pattern :: Raw.Message -> Maybe (User, Maybe Account)
account_pattern = commandMatchUser (S "ACCOUNT") f
    where f ["*"]  = Just Nothing
          f [acc]  = Just (Just (Account acc))
          f  _     = Nothing
          

pattern ACCOUNT user acc <- (account_pattern -> Just (user, acc))
  
raw_pattern :: Raw.Message -> Maybe (Cmd, [Text])
raw_pattern = commandMatchRaw f
    where f cmd params = Just (cmd, params)

pattern RAW cmd pms <- (raw_pattern -> Just (cmd,pms))
  
encode :: [Text] -> Raw.Params
encode ts = Raw.Params (map (Raw.Param . T.encodeUtf8) ts)

cmd :: MonadIRC m => Text -> [Text] -> m ()
cmd cmd params = Raw.irc_send (command cmd params)

command :: Text -> [Text] -> Raw.Message
command cmd params = Raw.Message Nothing Nothing (Raw.Command (T.encodeUtf8 cmd)) (encode params)
          
msg :: (IRCCommand target, MonadIRC m) => target -> Message -> m ()
msg = privmsg
          
class IRCCommand target where
    privmsg :: MonadIRC m => target -> Message -> m ()
    notice  :: MonadIRC m => target -> Message -> m ()

instance IRCCommand User where
    privmsg (User (Nick target) _ _) (Message msg) = cmd "PRIVMSG" [target, msg]
    notice  (User (Nick target) _ _) (Message msg) = cmd "NOTICE"  [target, msg]

instance IRCCommand Channel where
    privmsg (Channel ch) (Message msg) = cmd "PRIVMSG" [ch, msg]
    notice  (Channel ch) (Message msg) = cmd "NOTICE"  [ch, msg]

instance IRCCommand Nick where
    privmsg (Nick n) (Message msg) = cmd "PRIVMSG" [n,msg]
    notice  (Nick n) (Message msg) = cmd "NOTICE"  [n,msg]
    
instance IRCCommand Target where
    privmsg (Target t) (Message msg) = cmd "PRIVMSG" [t,msg]
    notice  (Target t) (Message msg) = cmd "NOTICE"  [t,msg]
    
instance IsString Message where
    fromString = Message . fromString