module IRC.NickTracking (
      UID
    , uid
    , NickTracker
    , TrackEvent(..)
    , onAccountChange
    , onNickChange
    , emptyTracker
    , defTracker
    , getUID
    , getNick
    , getAccount
    , handlers    
) where

import qualified Data.Bimap as BM
import           Data.Bimap (Bimap)
import qualified Data.Map.Strict as M
import           Data.Map   (Map)
import           Data.Functor
import qualified IRC.Raw as Raw
import           IRC.Types
import           IRC.Commands

data NickTracker
    = Tracker 
        !UID                 -- ^ next uid to use
        !(Bimap Nick UID)    -- ^ bimap keeping the mapping from nicks to UIDs
        !(Map UID Account)   -- ^ maps from user IDs to textual "user account", missing key means the user doesn't have a registered account
    deriving (Show,Eq)
    
uid :: Integer -> UID 
uid = UID

getNick :: NickTracker -> UID -> Maybe Nick
getNick (Tracker _ bmap _) uid = BM.lookupR uid bmap

getUID :: NickTracker -> Nick -> Maybe UID
getUID (Tracker _ bmap _) nick = BM.lookup nick bmap

getAccount :: NickTracker -> UID -> Maybe Account
getAccount (Tracker _ _ m) uid = M.lookup uid m

data TrackEvent
    = AccLogChange   UID            
                     (Maybe Account) -- Nothing means logout, Just acc means login
    | AccNickChange  Nick  -- ^ old nick 
                     Nick  -- ^ new nick

runCallbacks :: (trk -> m r) -> [trk -> Command msg m r] -> trk -> msg -> m r
runCallbacks continuation evs tracker msg = run (Handler (map ($ tracker) evs) (Fallback (\_ -> continuation tracker))) msg
 
         
onAccountChange :: (NickTracker -> UID -> Maybe Account -> m a -> m a) -> NickTracker -> Command TrackEvent m a
onAccountChange cb t = genCommand f (cb t)
    where f (AccLogChange n acc) = Just (n, (acc, ()))
          f _                    = Nothing
          
onNickChange :: (NickTracker -> Nick -> Nick -> m a -> m a) -> NickTracker -> Command TrackEvent m a
onNickChange cb t = genCommand f (cb t)
    where f (AccNickChange old new) = Just (old, (new, ()))
          f _                       = Nothing 
    
emptyTracker :: NickTracker
emptyTracker = Tracker (UID 0) BM.empty M.empty
    
defTracker :: Nick -> NickTracker
defTracker nick = addNick nick emptyTracker
    
newtype UID = UID Integer
    deriving (Show,Eq,Ord)
    
changeNick :: Nick -> Nick -> NickTracker -> NickTracker
changeNick oldnick newnick (Tracker next_uid bmap accs)
    | Just old_uid <- BM.lookup oldnick bmap
    , let new_map = BM.insert newnick old_uid (BM.delete oldnick bmap)
    = Tracker next_uid new_map accs
changeNick _ newnick trk -- passthru means the UID hasn't been added to the bimap (aka someone we didn't know changed nick)
    = addNick newnick trk
    

-- assumes there's no previous user using that nick
addNick :: Nick -> NickTracker -> NickTracker
addNick nick (Tracker (UID n) bmap accs) = Tracker (UID (n+1)) (BM.insert nick (UID n) bmap) accs

login :: Nick -> Account -> NickTracker -> NickTracker
login nick acc (Tracker next_uid bmap accs)
    | Just uid <- BM.lookup nick bmap
    = Tracker next_uid bmap (M.insert uid acc accs)
-- new user logins
login nick acc trk
    = login nick acc (addNick nick trk)
    
logout :: Nick -> NickTracker -> NickTracker
logout nick (Tracker next_uid bmap accs) 
    | Just uid <- BM.lookup nick bmap
    = Tracker next_uid bmap (M.delete uid accs)
logout nick trk           -- login out a nonexistant nick, adding it
    = addNick nick trk
    
handlers :: (Functor m, Monad m) => NickTracker -> (NickTracker -> m a) -> [NickTracker -> Command TrackEvent m a] -> [Command Raw.Message m a]
handlers trk fn evs
        = [trackingACCOUNT  trk fn evs
          ,trackingNICK     trk fn evs
          ,trackingWHOACC   trk fn evs
          ]
       
accChangeHandler :: (NickTracker -> m a) -> [NickTracker -> Command TrackEvent m a] -> NickTracker -> Nick -> Maybe Account -> m a
accChangeHandler continuation evs tracker nick x = uncurry (runCallbacks continuation evs) $ 
              let tracker' = case x of 
                                Nothing     -> logout nick tracker
                                Just newacc -> login  nick newacc tracker
                  uid     = maybe (error "Impossible case, UID should be in Map") id (getUID tracker' nick)
              in (tracker', AccLogChange uid x)
    
accNickChange :: (NickTracker -> m a) -> [NickTracker -> Command TrackEvent m a] -> NickTracker -> Nick -> Nick -> m a
accNickChange continuation evs tracker old_nick new_nick = uncurry (runCallbacks continuation evs) $ 
             let tracker' = changeNick old_nick new_nick tracker
             in (tracker', AccNickChange old_nick new_nick)
       
trackingACCOUNT :: (Functor m) => NickTracker -> (NickTracker -> m a) -> [NickTracker -> Command TrackEvent m a] -> Command Raw.Message m a
trackingACCOUNT tracker continuation evs = onCommand (S "ACCOUNT") params handler
    where params ["*"]  = Just (Nothing , ())
          params [acc]  = Just (Just acc, ())
          params  _     = Nothing
          handler (User nick _ _) x _ = accChangeHandler continuation evs tracker nick x 
       
trackingWHOACC :: (Functor m) => NickTracker -> (NickTracker -> m a) -> [NickTracker -> Command TrackEvent m a] ->  Command Raw.Message m a
trackingWHOACC tracker continuation evs = onCommandServerHost (N 354) params handler
    where params [_, nick, "0"]  = Just (nick, (Nothing , ()))
          params [_, nick, acc]  = Just (nick, (Just acc, ()))
          params  xs     =  Nothing
          handler _ nick x _ =  accChangeHandler continuation evs tracker nick x 
              
              
trackingNICK :: (Functor m) => NickTracker -> (NickTracker -> m a) -> [NickTracker -> Command TrackEvent m a] ->  Command Raw.Message m a
trackingNICK tracker continuation evs = onCommand (S "NICK") params handler
    where params [newnick] = Just (newnick, ())
          params  _        = Nothing
          handler (User old_nick _ _) new_nick _ = accNickChange continuation evs tracker old_nick new_nick
                                                   