{-# LANGUAGE TemplateHaskell #-}
module IRC.NickTracking (
      UID
    , uid
    , NickTracker
    , emptyTracker
    , defTracker
    , getUID
    , getNick
    , getAccount
    , getAccFromNick
    , trackerSM 
) where

import qualified Data.Bimap as BM
import           Data.Bimap (Bimap)
import qualified Data.Map.Strict as M
import           Data.Map   (Map)
import           Data.Functor
import           Control.Applicative
import           Common.Types
import qualified IRC.Raw as Raw
import           IRC.Types
import           IRC.Commands
import           Control.Lens
import qualified Control.Monad.Ether.State.Strict as ES
import           Control.Ether.TH


uid :: Integer -> UID 
uid = UID

getNick :: UID -> NickTracker -> Maybe Nick
getNick uid (Tracker _ bmap _) = BM.lookupR uid bmap

getUID :: Nick -> NickTracker -> Maybe UID
getUID nick (Tracker _ bmap _) = BM.lookup nick bmap

getAccount :: UID -> NickTracker -> Maybe Account
getAccount uid (Tracker _ _ m) = M.lookup uid m
   
getAccFromNick :: Nick -> NickTracker -> Maybe Account
getAccFromNick nick trk = getUID nick trk >>= (`getAccount` trk)
   
emptyTracker :: NickTracker
emptyTracker = Tracker (UID 0) BM.empty M.empty
    
defTracker :: Nick -> NickTracker
defTracker nick = addNick nick emptyTracker
        
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
            
        
trackerSM :: (EventsMonad m, TrackerMonad m) => Raw.Message -> m ()
trackerSM msg = do
     tracker <- ES.get nickTracker
     let x = case msg of
               NICK (User old_nick _ _) new_nick -> 
                    Just (NickChange old_nick new_nick
                         ,changeNick old_nick new_nick tracker)
               ACCOUNT (User nick _ _) (Just acc) -> 
                    Just (Login  nick acc
                         ,login nick acc tracker)
               ACCOUNT (User nick _ _) Nothing    -> 
                    Just (Logout nick
                         ,logout nick tracker    )
               RAW (N 354) [_, nick, "0"] -> 
                    Just (Logout (Nick nick)
                         ,logout (Nick nick) tracker    )
               RAW (N 354) [_, nick, acc] ->
                    Just (Login (Nick nick) (Account acc)
                    ,login (Nick nick) (Account acc) tracker)
               _ -> Nothing
     case x of
         Nothing -> return ()
         Just (ev, trk) -> do
             pushEvent ev 
             ES.put nickTracker trk