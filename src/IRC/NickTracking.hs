{-# LANGUAGE OverloadedStrings #-}
module IRC.NickTracking (
      UID
    , uid
    , NickTracker
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
    
getNick :: NickTracker -> UID -> Maybe Nick
getNick (Tracker _ bmap _) uid = BM.lookupR uid bmap

getUID :: NickTracker -> Nick -> Maybe UID
getUID (Tracker _ bmap _) nick = BM.lookup nick bmap

getAccount :: NickTracker -> UID -> Maybe Account
getAccount (Tracker _ _ m) uid = M.lookup uid m

handlers :: (Functor m, Monad m) => NickTracker -> (NickTracker -> m a) -> [Command m ()]
handlers trk fn = [trackingACCOUNT trk fn
                  ,trackingNICK trk fn
                  ,trackingWHOACC trk fn
                  ]
       
trackingACCOUNT :: (Functor m) => NickTracker -> (NickTracker -> m a) -> Command m ()
trackingACCOUNT tracker continuation = onCommand (S "ACCOUNT") params handler
    where params ["*"]  = Just (Nothing , ())
          params [acc]  = Just (Just acc, ())
          params  _     = Nothing
          handler (User nick _ _)  Nothing      = Just () <$ continuation (logout nick        tracker)
          handler (User nick _ _) (Just newacc) = Just () <$ continuation (login  nick newacc tracker)
       
trackingWHOACC :: (Functor m) => NickTracker -> (NickTracker -> m a) -> Command m ()
trackingWHOACC tracker continuation = onCommandServerHost (N 354) params handler
    where params [_, nick, "0"]  = Just (nick, (Nothing , ()))
          params [_, nick, acc]  = Just (nick, (Just acc, ()))
          params  xs     =  Nothing
          handler _ nick Nothing       = Just () <$ continuation (logout nick        tracker)
          handler _ nick (Just newacc) = Just () <$ continuation (login  nick newacc tracker)
              
              
trackingNICK :: (Functor m) => NickTracker -> (NickTracker -> m a) -> Command m ()
trackingNICK tracker continuation = onCommand (S "NICK") params handler
    where params [newnick] = Just (newnick, ())
          params  _        = Nothing
          handler (User old_nick _ _) new_nick = Just () <$ continuation (changeNick old_nick new_nick tracker)