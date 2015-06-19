module IRC.Raw.Serialize (serialize) where

import           IRC.Raw.Types
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid

serialize :: Message -> ByteString
serialize (Message tags prefix cmd params)
    =  (case tags of
            Nothing -> ""
            Just t  -> "@" <> serializeTags t <> " ")
    <> (case prefix of
             Nothing -> ""
             Just p  -> ":" <> serializePrefix p <> " ")
    <> serializeCommand cmd
    <> serializeParams params
    <> "\r\n"
    
serializeTags :: Tags -> ByteString
serializeTags (Tags tags) =
    case tags of
         [x]    -> serializeTag x
         (x:xs) -> serializeTag x <> ";" <> serializeTags (Tags tags)
    
serializeTag :: Tag -> ByteString
serializeTag (Tag key opt)
    =  serializeKey key 
    <> (case opt of
             Nothing -> ""
             Just o  -> "=" <> o)

serializeKey :: Key -> ByteString
serializeKey (Key vendor sequ)
    =  (case vendor of
            Nothing -> ""
            Just v  -> serializeVendor v <> "/")
    <> sequ

serializeVendor :: Vendor -> ByteString
serializeVendor (Vendor h) = serializeHost h

serializeHost :: Host -> ByteString
serializeHost (Host v) = v

serializeHostP :: HostP -> ByteString
serializeHostP (ValidHost   h) = serializeHost h
serializeHostP (InvalidHost h) = h

serializePrefix :: Prefix -> ByteString
serializePrefix (ServerName h) = serializeHost h
serializePrefix (Prefix nick user host)
    =  serializeNick nick
    <> (case user of
             Nothing -> ""
             Just u  -> "!" <> serializeUser u)
    <> (case host of
             Nothing -> ""
             Just h  -> "@" <> serializeHostP h)

serializeNick :: Nick -> ByteString
serializeNick (Nick n) = n

serializeUser :: User -> ByteString
serializeUser (User u) = u

serializeCommand :: Command -> ByteString
serializeCommand (CmdNumber n) = BS.pack (show n)
serializeCommand (Command cmd) = cmd

serializeParams :: Params -> ByteString
serializeParams (Params p)
    =   " "
    <> (case p of
             []     -> ""
             [x]    -> ":"              <> serializeParam x
             (x:xs) -> serializeParam x <> serializeParams (Params xs))
             
serializeParam :: Param -> ByteString
serializeParam (Param p) = p
