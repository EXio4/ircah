module IRC.Raw.Parser (parseIRC) where

import           Prelude hiding (takeWhile)
import           Control.Applicative
import           Control.Monad

import           IRC.Raw.Parser.Host
import           IRC.Raw.Types
import           Parser.Utils
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Attoparsec.ByteString.Char8
{-
  Char8, per name, doesn't work nicely with >0xFF characters
  this is good enough for parsing the barebones of IRC
  
  decoding into UTF8 should be done by the high level library (should it?)
-}


crlf :: Parser ()
crlf = () <$ string "\r\n" 

message :: Parser Message
message
    = (Message
    <$> optional (char '@' *> tags <* space) 
    <*> optional (char ':' *> prefix <* space)
    <*> command
    <*> params) <* crlf
    
tags :: Parser Tags
tags = Tags <$> many1 tag <?> "tags"

tag :: Parser Tag
tag = Tag <$> key <*> optional (char '=' *> escapedValue) <?> "tag"

key :: Parser Key
key = Key <$> optional (vendor <* char '/') <*> takeWhile (\x -> isAlpha_ascii x || isDigit x || x == '-') <?> "key"

escapedValue :: Parser ByteString
escapedValue = takeTill (\x -> x `elem` "; \0\r\n") <?> "escaped value"

vendor :: Parser Vendor
vendor = Vendor <$> host <?> "vendor"

prefix :: Parser Prefix
prefix
    = ((Prefix 
        <$> nick
        <*> optional (char '!' *> user)
        <*> optional (char '@' *> hostp))
        <* lookH ' ')
    <|> (ServerName <$> host)
    <?> "prefix"

nick :: Parser Nick
nick = Nick <$> nick' <?> "nick"
    where nick' =
            BS.cons
                <$> letter_ascii
                <*> takeWhile (\x -> isAlpha_ascii x || isDigit x || special x)
          special x = x `elem` "-_[]|\\`^{}" 
          

hostp :: Parser HostP
hostp =   ValidHost   <$> (host <* lookH ' ')
      <|> InvalidHost <$> takeWhile (/= ' ') -- nice hack for parsing cloaks
          
command :: Parser Command
command
    =   Command <$> takeWhile1 isAlpha_ascii
    <|> (\x y z -> CmdNumber (read [x,y,z])) <$> digit <*> digit <*> digit
    <?> "command"
    
params :: Parser Params
params = space *> (
        (\x -> Params [Param x]) <$> (char ':' *> trailing)
    <|> ((\x (Params y) -> Params (x:y)) <$> middle <*> params))
    <|> (Params [] <$ lookH '\r')
    <?> "params"

middle :: Parser Param
middle = Param <$> takeTill1 (\x -> x `elem` " \0\r\n") <?> "middle"

trailing :: Parser ByteString
trailing = takeTill (\x -> x `elem` "\0\r\n") <?> "trailing" 
        
user :: Parser User
user = User <$> takeTill (\x -> x `elem` " @\0\r\n") <?> "user"


parseIRC :: ByteString -> (ByteString, Either ([String], String) Message)
parseIRC bs =
    case parse message bs of
        Fail i xs x -> (i, Left (xs,x))
        Done i r    -> (i, Right r)
        Partial cnt -> (bs, Left ([], "PARTIAL"))