{-# LANGUAGE OverloadedStrings #-}

module IRC.Raw.Parser (parseIRC) where

import           Prelude hiding (takeWhile)
import           Control.Applicative
import           Control.Monad

import           IRC.Raw.Parser.Host
import           IRC.Raw.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Attoparsec.ByteString.Char8
{-
  Char8, per name, doesn't work nicely with >0xFF characters
  this is good enough for parsing the barebones of IRC
  
  decoding into UTF8 should be done by the high level library (should it?)
-}

takeTill1 :: (Char -> Bool) -> Parser ByteString 
takeTill1 f = takeWhile1 (not . f)

lookH :: Char -> Parser ()
lookH ch = do
    x <- peekChar'
    if x == ch
    then return ()
    else fail "lookH.." 

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
tags = Tags <$> many1 tag

tag :: Parser Tag
tag = Tag <$> key <*> optional (char '=' *> escapedValue)

key :: Parser Key
key = Key <$> optional (vendor <* char '/') <*> takeWhile (\x -> isAlpha_ascii x || isDigit x || x == '-')  

escapedValue :: Parser ByteString
escapedValue = takeTill (\x -> x `elem` "; \0\r\n")

vendor :: Parser Vendor
vendor = Vendor <$> host

prefix :: Parser Prefix
prefix
    = ((Prefix 
        <$> nick
        <*> optional (char '!' *> user)
        <*> optional (char '@' *> host))
        <* lookH ' ')
    <|> (ServerName <$> host)
nick :: Parser Nick
nick = Nick <$> nick'
    where nick' =
            BS.cons
                <$> letter_ascii
                <*> takeWhile (\x -> isAlpha_ascii x || isDigit x || special x)
          special x = x `elem` "-[]\\`^{}" 
          

command :: Parser Command
command
    =   Command <$> takeWhile1 isAlpha_ascii
    <|> (\x y z -> CmdNumber (read [x,y,z])) <$> digit <*> digit <*> digit
    
params :: Parser Params
params = space *> (
        (\x -> Params [Param x]) <$> (char ':' *> trailing)
    <|> ((\x (Params y) -> Params (x:y)) <$> middle <*> params))

middle :: Parser Param
middle = Param <$> takeTill1 (\x -> x `elem` " \0\r\n")

trailing :: Parser ByteString
trailing = takeTill (\x -> x `elem` "\0\r\n")
        
user :: Parser User
user = User <$> takeTill (\x -> x `elem` " \0\r\n")


parseIRC :: ByteString -> (ByteString, Either ([String], String) Message)
parseIRC bs =
    case parse message bs of
        Fail i xs x -> (i, Left (xs,x))
        Done i r    -> (i, Right r)
        Partial cnt -> (bs, Left ([], "PARTIAL"))