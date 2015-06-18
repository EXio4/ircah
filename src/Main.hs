{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           CAH.Cards.Types
import           CAH.Cards.Import
import           CAH.Cards.Serialize
import           Data.Set (Set)
import qualified IRC.Client as IRC
import           IRC.Types
import qualified IRC.Commands as IRC
import           IRC.Raw
import           Control.Monad
import           Control.Concurrent
import           System.Environment
import           System.IO
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

convertFN :: String -> String -> String -> IO ()
convertFN typ json out
    = case typ of
           "black" -> do
               x <- parseCards json
               case x of
                    Nothing -> putStrLn "error parsing black cards file"
                    Just v  -> withFile out WriteMode (`exportBlackCards` v)
           "white" -> do
               x <- parseCards json
               case x of
                    Nothing -> putStrLn "error parsing white cards file"
                    Just v  -> withFile out WriteMode (`exportWhiteCards` v)
           _ -> putStrLn "wat"

cfg net p nick ch = IRCConfig net p nick Nothing [ChannelCfg ch Nothing]
           
main :: IO ()
main = do
    x <- getArgs
    [cmd, port, nick, channel] <- getArgs
    case x of
         ["convert", format, json, target] -> convertFN format json target
         [network  , port  , nick, ch]     -> IRC.connectToIRC (cfg network (read port) nick ch) (forever client)
         
         
client :: IRC -> IO ()
client irc = do
    IRC.onIRC irc 
        (\x -> print x)
        [IRC.onChannelMsg $ \user channel msg -> do
            case msg of
                 "!ping" -> IRC.msg irc channel "pong"
                 _       -> return ()
        ]