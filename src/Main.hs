{-# LANGUAGE OverloadedStrings #-}

module Main (main,client) where

import           CAH.Cards.Types
import           CAH.Cards.Import
import qualified CAH.Cards.Serialize as Cards
import           Data.Set (Set)
import qualified IRC.Client as IRC
import           IRC.Types
import qualified IRC.Commands as IRC
import qualified IRC.Raw as Raw
import           Control.Monad
import           Control.Applicative
import           Data.Functor
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
    case x of
         ["convert", format, json, target] -> convertFN format json target
         ["load_cards", cardPack] -> 
                print =<< Cards.load cardPack
         [network  , port  , nick, ch]     ->
            IRC.connectToIRC (cfg network (read port) nick ch) (forever . client)
         xs -> putStrLn "invalid params"
         
         
client :: IRC -> IO ()
client irc = do
    IRC.onIRC irc 
        (\x -> return ())
        [IRC.onChannelMsg $ \user channel msg -> do
            case msg of
                 "!ping" ->
                     Just () <$ IRC.msg irc channel "pong"
                 x      -> 
                     Nothing <$ putStrLn ("msg :: " ++ show x)
        ]

        