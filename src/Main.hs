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
import qualified IRC.NickTracking as IRC.NickTracking
import           IRC.NickTracking (NickTracker, uid)
import           Control.Monad
import           Control.Applicative
import           Data.Functor
import           Data.Monoid
import           Control.Concurrent
import           System.Environment
import           System.IO
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import           Data.Text (Text)

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
            IRC.connectToIRC (cfg network (read port) nick ch) (client (IRC.NickTracking.defTracker (T.pack nick)))
         xs -> putStrLn "invalid params"
         
         
client :: NickTracker -> IRC -> IO ()
client tracker irc = do
    IRC.onIRC irc
        (\_ -> client tracker irc)
        (IRC.NickTracking.handlers tracker (\trk -> client trk irc) <>
        [IRC.onJOIN $ \user channel metadata -> do
                let user' = userNick user
                case IRC.NickTracking.getUID tracker user' of
                     Just v | v == (uid 0)
                         -> IRC.cmd irc "WHO" [channel, "%na"]
                     _   -> do
                         IRC.cmd irc "WHO" [user'  , "%na"]
                return (Just ())
        ,IRC.onChannelMsg $ \user channel msg -> do
            case msg of
                 "!ping" -> 
                     IRC.msg irc channel "pong"
                 "!me"   ->
                     IRC.msg irc channel (T.pack (show (IRC.NickTracking.getUID tracker (userNick user))))
                 "!quit" -> do
                     IRC.cmd irc "QUIT" ["..."]
                     error "forced crash"
                 x       -> putStrLn ("msg :: " ++ show x)
            return (Just ())
        ])
    client tracker irc

        