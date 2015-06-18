{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           CAH.Cards.Types
import           CAH.Cards.Import
import           CAH.Cards.Serialize
import           Data.Set (Set)
import           IRC
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

main :: IO ()
main = do
    [network, port, nick, channel] <- getArgs
    if network == "convert"
    then convertFN port nick channel
    else connectToIRC_raw network (read port) (irc (BS.pack nick) (BS.pack channel))
    where irc nick channel i = do
            x <- forkIO (reader i channel)
            irc_send i (Message Nothing Nothing (Command "USER") (Params [Param "x", Param "x", Param "x", Param "x"]))
            irc_send i (Message Nothing Nothing (Command "NICK") (Params [Param nick]))
            writer i
            
reader :: IRC -> ByteString -> IO ()
reader irc channel = forever $ do
    x <- irc_read irc
    case x of 
         (Message _ _ (Command "PING") params) ->
            irc_send irc (Message Nothing Nothing (Command "PONG") params)
         (Message _ _ (CmdNumber 376)  _) ->
            irc_send irc (Message Nothing Nothing (Command "JOIN") (Params [Param channel]))
         _ -> return ()
         
         
writer :: IRC -> IO ()
writer irc = forever $ do
    threadDelay 50000