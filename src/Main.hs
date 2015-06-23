module Main (main) where

import           CAH.Cards.Types
import           CAH.Cards.Import
import qualified CAH.Cards.Serialize as Cards
import           Data.Set (Set)
import           IRC.Game
import qualified IRC.Client as IRC
import           IRC
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
         [network  , port  , nick, ch]     -> do
                defPackE <- Cards.load "packs/default"
                case defPackE of
                     Left xs -> do
                         putStrLn "error loading default pack"
                         forM_ xs print
                     Right defPack -> 
                         let irc_config = cfg network (read port) nick ch
                         in runGame irc_config [defPack] (game (T.pack nick) (T.pack ch))
         xs -> putStrLn "invalid params"
         
    

        