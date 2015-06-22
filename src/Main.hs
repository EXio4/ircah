module Main (main) where

import           Common.StateMachine
import           CAH.Cards.Types
import           CAH.Cards.Import
import qualified CAH.Cards.Serialize as Cards
import           Data.Set (Set)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           IRC.Game
import           CAH.Game
import           IRC.FRPTypes
import qualified IRC.Client as IRC
import qualified IRC.NickTracking as Tracker
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
            --let game = setupPlayTracking (T.pack ch) Tracker.trackerSM 
            --in IRC.connectToIRC (cfg network (read port) nick ch) (IRC.runSM game (Tracker.defTracker (T.pack nick), emptyPlayersList))
            IRC.connectToIRC (cfg network (read port) nick ch) $ do
                loopG (wireEx f) clockSes
         xs -> putStrLn "invalid params"
         
    

        