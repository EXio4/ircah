{-# LANGUAGE Arrows #-}
module Main (main) where

import           Prelude hiding ((.), id)
import           Common.StateMachine
import           CAH.Cards.Types
import           CAH.Cards.Import
import qualified CAH.Cards.Serialize as Cards
import           Data.Set (Set)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           IRC.Game
import           CAH.Types
import           CAH.Game
import           IRC.FRPTypes
import qualified IRC.Client as IRC
import qualified IRC.NickTracking as Tracker
import qualified IRC.Raw as Raw
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
import           Control.Arrow
import           Control.Wire

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
            
   
    
    
--logic :: Nick -> Channel -> Wire s () (IRC IO) Raw.Message ((Tracker.NickTracker, Players), EventPlayer)
logic nick channel {-
    = dup >>> first (Tracker.tracker nick)
          >>> arr (\(x,y) -> ((x,(x,y)), (x,y)))
          >>> first (second (setupPlayerTracking channel))
          >>> second (toLogicCmd channel) -}
    = proc msg -> do
        trk     <- Tracker.tracker nick         -< msg
        players <- setupPlayerTracking channel  -< (trk,msg)
        logic   <- toLogicCmd channel           -< (trk, msg)
        returnA -< (trk, players, logic)
        -- ??
                      
main :: IO ()
main = do
    x <- getArgs
    case x of
         ["convert", format, json, target] -> convertFN format json target
         ["load_cards", cardPack] -> 
                print =<< Cards.load cardPack
         [network  , port  , nick, ch]     -> do
            let cards = undefined
                nick't = T.pack nick
                ch't   = T.pack ch
            IRC.connectToIRC (cfg network (read port) nick ch) $ do
                loopG (ircMessage >>> logic nick't ch't) clockSes
         xs -> putStrLn "invalid params"
         
    

        