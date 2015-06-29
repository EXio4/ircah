{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Translate where

import           IRC.Types
import           CAH.Cards.Types
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text (Text)

int :: Integer -> Text
int = T.pack . show


showCards :: [(Integer,WhiteCard)] -> Text
showCards = T.intercalate ", " . map f
    where f (n, WhiteCard x) = int n <> ": " <> x
          
showPlayers :: [Nick] -> Text
showPlayers []    = ""
showPlayers [Nick x]   = x
showPlayers [Nick x,Nick y] = x <> " and " <> y
showPlayers (Nick x:xs) = x <> ", " <> showPlayers xs

showBlack :: BlackCard -> Text
showBlack (BlackCard x) = T.concat (map f x)
    where f (Txt x) = x
          f VisibleHole   = "____"
          f InvisibleHole = ""

fillBlackCard :: BlackCard -> [WhiteCard] -> Text
fillBlackCard (BlackCard x) = go x 
    where go (Txt x:xs)            rs  = x <> go xs rs
          go (VisibleHole:xs)   (WhiteCard r:rs) = r <> go xs rs
          go (InvisibleHole:xs) (WhiteCard r:rs) = r <> go xs rs
          go []                    []  = ""
          go _                     _   = "???"
          
translate :: TextMessage -> Text
translate x =
    case x of
        NoError                 ->
            "Nothing bad happened, wut?"

        UserNotPlaying (Nick n) ->
            n <> ": you aren't playing, duh!"

        UserNotIdentified (Nick n) ->
            n <> ": you must identify to play this game"

        GameAlreadyBeingPlayed ->
            "we're already playing, you can just !join"
            
        NotEnoughPlayers n ->
            "a game needs (at least) " <> int n <> " players"
            
        GameStarted pls ->
            "wake up! a new game is being played! " <> showPlayers pls
            
        MustPickNCards (Nick n) m ->
            "you must pick " <> int m <> " cards"

        CzarPicked (Nick czar) (Nick picked) black white (Points n) ->
            czar <> " picked " <> fillBlackCard black white <> " and " <> picked <> " gets " <> int n <> " awesome point" <>
                        (if n > 1
                        then "s"
                        else "")
        YourCardsAre (Nick nick) xs -> 
            nick <> "! your cards are: " <> showCards xs
        
        TheCardsAre ->
            "Everyone picked and their cards are:"
            
        AlreadyPlaying (Nick nick) ->
            nick <> " you are already playing!"
            
        ReplacingOld (Nick old) (Nick new) ->
            new <> " replaces " <> old
            
        JoinPlayer (Nick n) ->
            n <> " joins"
            
        LeavePlayer (Nick n) ->
            n <> " leaves"
            
        CardsPicked (Nick nick) n black white ->
            int n <> ": " <> fillBlackCard black white
            
        PlayersList [] ->
            "Nobody's playing"
        
        PlayersList xs ->
            "Players: " <> showPlayers xs
            
        PlayersWin [] _ ->
            "nobody won :("

        PlayersWin [Nick x]  (Points p) ->
            x <> " won the game with " <> int p <> " awesome points!"

        PlayersWin _ _ ->
            "multiple winners isn't handled.. yet"

        StatusWaitingCzar (Nick czar) ->
            "The czar is " <> czar <> "... and we're waiting for him!"
            
        StatusWaitPlayers (Nick czar) players -> 
            "The czar is " <> czar <> "; we're waiting for: " <> showPlayers players
            
        Table (Nick czar) blackCard ->
            "The czar is " <> czar <> "; the black card is " <> showBlack blackCard
            
        Debug x -> 
            "Debug info: " <> x