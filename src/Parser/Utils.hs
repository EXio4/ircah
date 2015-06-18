module Parser.Utils where

import           Prelude hiding (takeWhile)
import           Control.Applicative
import           Control.Monad

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Attoparsec.ByteString.Char8

takeTill1 :: (Char -> Bool) -> Parser ByteString 
takeTill1 f = takeWhile1 (not . f)

lookH :: Char -> Parser ()
lookH ch = do
    x <- peekChar'
    if x == ch
    then return ()
    else fail "lookH.." 
    
onLookH :: Char -> Parser a -> Parser a -> Parser a
onLookH ch true false = do
    x <- optional (lookH ch)
    case x of
         Nothing -> false
         Just  _ -> true