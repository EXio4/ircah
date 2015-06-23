module CAH.Cards.Import where

import           CAH.Cards.Types
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BS
import           Data.Aeson
import           Data.Set (Set)
import qualified Data.Set as S
import           System.IO
import           Control.Monad
import           Control.Applicative

instance FromJSON WhiteCard where
    parseJSON (Object v) = do
                (x :: Text) <- v .: "type"
                guard (x == "Answer")
                WhiteCard <$> v .: "value"
    parseJSON _          = mzero
    
instance FromJSON BlackCard where
    parseJSON (Object v) = do
                (x :: Text) <- v .: "type"
                guard (x == "Question")
                BlackCard . convertT <$> v .: "value"
    parseJSON _          = mzero
    
    
-- naive function
convertT :: String -> [HText]
convertT xs =
        let v = go id xs
        in if countHoles (BlackCard v) == 0 
              then v ++ [Txt " ", InvisibleHole]
              else v 
    where go dlist ('%':'s':xs) = Txt (T.pack (dlist [])) : VisibleHole : go id xs
          go dlist ('%':'%':xs) = go (dlist . ('%':)) xs
          go dlist (x:xs) = go (dlist . (x:)) xs
          go dlist []     = [Txt (T.pack (dlist []))]
          
parseCards :: (Ord a, FromJSON a) => FilePath -> IO (Maybe (Set a))
parseCards = fmap decode . BS.readFile
