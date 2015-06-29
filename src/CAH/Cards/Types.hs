{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CAH.Cards.Types where

import           Data.Text (Text)
import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.Foldable as F
import           System.IO
import           Data.Aeson


data Metadata = Metadata {
     pack_name     :: Text
    ,pack_language :: Language
} deriving (Show,Eq)

data Pack = Pack {
     pack_metadata   :: Metadata
    ,pack_whitecards :: Set WhiteCard
    ,pack_blackcards :: Set BlackCard
} deriving (Show,Eq)

newtype Language = Lang Text
    deriving (Show,Eq,FromJSON,ToJSON)

data WhiteCard = WhiteCard Text
    deriving (Show,Eq,Ord)
data BlackCard = BlackCard [HText]
    deriving (Show,Eq,Ord)
data HText = VisibleHole
           | InvisibleHole
           | Txt Text
    deriving (Show,Eq,Ord)

defaultLanguage :: Language
defaultLanguage = Lang "en"

instance FromJSON Metadata where
    parseJSON (Object v)
            =   Metadata
            <$> v .: "name"
            <*> v .: "language"
    parseJSON _ = mzero
    
instance ToJSON Metadata where
    toJSON (Metadata name language) = object ["name" .= name, "language" .= language]
    
countHoles ::  BlackCard -> Int
countHoles (BlackCard x) = length (filter h x)
    where h VisibleHole   = True
          h InvisibleHole = True
          h Txt{} = False
    
          
exportWhiteCards :: Handle -> Set WhiteCard -> IO ()
exportWhiteCards h = F.mapM_ (\(WhiteCard x) -> T.hPutStrLn h x)

exportBlackCards :: Handle -> Set BlackCard -> IO ()
exportBlackCards h = F.mapM_ (\(BlackCard x) -> f x)
    where f xs = mapM_ (T.hPutStr h . conv) xs >> T.hPutStrLn h ""
          conv (Txt x) = x
          conv VisibleHole   = "_"
          conv InvisibleHole = "__"