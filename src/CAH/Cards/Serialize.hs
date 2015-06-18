{-# LANGUAGE OverloadedStrings #-}
module CAH.Cards.Serialize where

import           CAH.Cards.Types
import           System.IO
import           Control.Monad
import           Data.Monoid
import           Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Yaml as YAML
import           Parser.Utils

-- this should return IO (Either String Pack)

load :: FilePath -> IO Pack
load directory 
    =   Pack
    <$> loadMetadata (directory <> "/metadata")
    <*> loadWhite    (directory <> "/pack/white.txt")
    <*> loadBlack    (directory <> "/pack/black.txt")
    
loadMetadata :: FilePath -> IO Metadata
loadMetadata ps = do
    x <- fmap YAML.decode (BS.readFile ps)
    case x of
         Nothing -> putStrLn ("error loading metadata ~" <> ps) *> undefined
         Just v  -> return v 

loadWhite :: FilePath -> IO (Set WhiteCard)
loadWhite = parseOver parseWhiteCard

loadBlack = parseOver parseBlackCard

parseOver :: Ord a => Parser a -> FilePath -> IO (Set a)
parseOver parser path = do
    file <- BS.readFile path
    let x = parseOnly (fmap S.fromList (many parser)) file
    case x of
         Left err -> S.empty <$ putStrLn ("error loading card file ~ " <> path)
         Right x  -> return x 

parseWhiteCard :: Parser WhiteCard
parseWhiteCard = (WhiteCard . T.decodeUtf8 <$> takeWhile1 (/= '\n')) <* char '\n'
    
parseBlackCard :: Parser BlackCard
parseBlackCard = BlackCard <$> go
    where consel :: Parser [HText] -> Parser [HText]
          consel x = do w <- optional (takeTill1 (\x -> x == '\n' || x == '_'))
                        case w of
                             Nothing -> x
                             Just w' -> (Txt (T.decodeUtf8 w') :) <$> x
          go :: Parser [HText]
          go   = consel (onLookH '_' rest (return [])) 
          rest :: Parser [HText]
          rest = (:) 
                <$> (   (InvisibleHole <$ string "__")
                    <|> (VisibleHole   <$ string "_"))
                <*> go 
            
        
save :: FilePath -> Pack -> IO ()
save directory (Pack metadata white black) = do
    saveMetadata metadata (directory <> "/metadata")
    saveWhite    white    (directory <> "/pack/white.txt")
    saveBlack    black    (directory <> "/pack/black.txt")    
    
saveMetadata :: Metadata -> FilePath -> IO ()
saveMetadata md fp = withFile fp WriteMode $ \h -> BS.hPutStr h (YAML.encode md)
saveWhite :: Set WhiteCard -> FilePath -> IO ()
saveWhite set fp = withFile fp WriteMode $ \h ->
                    mapM_ (\(WhiteCard x) -> T.hPutStrLn h x) (S.toList set)
saveBlack :: Set BlackCard -> FilePath -> IO ()
saveBlack set fp = withFile fp WriteMode $ \h ->
                    mapM_ (\(BlackCard x) -> mapM_ (put h) x >> hPutStrLn h "") (S.toList set)
    where put h (Txt x) = T.hPutStr h x
          put h InvisibleHole = hPutStr h "__"
          put h VisibleHole   = hPutStr h "_"