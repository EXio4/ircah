module CAH.Cards.Serialize where

import           CAH.Cards.Types
import           System.IO
import           Control.Monad
import           Data.Monoid
import           Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Foldable as F
import           Data.Text (Text)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Yaml as YAML
import           Parser.Utils
import qualified Control.Exception as Exc

-- this should return IO (Either String Pack)

metadataFile,whiteFiles,blackFiles :: FilePath -> FilePath
[metadataFile,whiteFiles,blackFiles] = map (flip (<>)) ["/metadata", "/cards/whites.txt", "/cards/blacks.txt"]

load :: FilePath -> IO (Either [String] Pack)
load directory 
    =   (liftA3 Pack)
    <$> loadMetadata (metadataFile directory)
    <*> loadWhite    (whiteFiles   directory)
    <*> loadBlack    (blackFiles   directory)
    
save :: FilePath -> Pack -> IO ()
save directory (Pack metadata white black) = do
    saveMetadata metadata (metadataFile directory)
    saveWhite    white    (whiteFiles   directory)
    saveBlack    black    (blackFiles   directory)    
    
    
loadMetadata :: FilePath -> IO (Either [String] Metadata)
loadMetadata ps = do
    x <- fmap YAML.decode (BS.readFile ps)
    case x of
         Nothing -> return $ Left ["error loading metadata ~" <> ps <> "\n"]
         Just v  -> return $ Right v

loadWhite :: FilePath -> IO (Either [String] (Vector WhiteCard))
loadWhite = parseOver parseWhiteCard

loadBlack :: FilePath -> IO (Either [String] (Vector BlackCard))
loadBlack = parseOver parseBlackCard
    

parseO :: (ByteString -> Maybe a) -> ByteString -> [a]
parseO f xs = [ x | y <- BS.lines xs, Just x <- [f y]]
    
parseOver :: Ord a => (ByteString -> Maybe a) -> FilePath -> IO (Either [String] (Vector a))
parseOver parser path = do
    file <- Exc.try (BS.readFile path)
    case file of 
         Left (x :: IOError) -> return $ Left ["error loading card file ~ " <> path <> " ( " <>  show x <> ")"]
         Right file -> return $ Right (V.fromList (parseO parser file)) 

parseWhiteCard :: ByteString -> Maybe WhiteCard
parseWhiteCard = Just . WhiteCard . T.decodeUtf8
        
parseBlackCard :: ByteString -> Maybe BlackCard
parseBlackCard xs = either (const Nothing) Just (parseOnly (BlackCard <$> go) xs)
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
            
saveMetadata :: Metadata -> FilePath -> IO ()
saveMetadata md fp = withFile fp WriteMode $ \h -> BS.hPutStr h (YAML.encode md)
saveWhite :: Vector WhiteCard -> FilePath -> IO ()
saveWhite vect fp = withFile fp WriteMode $ \h ->
                    F.mapM_ (\(WhiteCard x) -> T.hPutStrLn h x) vect
saveBlack :: Vector BlackCard -> FilePath -> IO ()
saveBlack vect fp = withFile fp WriteMode $ \h ->
                    F.mapM_ (\(BlackCard x) -> mapM_ (put h) x >> hPutStrLn h "") vect
    where put h (Txt x) = T.hPutStr h x
          put h InvisibleHole = hPutStr h "__"
          put h VisibleHole   = hPutStr h "_"