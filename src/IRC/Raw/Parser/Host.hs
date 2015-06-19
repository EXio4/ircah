module IRC.Raw.Parser.Host (host, parseHost) where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid

import           IRC.Raw.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Attoparsec.ByteString.Char8

-- naive parser

opt :: Parser ByteString -> Parser ByteString
opt = option "FAIL"

host :: Parser Host
host = Host <$> domain

domain :: Parser ByteString
domain = subdomain <|> " "

subdomain :: Parser ByteString
subdomain = do
    x <- label 
    y <- optional (char '.')
    case y of
         Nothing -> return x
         Just p  -> (\z -> x <> BS.singleton p <> z) <$> subdomain

label :: Parser ByteString
label = BS.cons <$> letter_ascii <*> opt f
    where f = do
            y <- optional let_dig_hyp
            case y of
                 Nothing -> return BS.empty
                 Just '-' ->  BS.cons '-' <$> f
                 Just  c  ->  BS.cons  c  <$> opt f
            
ldh_str :: Parser ByteString
ldh_str = BS.pack <$> many1 let_dig_hyp

let_dig_hyp :: Parser Char
let_dig_hyp = let_dig <|> char '-'

let_dig :: Parser Char
let_dig = letter_ascii <|> digit

parseHost :: ByteString -> Either String Host
parseHost = parseOnly host