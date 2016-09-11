module Text.Protocol.NMEA0183.Parsers.Talker.GGA where

import Control.Applicative
import Data.Char
import qualified Data.Text as T
import Text.Protocol.NMEA0183.Types.Talker.GGA
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

parseNS :: (CharParsing m, Monad m) => m GGANorthSouth
parseNS = do
  ns <- char 'N' <|> char 'S'
  return $ case ns of
    'N' -> N
    'S' -> S

parseEW :: (CharParsing m, Monad m) => m GGAEastWest
parseEW = do
  ew <- char 'E' <|> char 'W'
  return $ case ew of
    'E' -> E
    'W' -> W

parseGGA :: (CharParsing m, TokenParsing m, Monad m) => m GGA
parseGGA = do
  time <- double
  _ <- char ','
  lat <- GGALatitude <$> double
  _ <- char ','
  ns <- parseNS
  _ <- char ','
  long <- GGALongitude <$> double
  _ <- char ','
  ew <- parseEW
  _ <- char ','
  qual <- toEnum . read . return <$> digit :: (CharParsing m, Monad m) => m GPSQuality
  _ <- char ','
  sats <- read <$> some digit
  _ <- char ','
  hDilution <- double
  _ <- char ','
  alt <- double
  _ <- char ','
  altUnits <- upper
  _ <- char ','
  hGeoid <- double
  _ <- char ','
  hGeoidUnits <- upper
  _ <- char ','
  dGPSLast <- optional (read <$> some digit)
  _ <- char ','
  dGPSRefId <- fmap (fmap T.pack) (optional (many . noneOf $ ",\r"))
  return $
    GGA
    time
    lat
    ns
    long
    ew
    qual
    sats
    hDilution
    alt
    altUnits
    hGeoid
    hGeoidUnits
    dGPSLast
    dGPSRefId
