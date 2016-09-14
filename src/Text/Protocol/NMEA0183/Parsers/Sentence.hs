module Text.Protocol.NMEA0183.Parsers.Sentence where

import Control.Applicative
import Data.Char
import qualified Data.Text as T
import Text.Protocol.NMEA0183.Types.Talker
import Text.Parser.Char
import Text.Parser.Combinators

parseTalkerId :: (Monad m, CharParsing m) => m TalkerIdentifier
parseTalkerId = parseU <|> parseNormal
  where
    parseU = do
      _ <- char 'U'
      n <- digit
      -- This 'read' is bad (partial), but we are relying on "parsers" indeed
      -- giving us a digit back (the parser should/will fail otherwise).
      -- Nevertheless, we should possibly 'readMay' and fail ourselves.
      return (U (read [n]))
    parseNormal = do
      x <- count 2 upper
      return $ case x of
        "AB" -> AB
        "AD" -> AD
        "AG" -> AG
        "AP" -> AP
        "BN" -> BN
        "CC" -> CC
        "CD" -> CD
        "CM" -> CM
        "CS" -> CS
        "CT" -> CT
        "CV" -> CV
        "CX" -> CX
        "DE" -> DE
        "DF" -> DF
        "DU" -> DU
        "EC" -> EC
        "EP" -> EP
        "ER" -> ER
        "GP" -> GP
        "HC" -> HC
        "HE" -> HE
        "HN" -> HN
        "II" -> II
        "IN" -> IN
        "LA" -> LA
        "LC" -> LC
        "MP" -> MP
        "NL" -> NL
        "OM" -> OM
        "OS" -> OS
        "RA" -> RA
        "SD" -> SD
        "SN" -> SN
        "SS" -> SS
        "TI" -> TI
        "TR" -> TR
        "UP" -> UP
        "VD" -> VD
        "DM" -> DM
        "VW" -> VW
        "WI" -> WI
        "YC" -> YC
        "YD" -> YD
        "YF" -> YF
        "YL" -> YL
        "YP" -> YP
        "YR" -> YR
        "YT" -> YT
        "YV" -> YV
        "YX" -> YX
        "ZA" -> ZA
        "ZC" -> ZC
        "ZQ" -> ZQ
        "ZV" -> ZV

parseSentenceIdentifier :: CharParsing m => m T.Text
parseSentenceIdentifier = T.pack <$> count 3 upper

parseChecksum :: (CharParsing m, Monad m) => m (Char, Char)
parseChecksum = do
  _ <- char ','
  _ <- char '*'
  (,) <$> upperHexDigit <*> upperHexDigit
  where
    upperHexDigit =
      satisfy (\c -> (isHexDigit c && isUpper c) || isDigit c)
      <?> "uppercase hexadecimal digit"

-- | Parse a NMEA 0183 sentence. The idea is to use this like
-- @parseSentence parseGGA :: Parser (TalkerSentence GGA)@
parseSentence :: (CharParsing m, Monad m) => m a -> m (TalkerSentence a)
parseSentence f = do
  _ <- char '$'
  talkerId <- parseTalkerId
  sentenceIdentifier <- parseSentenceIdentifier
  _ <- char ','
  dataFields <- f
  checksum <- optional parseChecksum
  _ <- char '\r'
  _ <- char '\n'
  TalkerSentence <$> pure talkerId
                 <*> pure sentenceIdentifier
                 <*> pure dataFields
                 <*> pure checksum
