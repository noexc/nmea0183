module Text.Protocol.NMEA0183.Parsers.Sentence where

import Control.Applicative
import qualified Data.Text as T
import Text.Protocol.NMEA0183.Types.Talker
import Text.Parser.Char
import Text.Parser.Combinators

parseTalkerId :: (Monad m, CharParsing m) => m TalkerIdentifier
parseTalkerId = do
  x <- string "P" <|> count 2 upper
  return $ case x of
    "AG" -> AG
    "AP" -> AP
    "CD" -> CD
    "CR" -> CR
    "CS" -> CS
    "CT" -> CT
    "CV" -> CV
    "CX" -> CX
    "DF" -> DF
    "EC" -> EC
    "EP" -> EP
    "ER" -> ER
    "GP" -> GP
    "HC" -> HC
    "HE" -> HE
    "HN" -> HN
    "II" -> II
    "IN" -> IN
    "LC" -> LC
    "P"  -> P
    "RA" -> RA
    "SD" -> SD
    "SN" -> SN
    "SS" -> SS
    "TI" -> TI
    "VD" -> VD
    "DM" -> DM
    "VW" -> VW
    "WI" -> WI
    "YX" -> YX
    "ZA" -> ZA
    "ZC" -> ZC
    "ZQ" -> ZQ
    "ZV" -> ZV

parseSentenceIdentifier :: CharParsing m => m T.Text
parseSentenceIdentifier = T.pack <$> count 3 upper

-- | Parse a NMEA 0183 sentence. The idea is to use this like
-- @parseSentence parseGGA :: Parser (TalkerSentence GGA)@
parseSentence :: (CharParsing m, Monad m) => m a -> m (TalkerSentence a)
parseSentence f = do
  _ <- char '$'
  talkerId <- parseTalkerId
  sentenceIdentifier <- parseSentenceIdentifier
  _ <- char ','
  dataFields <- f
  TalkerSentence <$> pure talkerId
                 <*> pure sentenceIdentifier
                 <*> pure dataFields
                 <*> pure Nothing
