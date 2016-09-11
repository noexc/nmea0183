{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Text.Protocol.NMEA0183.Types.Talker where

import Control.Lens
import qualified Data.Text as T

data TalkerIdentifier =
    AG -- ^ Autopilot - General
  | AP -- ^ Autopilot - Magnetic
  | CD -- ^ Communications – Digital Selective Calling (DSC)
  | CR -- ^ Communications – Receiver / Beacon Receiver
  | CS -- ^ Communications – Satellite
  | CT -- ^ Communications – Radio-Telephone (MF/HF)
  | CV -- ^ Communications – Radio-Telephone (VHF)
  | CX -- ^ Communications – Scanning Receiver
  | DF -- ^ Direction Finder
  | EC -- ^ Electronic Chart Display & Information System (ECDIS)
  | EP -- ^ Emergency Position Indicating Beacon (EPIRB)
  | ER -- ^ Engine Room Monitoring Systems
  | GP -- ^ Global Positioning System (GPS)
  | HC -- ^ Heading – Magnetic Compass
  | HE -- ^ Heading – North Seeking Gyro
  | HN -- ^ Heading – Non North Seeking Gyro
  | II -- ^ Integrated Instrumentation
  | IN -- ^ Integrated Navigation
  | LC -- ^ Loran C
  | P  -- ^ Proprietary Code
  | RA -- ^ RADAR and/or ARPA
  | SD -- ^ Sounder, Depth
  | SN -- ^ Electronic Positioning System, other/general
  | SS -- ^ Sounder, Scanning
  | TI -- ^ Turn Rate Indicator
  | VD -- ^ Velocity Sensor, Doppler, other/general
  | DM -- ^ Velocity Sensor, Speed Log, Water, Magnetic
  | VW -- ^ Velocity Sensor, Speed Log, Water, Mechanical
  | WI -- ^ Weather Instruments
  | YX -- ^ Transducer
  | ZA -- ^ Timekeeper – Atomic Clock
  | ZC -- ^ Timekeeper – Chronometer
  | ZQ -- ^ Timekeeper – Quartz
  | ZV -- ^ Timekeeper – Radio Update, WWV or WWVH
  deriving (Eq, Ord, Show)

data TalkerSentence a =
  TalkerSentence {
    _talkerIdentifier :: TalkerIdentifier
  , _sentenceIdentifier :: T.Text
  , _dataFields :: a
  , _checksum :: Maybe (Char, Char)
  } deriving (Eq, Functor, Ord)

makeClassy ''TalkerSentence

instance Show a => Show (TalkerSentence a) where
  show (TalkerSentence a b c (Just d)) =
    "$" ++ show a ++ T.unpack b ++ "," ++ show c ++ ",*" ++ show d
  show (TalkerSentence a b c Nothing) =
    "$" ++ show a ++ T.unpack b ++ "," ++ show c
