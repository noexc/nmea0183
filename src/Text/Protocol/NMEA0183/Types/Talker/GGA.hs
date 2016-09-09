module Text.Protocol.NMEA0183.Types.Talker.GGA where

import Data.Maybe (maybe)
import qualified Data.Text as T

data GGANorthSouth = N | S deriving (Eq, Ord, Show)
data GGAEastWest = E | W deriving (Eq, Ord, Show)
data GGALatitude = GGALatitude Double deriving (Eq, Ord)
data GGALongitude = GGALongitude Double deriving (Eq, Ord)

instance Show GGALatitude where
  show (GGALatitude d) = show d

instance Show GGALongitude where
  show (GGALongitude d) = show d

data GPSQuality =
    FixNotAvailable
  | GPSFix
  | DGPSFix
  | PPSFix
  | RealTimeKinematic
  | FloatRTK
  | Estimated
  | ManualInputMode
  | SimulationMode
  deriving (Eq, Ord, Enum)

instance Show GPSQuality where
  show = show . fromEnum

data GGA =
  GGA {
    _time :: T.Text -- TODO: UTCTime
  , _latitude :: GGALatitude
  , _northSouth :: GGANorthSouth
  , _longitude :: GGALongitude
  , _eastWest :: GGAEastWest
  , _gpsQuality :: GPSQuality
  , _numSatellites :: Int
  , _horizontalDilution :: Double
  , _antennaAltitude :: Double
  , _antennaAltitudeUnits :: Char
  , _geoidalSeparation :: Double
  , _geoidalSeparationUnits :: Char
  , _dGPSAge :: Maybe Double
  , _dGPSReferenceStationID :: Maybe T.Text
  } deriving (Eq, Ord)

instance Show GGA where
  show (GGA a b c d e f g h i j k l m n) =
    T.unpack a ++ "," ++ show b ++ "," ++ show c ++ "," ++ show d ++ "," ++
    show e ++ "," ++ show f ++ "," ++ show g ++ "," ++ show h ++ "," ++
    show i ++ "," ++ show j ++ "," ++ show k ++ "," ++ show l ++ "," ++
    maybe "" show m ++ "," ++ maybe "" T.unpack n
