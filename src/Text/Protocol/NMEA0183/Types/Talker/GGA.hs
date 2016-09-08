module Text.Protocol.NMEA0183.Types.Talker.GGA where

import Data.Maybe (maybe)
import qualified Data.Text as T

data NorthSouth = N | S deriving (Eq, Ord, Show)
data EastWest = E | W deriving (Eq, Ord, Show)
data Latitude = Latitude Double deriving (Eq, Ord)
data Longitude = Longitude Double deriving (Eq, Ord)

instance Show Latitude where
  show (Latitude d) = show d

instance Show Longitude where
  show (Longitude d) = show d

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
  , _latitude :: Latitude
  , _northSouth :: NorthSouth
  , _longitude :: Longitude
  , _eastWest :: EastWest
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
