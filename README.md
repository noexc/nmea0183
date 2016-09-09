# nmea0183

A Haskell library for parsing, emitting, and manipulating NMEA0183 packets.

## Demo

```haskell
> :set -XOverloadedStrings
> import Data.Attoparsec.Text
> import Text.Protocol.NMEA0183.Types.Talker.GGA
> import Text.Protocol.NMEA0183.Parsers.Sentence
> import Text.Protocol.NMEA0183.Parsers.Talker.GGA
> let Right f = parseOnly (parseSentence parseGGA) "$GPGGA,092750.000,5321.6802,N,00630.3372,W,1,8,1.03,61.7,M,55.2,M,,*76\r\n"
> f
$GPGGA,92750.0,5321.6802,N,630.3372,W,1,8,1.03,61.7,'M',55.2,'M',,*76
> f ^. dataFields
92750.0,5321.6802,N,630.3372,W,1,8,1.03,61.7,'M',55.2,'M',,*76
> f ^. dataFields . numSatellites
8
> :t f
f :: TalkerSentence GGA
```

## License

BSD-2
