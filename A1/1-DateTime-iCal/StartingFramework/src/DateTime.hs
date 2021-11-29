module DateTime where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord)


-- Exercise 1

{-- Parsers for the Date datatype --}
parseYear :: Parser Char Year
parseYear = (\m c d y -> Year (1000*m + 100*c + 10*d + y)) <$> digit <*> digit <*> digit <*> digit

parseMonth :: Parser Char Month
parseMonth = (\fst snd -> Month (10*fst + snd)) <$> digit <*> digit

parseDay :: Parser Char Day
parseDay = (\fst snd -> Day (10*fst + snd)) <$> digit <*> digit

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

{-- Parsers for the Time datatype --}
parseSecond :: Parser Char Second
parseSecond = (\fst snd -> Second (10*fst + snd)) <$> digit <*> digit

parseMinute :: Parser Char Minute
parseMinute = (\fst snd -> Minute (10*fst + snd)) <$> digit <*> digit

parseHour :: Parser Char Hour
parseHour = (\fst snd -> Hour (10* fst + snd)) <$> digit <*> digit

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

{-- Parser for utc--}
parseUtc :: Parser Char Bool
parseUtc = Const True <$> symbol 'Z' <|> succeed False

parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* symbol 'T' <*> parseTime <*> parseUtc

-- Exercise 2

run :: Parser a b -> [a] -> Maybe b
run p xs = record (p xs)
    where 
        record [] = Nothing
        record ((_, rest_str):xs)
            | null rest_str = Just x
            | otherwise = record xs
-- Exercise 3
printDateTime :: DateTime -> String
printDateTime = undefined

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
