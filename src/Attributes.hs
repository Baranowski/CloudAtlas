{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Attributes where

import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Clock.POSIX
import System.Locale
import Data.List
import Text.Printf

import QAT

type Contact = (String, String)

data Attribute
    = Aint (Maybe Integer)
    | Astr (Maybe String)
    | Atime (Maybe UTCTime)
    | Aset Int (Maybe [Attribute])
    | Alist Int (Maybe [Attribute])
    | Abool (Maybe Bool)
    | Aquery (Maybe QAT)
    | Acontact (Maybe Contact)
    | Aduration (Maybe Integer)
    | Afloat (Maybe Double)
    deriving (Show, Eq, Read)
isNonNullQuery (Aquery (Just x)) = True
isNonNullQuery _ = False
isNull (Aint Nothing) = True
isNull (Astr Nothing) = True
isNull (Atime Nothing) = True
isNull (Aset _ Nothing) = True
isNull (Alist _ Nothing) = True
isNull (Abool Nothing) = True
isNull (Aquery Nothing) = True
isNull (Acontact Nothing) = True
isNull (Aduration Nothing) = True
isNull (Afloat Nothing) = True
isNull _ = False

sameType (Aint _) (Aint _) = True
sameType (Astr _) (Astr _) = True
sameType (Atime _) (Atime _) = True
sameType (Aset _ _) (Aset _ _) = True
sameType (Alist _ _) (Alist _ _) = True
sameType (Abool _) (Abool _) = True
sameType (Aquery _) (Aquery _) = True
sameType (Acontact _) (Acontact _) = True
sameType (Aduration _) (Aduration _) = True
sameType (Afloat _) (Afloat _) = True
sameType _ _ = False

getQuery (Aquery (Just x)) = x

time_format = "%Y/%m/%d %H:%M:%S%Q"
timeFromStr s = readTime defaultTimeLocale time_format s
epoch :: UTCTime
epoch = timeFromStr "2000/01/01 00:00:00.000"
eternity :: UTCTime
eternity = timeFromStr "2100/01/01 00:00:00.000"

timeToTimestamp ::   UTCTime -> Integer
timeToTimestamp t = round $ (utcTimeToPOSIXSeconds t) * dPrecision
timestampToTime i = posixSecondsToUTCTime $ (fromIntegral i)/dPrecision

decimal :: GenParser Char st Integer
decimal = do
    s <- many1 (oneOf ['0'..'9'])
    case (reads s)::[(Integer, String)] of
        [(i, "")] -> return i
        _ -> fail "Cannot parse integer"

parseDuration = do
    sgn <- ((char '+' >> return '+') <|> (char '-' >> return '-'))
    days <- decimal
    space
    hours <- decimal
    char ':'
    minutes <- decimal
    char ':'
    seconds <- decimal
    char '.'
    msecs <- decimal
    let absVal = ((((days*24)+hours)*60+minutes)*60+seconds)*1000 + msecs
    let res = case sgn of
                '+' -> absVal
                '-' -> -absVal
    return res

durFromStr s = case parse parseDuration "" s of
    Left _ -> Nothing
    Right x -> Just x


diffTConvert :: DiffTime -> NominalDiffTime
diffTConvert = fromRational . toRational
diffTRConvert :: NominalDiffTime -> DiffTime
diffTRConvert = fromRational . toRational

printNamedAttrib (name, attr) = "    " ++ name ++ (printAttrib attr) ++ "\n"

class MyShow a where
    myshow :: a -> String
instance MyShow Integer where
    myshow x = show x
instance MyShow String where
    myshow x = show x
instance MyShow UTCTime where
    myshow x = formatTime defaultTimeLocale time_format x
instance MyShow Bool where
    myshow True = "TRUE"
    myshow False = "FALSE"
instance MyShow Double where
    myshow x = show x
instance MyShow QAT where
    myshow x = ppshow x
instance MyShow Contact where
    myshow (h,p) = h ++ ":" ++ p

pMb Nothing = "NULL"
pMb (Just x) = myshow x

printAttrib attr = " : " ++ (printAType attr) ++ " = " ++ (printAVal attr)
printAVal (Aint x) = pMb x
printAVal (Astr x) = pMb x
printAVal (Atime x) = pMb x
printAVal (Abool x) = pMb x
printAVal (Aquery x) = pMb x
printAVal (Acontact x) = pMb x
printAVal (Aduration Nothing) = "NULL"
printAVal (Aduration (Just x)) = 
    printf "%c%d %02d:%02d:%02d.%03d" sgn days hours minutes secs msecs
    where
    (sgn, absX) = case x >= 0 of
        True -> ('+', x)
        False -> ('-', -x)
    msecs = absX `mod` 1000
    ms_x = absX `div` 1000
    secs = ms_x `mod` 60
    s_x = ms_x `div`60
    minutes = s_x `mod` 60
    min_x = s_x `div` 60
    hours = min_x `mod` 24
    h_x = min_x `div` 24
    days = h_x
printAVal (Afloat x) = pMb x
printAVal (Aset _ (Just xs)) = "{ " ++
    (", " `intercalate` (map printAVal xs)) ++ " }"
printAVal (Alist _ (Just xs)) = "[ " ++
    (", " `intercalate` (map printAVal xs)) ++ " ]"
printAVal _ = "NULL"

dPrecDiff = 1000000
dPrecision = 1000.0


printAType (Aint _) = "integer"
printAType (Astr _) = "string"
printAType (Atime _) = "time"
printAType (Aset i xs) = printCType "set" i xs
printAType (Alist i xs) = printCType "list" i xs
printAType (Abool _) = "boolean"
printAType (Aquery _) = "query"
printAType (Acontact _) = "contact"
printAType (Aduration _) = "duration"
printAType (Afloat _) = "double"
printCType kw i xs = kw ++ " of " ++ (case i of
    0 -> ""
    _ -> (show i) ++ " ") ++
    (case xs of
        Just (x:_) -> printAType x
        _ -> "*")
