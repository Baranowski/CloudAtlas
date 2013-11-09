module Zones where

import qualified Data.Map as M
import QAT
import Data.Time.Clock
import Data.Time.Format
import System.Locale


data Zone = Zone (M.Map String Attribute) [Zone]
    deriving (Show, Eq)

data Attribute
    = Aint (Maybe Int)
    | Astr (Maybe String)
    | Atime (Maybe UTCTime)
    | Aset Int Type (Maybe [Attribute])
    | Alist Int Type (Maybe [Attribute])
    | Atuple Int (Maybe [Attribute])
    | Abool (Maybe Bool)
    | Aquery (Maybe QAT)
    | Acontact (Maybe String)
    | Aduration (Maybe String) -- TODO
    | Afloat (Maybe Double)
    deriving (Show, Eq)
data Type
    = Tint
    | Tstr
    | Ttime
    | Tset Type
    | Tlist Type
    | Ttuple [Type]
    | Tbool
    | Tquery
    | Tcontact
    | Tduration
    deriving (Show, Eq)

time_format = "%Y/%m/%d %H:%M:%S%Q"
timeFromStr s = readTime defaultTimeLocale time_format s

durFromStr s = (Just s)
