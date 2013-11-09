module Zones where

import qualified Data.Map as M
import QAT
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Data.Maybe


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

getAttr s (Zone attribs _) = M.lookup s attribs
getAttrS s z = fromJust $ getAttr s z

printAttribs zone = go [] zone
  where
  go path z@(Zone attribs children) =
    fullName ++ "\n" ++
        (concatMap printNamedAttrib (M.toList attribs)) ++ 
        (concatMap (go newPath) children)
    where
    newPath = path ++ (case getAttrS "name" z of
        Astr Nothing-> []
        Astr (Just x) -> [x])
    fullName = case newPath of
        [] -> "/"
        _ -> concatMap ("/" ++) newPath

printNamedAttrib (name, attr) = "    " ++ name ++ (printAttrib attr) ++ "\n"

printMb Nothing = "NULL"
printMb (Just x) = myPrint x

printAttrib attr = " : " ++ (printAType attr) ++ " = " ++ (printAVal attr)
printAVal _ = ""

printAType (Aint _) = "integer"
printAType (Astr _) = "string"
printAType (Atime _) = "time"
printAType (Aset i t _) = printCType "set" i t
printAType (Alist i t _) = printCType "list" i t
printAType (Atuple i Nothing) = "<< * " ++ (concatMap (\_->", * ") [2..i]) ++ ">>"
printAType (Atuple _ (Just xs)) = 
printAType (Abool _) = "boolean"
printAType (Aquery _) = "query"
printAType (Acontact _) = "contact"
printAType (Aduration _) = "duration"
printAType (Afloat _) = "double"

printCType kw i t = kw ++ " of " ++ (case i of
    0 -> ""
    _ -> (show i) ++ " ") ++ (printType t)
