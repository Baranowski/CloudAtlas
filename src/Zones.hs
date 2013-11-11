{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
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
    | Aset Int (Maybe [Attribute])
    | Alist Int (Maybe [Attribute])
    | Abool (Maybe Bool)
    | Aquery (Maybe QAT)
    | Acontact (Maybe String)
    | Aduration (Maybe String) -- TODO
    | Afloat (Maybe Double)
    deriving (Show, Eq)
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

class MyShow a where
    myshow :: a -> String
instance MyShow Int where
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
    myshow x = show x -- TODO

pMb Nothing = "NULL"
pMb (Just x) = myshow x

printAttrib attr = " : " ++ (printAType attr) ++ " = " ++ (printAVal attr)
printAVal (Aint x) = pMb x
printAVal (Astr x) = pMb x
printAVal (Atime x) = pMb x
printAVal (Abool x) = pMb x
printAVal (Aquery x) = pMb x
printAVal (Acontact x) = pMb x
printAVal (Aduration x) = pMb x
printAVal (Afloat x) = pMb x
--TODO nadmiarowy przecinek
printAVal (Aset _ (Just xs)) = "{" ++
    (concatMap ((++", ") . printAVal) xs) ++ "}"
printAVal (Alist _ (Just xs)) = "[" ++
    (concatMap ((++", ") . printAVal) xs) ++ "]"
printAVal _ = "NULL"

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
printCType kw i xs= kw ++ " of " ++ (case i of
    0 -> ""
    _ -> (show i) ++ " ") ++
    (case xs of
        Just (x:_) -> printAType x
        _ -> "*")
