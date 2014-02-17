{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Zones where

import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Clock.POSIX
import System.Locale
import Data.Maybe
import Data.List
import Text.Printf
import Text.Parsec.String
import Control.Concurrent.STM

import QAT
import SecData
import Attributes

type ZoneAttrs = M.Map String Attribute
data ZoneInfo = ZoneInfo { zi_attrs :: ZoneAttrs
                         , zi_cert  :: Maybe Certificate
                         , zi_zc    :: Maybe ZoneCert
                         , zi_qcs   :: [QueryCert]
                         }
                         deriving (Show,Read)

data Zone = Zone
    { z_info :: TVar ZoneInfo
    , z_kca  :: Maybe PubKey
    , z_kpriv:: Maybe PrivKey
    , z_kids :: [Zone]
    }
    deriving (Eq)

data ZoneS = ZoneS
    { zs_attrs :: ZoneAttrs
    , zs_kids :: [ZoneS]
    , zs_qs :: [QAT]
    }
    deriving (Show, Eq)

{-
zoneStoTvar z = go z
    where
    go z = do
        newKids <- mapM zoneStoTvar (zs_kids z)
        newAttrs <- newTVar (zs_attrs z)
        return Zone{z_attrs=newAttrs, z_kids=newKids}
        -}

zoneTvarToS z = go z
    where
    go z = do
        newKids <- mapM go (z_kids z)
        newInfo <- readTVar (z_info z)
        return ZoneS{ zs_attrs=zi_attrs newInfo
                    , zs_kids=newKids
                    , zs_qs=map qc_code (zi_qcs newInfo)
                    }


getAttr s (ZoneS attribs _ _) = M.lookup s attribs
getAttrS s z = fromJust $ getAttr s z

printAttribs zone = go [] zone --TODO queries
  where
  go path z@(ZoneS attribs children qs) =
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
