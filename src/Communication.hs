{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}
module Communication where

import Data.List.Split
import Data.Time.Clock
import Data.Word
import Data.Bits
import Data.Char
import Network.Socket
import Control.Monad.Error
import Data.Functor.Identity
import Data.Either

import Zones
import QAT
import Parser

type CommMonad res = (ErrorT String Identity) res
generalizeId m = return (runIdentity m)

class Serializable a where
    serialize :: a -> [Word8]
    deserialize :: [Word8] -> CommMonad (a, [Word8])

instance Serializable a => Serializable [a] where
    serialize xs = (serialize $ length xs) ++ (concatMap serialize xs)
    deserialize xs = do
        (i::Int, rest) <- deserialize xs
        go i [] rest
        where
          go 0 acc rest = return (reverse acc, rest)
          go i acc rest = do
            (next::a, rest) <- deserialize rest
            go (i-1) (next:acc) rest

instance (Serializable a, Serializable b) => Serializable (a, b) where
    serialize (pA, pB) = (serialize pA) ++ (serialize pB)
    deserialize xs = do
        (pA, rest) <- deserialize xs
        (pB, rest) <- deserialize rest
        return ((pA, pB), rest)

instance Serializable a => Serializable (Maybe a) where
    serialize Nothing = [0]
    serialize (Just x) = [1] ++ (serialize x)
    deserialize (0:xs) = return (Nothing, xs)
    deserialize (1:xs) = do
        (res, xs) <- deserialize xs
        return (Just res, xs)
    deserialize _ = fail "Unrecognized Maybe serialization"

newtype Header = Header {hd_port :: PortNumber}

updateClient (SockAddrInet p host) hd =
    return $ SockAddrInet (hd_port hd) host
updateClient (SockAddrInet6 p flow host sc) hd =
    return $ SockAddrInet6 (hd_port hd) flow host sc
updateClient _ _ = 
    fail "updateClient: Unsupported address"

readHeader msg = do
    (p::Integer, newMsg) <- deserialize msg
    return (Header (fromIntegral p), newMsg)

addHeader port msg = (serialize ((fromIntegral port)::Integer)) ++ msg
    
data Msg
    = FreshnessInit Freshness
    | FreshnessResponse Freshness
    | RmiReq Int RemoteCall
    | ZInfo String [(String, Attribute)]
    | RmiResp Int RemoteReturn

instance Serializable Msg where
    serialize (FreshnessInit fr) = 1:(serialize fr)
    serialize (FreshnessResponse fr) = 2:(serialize fr)
    serialize (RmiReq i r) = 3:(serialize i) ++ (serialize r)
    serialize (ZInfo p l) = 4:(serialize p) ++ (serialize l)
    serialize (RmiResp i r) = 5:(serialize i) ++ (serialize r)

    deserialize (1:xs) = do
        (fr, rest) <- deserialize xs
        return (FreshnessInit fr, rest)
    deserialize (2:xs) = do
        (fr, rest) <- deserialize xs
        return (FreshnessResponse fr, rest)
    deserialize (3:xs) = do
        (i, xs) <- deserialize xs
        (rmi, xs) <- deserialize xs
        return (RmiReq i rmi, xs)
    deserialize (4:xs) = do
        (p::String, xs) <- deserialize xs
        (l::[(String, Attribute)], xs) <- deserialize xs
        return (ZInfo p l, xs)
    deserialize (5:xs) = do
        (i, xs) <- deserialize xs
        (r, xs) <- deserialize xs
        return (RmiResp i r, xs)
    deserialize _ = fail "Unrecognized Msg serialization"
deserializeMsg xs = do
    (res, xs) <- deserialize xs
    when (xs /= []) $ fail $ "Message longer than expected"
    return res

instance Serializable Attribute where
    serialize (Aint x) = [1] ++ (serialize x)
    serialize (Astr x) = [2] ++ (serialize x)
    serialize (Atime x) = [3] ++ (serialize x)
    serialize (Aset i l) = [4] ++ (serialize i) ++ (serialize l)
    serialize (Alist i l) = [5] ++ (serialize i) ++ (serialize l)
    serialize (Abool x) = [6] ++ (serialize x)
    serialize (Aquery x) = [7] ++ (serialize x)
    serialize (Acontact x) = [8] ++ (serialize x)
    serialize (Aduration x) = [9] ++ (serialize x)
    serialize (Afloat x) = [10] ++ (serialize x)

    deserialize (1:xs) = do
        (i::Maybe Int, xs) <- deserialize xs
        return (Aint i, xs)
    deserialize (2:xs) = do
        (s::Maybe String, xs) <- deserialize xs
        return (Astr s, xs)
    deserialize (3:xs) = do
        (t::Maybe UTCTime, xs) <- deserialize xs
        return (Atime t, xs)
    deserialize (4:xs) = do
        (i::Int, xs) <- deserialize xs
        (l::Maybe [Attribute], xs) <- deserialize xs
        return (Aset i l, xs)
    deserialize (5:xs) = do
        (i::Int, xs) <- deserialize xs
        (l::Maybe [Attribute], xs) <- deserialize xs
        return (Alist i l, xs)
    deserialize (6:xs) = do
        (b::Maybe Bool, xs) <- deserialize xs
        return (Abool b, xs)
    deserialize (7:xs) = do
        (q::Maybe QAT, xs) <- deserialize xs
        return (Aquery q, xs)
    deserialize (8:xs) = do
        (c::Maybe Contact, xs) <- deserialize xs
        return (Acontact c, xs)
    deserialize (9:xs) = do
        (d::Maybe Integer, xs) <- deserialize xs
        return (Aduration d, xs)
    deserialize (10:xs) = do
        (f::Maybe Double, xs) <- deserialize xs
        return (Afloat f, xs)

newtype Freshness = Freshness [(String, Integer)]
instance Serializable Freshness where
    serialize (Freshness l) = serialize l

    deserialize xs = do
        (l, xs) <- deserialize xs
        return (Freshness l, xs)

instance Serializable Integer where
    serialize x = if x >= 0
        then go [] x 0
        else go [] (-x) 128
      where
        go acc 0 cells = cells:acc
        go acc x cells = go ((fromIntegral x):acc) (x `shiftR` 8) (cells+1)

    deserialize (count:xs) = if count <= 128
        then go (fromIntegral count) 0 xs
        else do
            (res, xs) <- go (fromIntegral $ count-128) 0 xs
            return (-res, xs)
      where
        go 0 acc xs = return (acc, xs)
        go c acc (x:xs) =
            go (c-1)
               ((acc `shiftL` 8) + (fromIntegral x))
               xs

sInt i = serialize ((fromIntegral i)::Integer)
desInt xs = do
    (i::Integer, rest) <- deserialize xs
    return (fromIntegral i, rest)

instance Serializable Int where
    serialize = sInt
    deserialize = desInt
instance Serializable Word16 where
    serialize = sInt
    deserialize = desInt
instance Serializable Word32 where
    serialize = sInt
    deserialize = desInt

instance Serializable Char where
    serialize ch = [fromIntegral $ ord ch]
    deserialize (x:xs) =
        return (chr (fromIntegral x), xs)

instance Serializable Double where
    serialize d = (serialize i1) ++ (serialize i2)
      where
        (i1, i2) = decodeFloat d
    deserialize xs = do
        (i1::Integer, xs) <- deserialize xs
        (i2::Int, xs) <- deserialize xs
        return (encodeFloat i1 i2, xs)

instance Serializable UTCTime where
    serialize t = (serialize $ timeToTimestamp t)
    deserialize xs = do
        (i::Integer, xs) <- deserialize xs
        return (timestampToTime i, xs)

instance Serializable SockAddr where
    serialize (SockAddrInet (PortNum p) (h)) =
        1:(serialize p) ++ (serialize h)
    serialize (SockAddrInet6 (PortNum p) fi (h1,h2,h3,h4) si) =
        2:(serialize p) ++ (concatMap serialize [fi,h1,h2,h3,h4,si])

    deserialize (1:xs) = do
        (p::Word16, xs) <- deserialize xs
        (h::Word32, xs) <- deserialize xs
        return (SockAddrInet (PortNum p) h, xs)
    deserialize (2:xs) = do
        (p::Word16, xs) <- deserialize xs
        (fi::Word32, xs) <- deserialize xs
        (h1::Word32, xs) <- deserialize xs
        (h2::Word32, xs) <- deserialize xs
        (h3::Word32, xs) <- deserialize xs
        (h4::Word32, xs) <- deserialize xs
        (si::Word32, xs) <- deserialize xs
        return (SockAddrInet6 (PortNum p) fi (h1,h2,h3,h4) si, xs)

instance Serializable Bool where
    serialize True = [1]
    serialize False = [0]
    deserialize (1:xs) = return (True, xs)
    deserialize (0:xs) = return (False, xs)

instance Serializable QAT where
    serialize q = serialize $ show q
    deserialize xs = do
        (s::String, xs) <- deserialize xs
        q <- case (parseSingle s) of
            Left err -> fail $ show err
            Right x -> return x
        return (q, xs)

data RemoteCall
    = SetContacts [Contact]
    | GetBagOfZones
    | GetZoneAttrs String
    | SetZoneAttrs {sza_path:: String, sza_attrs::[(String, Attribute)]}

instance Serializable RemoteCall where
    serialize (SetContacts cs) = 1:(serialize cs)
    serialize (GetBagOfZones) = [2]
    serialize (GetZoneAttrs s) = 3:(serialize s)
    serialize (SetZoneAttrs p l) = 4:(serialize p) ++ (serialize l)

    deserialize (1:xs) = do
        (l::[Contact], xs) <- deserialize xs
        return (SetContacts l, xs)
    deserialize (2:xs) = do
        return (GetBagOfZones, xs)
    deserialize (3:xs) = do
        (p::String, xs) <- deserialize xs
        return (GetZoneAttrs p, xs)
    deserialize (4:xs) = do
        (p::String, xs) <- deserialize xs
        (l, xs) <- deserialize xs
        return (SetZoneAttrs p l, xs)

data RemoteReturn
    = RmiOk
    | RmiZoneInfo [(String, Attribute)]
    | RmiBagOfZones [String]

instance Serializable RemoteReturn where
    serialize RmiOk = [1]
    serialize (RmiZoneInfo l) = 2:(serialize l)
    serialize (RmiBagOfZones l) = 3:(serialize l)

    deserialize (1:xs) = return (RmiOk, xs)
    deserialize (2:xs) = do
        (l, xs) <- deserialize xs
        return (RmiZoneInfo l, xs)
    deserialize (3:xs) = do
        (l, xs) <- deserialize xs
        return (RmiBagOfZones l, xs)
