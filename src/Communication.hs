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
import qualified Data.Map as M

import Zones
import QAT
import Parser
import SecData
import Attributes

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
instance (Serializable a, Serializable b, Serializable c) => Serializable (a, b, c) where
    serialize (pA, pB, pC) = serialize (pA, (pB, pC))
    deserialize xs = do
        ((pA, (pB, pC)), rest) <- deserialize xs
        return ((pA, pB, pC), rest)
instance (Serializable a, Serializable b, Serializable c, Serializable d) => Serializable (a, b, c, d) where
    serialize (pA, pB, pC, pD) = serialize ((pA,pB),(pC,pD))
    deserialize xs = do
        (((pA,pB),(pC,pD)), rest) <- deserialize xs
        return ((pA, pB, pC, pD), rest)
instance (Serializable a, Serializable b, Serializable c, Serializable d, Serializable e) => Serializable (a, b, c, d, e) where
    serialize (pA, pB, pC, pD, pE) = serialize ((pA,pB),(pC,pD,pE))
    deserialize xs = do
        (((pA,pB),(pC,pD,pE)), rest) <- deserialize xs
        return ((pA, pB, pC, pD, pE), rest)

instance Serializable a => Serializable (Maybe a) where
    serialize Nothing = [0]
    serialize (Just x) = [1] ++ (serialize x)
    deserialize (0:xs) = return (Nothing, xs)
    deserialize (1:xs) = do
        (res, xs) <- deserialize xs
        return (Just res, xs)
    deserialize _ = fail "Unrecognized Maybe serialization"

instance Serializable ZoneAttrs where
    serialize = serialize
              . M.toAscList
              . (M.filter $ not . (sameType $ Aquery Nothing))
    deserialize xs = do
        (l, xs) <- deserialize xs
        return (M.fromList l, xs)

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
    = FreshnessPre Integer
    | FreshnessInit TimeInfo Freshness
    | FreshnessResponse TimeInfo Freshness
    | RmiReq Int RemoteCall
    | ZInfo TimeInfo String ZoneInfo
    | RmiResp Int RemoteReturn
    deriving (Show)

type TimeInfo = (Integer,Integer,Integer)

instance Serializable Msg where
    serialize (FreshnessInit t fr) = 1:(serialize (t,fr))
    serialize (FreshnessResponse t fr) = 2:(serialize (t,fr))
    serialize (RmiReq i r) = 3:(serialize i) ++ (serialize r)
    serialize (ZInfo t p l) = 4:(serialize (t,p,l))
    serialize (RmiResp i r) = 5:(serialize i) ++ (serialize r)
    serialize (FreshnessPre t) = 6:(serialize t)

    deserialize (1:xs) = do
        ((t,fr), rest) <- deserialize xs
        return (FreshnessInit t fr, rest)
    deserialize (2:xs) = do
        ((t,fr), rest) <- deserialize xs
        return (FreshnessResponse t fr, rest)
    deserialize (3:xs) = do
        (i, xs) <- deserialize xs
        (rmi, xs) <- deserialize xs
        return (RmiReq i rmi, xs)
    deserialize (4:xs) = do
        ((t,p,l), xs) <- deserialize xs
        return (ZInfo t p l, xs)
    deserialize (5:xs) = do
        (i, xs) <- deserialize xs
        (r, xs) <- deserialize xs
        return (RmiResp i r, xs)
    deserialize (6:xs) = do
        (t, xs) <- deserialize xs
        return (FreshnessPre t, xs)
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
        (i, xs) <- deserialize xs
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
    deriving (Show)
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

instance Serializable Word8 where
    serialize x = [x]
    deserialize (x:xs) = return (x, xs)

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
    serialize q = serialize $ ppshow q
    deserialize xs = do
        (s::String, xs) <- deserialize xs
        q <- case (parseSingle s) of
            Left err -> fail $ show err
            Right x -> return x
        return (q, xs)

instance Serializable ZoneInfo where
    serialize (ZoneInfo za cert zc qc) = serialize (za, cert, zc, qc)
    deserialize xs = do
        ((za, cert, zc, qc), xs) <- deserialize xs
        return (ZoneInfo za cert zc qc, xs)

instance Serializable Certificate where
    serialize (Certificate id cr sig) = serialize (id, cr, sig)
    deserialize xs = do
        ((id, cr, sig), xs) <- deserialize xs
        return (Certificate id cr sig, xs)

instance Serializable ZoneCert where
    serialize (ZoneCert lev nm pk cert) = serialize (lev, nm, pk, cert)
    deserialize xs = do
        ((lev, nm, pk, cert), xs) <- deserialize xs
        return (ZoneCert lev nm pk cert, xs)

instance Serializable FeedCert where
    serialize (FeedCert as cert cc) = serialize (as,cert,cc)
    deserialize xs = do
        ((as,cert,cc), xs) <- deserialize xs
        return (FeedCert as cert cc, xs)

instance Serializable ClientCert where
    serialize (ClientCert au pk ct zs as) = serialize (au,pk,ct,zs,as)
    deserialize xs = do
        ((au,pk,ct,zs,as), xs) <- deserialize xs
        return (ClientCert au pk ct zs as, xs)

instance Serializable QueryCert where
    serialize (QueryCert c n mi ma ce cc) = serialize ((c,n,mi),(ma,ce,cc))
    deserialize xs = do
        (((c,n,mi),(ma,ce,cc)), xs) <- deserialize xs
        return (QueryCert c n mi ma ce cc, xs)

instance Serializable PubKey where
    serialize = serialize . show
    deserialize xs = do
        (s, xs) <- deserialize xs
        return (read s, xs)

instance Serializable PrivKey where
    serialize = serialize . show
    deserialize xs = do
        (s, xs) <- deserialize xs
        return (read s, xs)

data RemoteCall
    = SetContacts [Contact]
    | GetBagOfZones
    | GetZoneAttrs String
    | SetZoneAttrs FeedCert
    | InstallQuery QueryCert
    | UninstallQuery {uq_path:: String, uq_name::String}
    deriving (Show)

instance Serializable RemoteCall where
    serialize (SetContacts cs) = 1:(serialize cs)
    serialize (GetBagOfZones) = [2]
    serialize (GetZoneAttrs s) = 3:(serialize s)
    serialize (SetZoneAttrs l) = 4:(serialize l)
    serialize (InstallQuery c) = 5:(serialize c)
    serialize (UninstallQuery p n) = 6:(serialize (p,n))

    deserialize (1:xs) = do
        (l, xs) <- deserialize xs
        return (SetContacts l, xs)
    deserialize (2:xs) = do
        return (GetBagOfZones, xs)
    deserialize (3:xs) = do
        (p::String, xs) <- deserialize xs
        return (GetZoneAttrs p, xs)
    deserialize (4:xs) = do
        (l, xs) <- deserialize xs
        return (SetZoneAttrs l, xs)
    deserialize (5:xs) = do
        (c, xs) <- deserialize xs
        return (InstallQuery c, xs)
    deserialize (6:xs) = do
        ((p,n), xs) <- deserialize xs
        return (UninstallQuery p n, xs)

data RemoteReturn
    = RmiOk
    | RmiZoneInfo [(String, Attribute)]
    | RmiBagOfZones [String]
    | RmiErr String
    deriving (Show)

instance Serializable RemoteReturn where
    serialize RmiOk = [1]
    serialize (RmiZoneInfo l) = 2:(serialize l)
    serialize (RmiBagOfZones l) = 3:(serialize l)
    serialize (RmiErr e) = 4:(serialize e)

    deserialize (1:xs) = return (RmiOk, xs)
    deserialize (2:xs) = do
        (l, xs) <- deserialize xs
        return (RmiZoneInfo l, xs)
    deserialize (3:xs) = do
        (l, xs) <- deserialize xs
        return (RmiBagOfZones l, xs)
    deserialize (4:xs) = do
        (e, xs) <- deserialize xs
        return (RmiErr e, xs)
