{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}
module Communication where

import Data.List.Split
import Data.Word
import Data.Bits
import Data.Char
import Network.Socket
import Control.Monad.Error
import Data.Functor.Identity
import Data.Either

import Zones

bToStr = map wToCh
wToCh :: Word8 -> Char
wToCh w = chr (fromIntegral w)

strToB = map chToW
chToW :: Char -> Word8
chToW ch = fromIntegral $ ord ch


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
    
data Msg
    = FreshnessInit Freshness
    | FreshnessResponse Freshness
    | RmiReq RemoteCall
    | ZInfo String [(String, Attribute)]

instance Serializable Msg where
    serialize (FreshnessInit fr) = 1:(serialize fr)
    serialize (FreshnessResponse fr) = 2:(serialize fr)
    serialize (RmiReq r) = 3:(serialize r)
    serialize (ZInfo p l) = 4:(serialize p) ++ (sAttrs)
      where
        sAttrs = (serialize $ length l) ++ (concatMap serAttr l)
        serAttr (n, a) = (serialize n) ++ (serialize a)

    deserialize (1:xs) = do
        (fr, rest) <- deserialize xs
        return (FreshnessInit fr, rest)
    deserialize (2:xs) = do
        (fr, rest) <- deserialize xs
        return (FreshnessResponse fr, rest)
    deserialize (3:xs) = do
        (rmi, rest) <- desrRmi xs
        return (RmiReq rmi, rest)
    deserialize _ = fail "deserialize: Unknown message type"
deserializeMsg = deserialize :: [Word8] -> CommMonad (Msg, [Word8])

instance Serializable Attribute where

    serialize a = [1] -- TODO

newtype Freshness = Freshness [(String, Integer)]
instance Serializable Freshness where
    serialize (Freshness l) = serialize l

    deserialize xs = do
        (l, xs) <- deserialize xs
        return (Freshness l, xs)

instance Serializable Integer where
    serialize x = go [] x 0
      where
        go acc 0 cells = cells:acc
        go acc x cells = go ((fromIntegral x):acc) (x `shiftR` 8) (cells+1) -- TODO: liczby ujemne

    deserialize (count:xs) = go (fromIntegral count) 0 xs
      where
        go 0 acc xs = return (acc, xs)
        go c acc (x:xs) =
            go (c-1)
               ((acc `shiftL` 8) + (fromIntegral x))
               xs

instance Serializable Int where
    serialize i = serialize ((fromIntegral i)::Integer)
    deserialize xs = do
        (i::Integer, rest) <- deserialize xs
        return (fromIntegral i, rest)

instance Serializable Char where
    serialize ch = [fromIntegral $ ord ch]
    deserialize (x:xs) =
        return (chr (fromIntegral x), xs)
    deserialize _ = fail "Trying to deserialize empty input"

data RemoteCall = RC -- TODO
instance Serializable RemoteCall where
    serialize _ = return 1
desrRmi xs = return (RC, [])
