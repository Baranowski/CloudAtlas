{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}
module Communication where

import Data.List.Split
import Data.Word
import Data.Bits
import Data.Char
import Network.Socket
import Control.Monad.Error
import Data.Either

bToStr = map wToCh
wToCh :: Word8 -> Char
wToCh w = chr (fromIntegral w)

strToB = map chToW
chToW :: Char -> Word8
chToW ch = fromIntegral $ ord ch


class Serializable a where
    serialize :: a -> [Word8]

newtype Header = Header {hd_port :: PortNumber}

updateClient (SockAddrInet p host) hd =
    return $ SockAddrInet (hd_port hd) host
updateClient (SockAddrInet6 p flow host sc) hd =
    return $ SockAddrInet6 (hd_port hd) flow host sc
updateClient _ _ = 
    fail "updateClient: Unsupported address"

readHeader msg = do
    (p::Int, newMsg) <- desrInt msg
    return (Header (fromIntegral p), newMsg)
    
data Msg
    = FreshnessInit Freshness
    | FreshnessResponse Freshness
    | RmiReq RemoteCall

instance Serializable Msg where
    serialize (FreshnessInit fr) = 1:(serialize fr)
    serialize (FreshnessResponse fr) = 2:(serialize fr)
    serialize (RmiReq r) = 3:(serialize r)

desrMsg ::  [Word8] -> Either String Msg
desrMsg (1:xs) = do
    fr <- desrFreshness xs
    return $ FreshnessInit fr
desrMsg (2:xs) = do
    fr <- desrFreshness xs
    return $ FreshnessResponse fr
desrMsg (3:xs) = do
    rmi <- desrRmi xs
    return $ RmiReq rmi
desrMsg _ = fail "desrMsg: Unknown message type"
deserializeMsg = desrMsg

newtype Freshness = Freshness [(String, Int)]
instance Serializable Freshness where
    serialize (Freshness l) = (fromIntegral $ length l):(concatMap srSingle l)
      where
      srSingle (name, timestamp) = (serialize name) ++ (serialize timestamp)
desrFreshness (count:xs) = go count [] xs
  where
    go 0 acc [] = return $ Freshness $ reverse acc
    go c acc xs = do
        (name, remaining) <- desrStr xs
        (timestamp, remaining) <- desrInt remaining
        go (c-1) ((name, timestamp):acc) remaining

instance Serializable String where
    serialize s = (fromIntegral $ length s):(strToB s)
desrStr (count:xs) = do
    let c = fromIntegral count
    return $ (bToStr (take c xs), drop c xs)

instance Serializable Int where
    serialize x = go [] x 0
      where
        go acc 0 cells = cells:acc
        go acc x cells = go ((fromIntegral x):acc) (x `shiftR` 8) (cells+1) -- TODO: liczby ujemne
desrInt (count:xs) = go (fromIntegral count) 0 xs
  where
    go 0 acc xs = return (acc, xs)
    go c acc (x:xs) =
        go (c-1)
           ((acc `shiftL` 8) + (fromIntegral x))
           xs

data RemoteCall = RC -- TODO
instance Serializable RemoteCall where
    serialize _ = return 1
desrRmi xs = return RC
