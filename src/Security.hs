{-# LANGUAGE FlexibleInstances, RecordWildCards, ScopedTypeVariables #-}
module Security where

import Data.Word
import Data.Time.Clock
import qualified Codec.Crypto.RSA as R
import Crypto.Random
import qualified Data.ByteString.Lazy as L
import Control.Monad.Error
import Control.Monad.Reader

import Concurrency
import ServerConfig
import Zones
import Communication
import SecData
import Utils

verifyMsg :: Msg -> ReaderT Env (ErrorT String IO) ()
verifyMsg z@(ZInfo _ p zi) = do
    z <- embedSTM $ getByPath_stm $ parentOf p 
    pk <- case z_kca z of
        Just pk -> return pk
        _ -> fail "CA public key unknown"
    verify pk $ zi_zc zi
    verify (zc_pubkey $ zi_zc zi) (p, zi)

verifyMsg _ = return ()

generateKeys :: IO (PrivKey,PubKey)
generateKeys = do
    g::SystemRandom <- newGenIO
    let (pub,priv,_) = R.generateKeyPair g 1024
    return (priv,pub)

signZMI :: PrivKey -> String -> UTCTime -> ZoneAttrs -> Certificate
signZMI pk issuer t attrs = Certificate{..}
    where
    ct_id = issuer ++ "_" ++ (show tstamp)
    ct_sig = L.unpack $ R.sign pk (L.pack serialized)
    ct_create = tstamp
    tstamp = timeToTimestamp t
    serialized = (serialize attrs)
              ++ (serialize ct_id)
              ++ (serialize ct_create)

signZone :: String -> UTCTime -> PrivKey -> Int -> String -> PubKey -> ZoneCert
signZone issuer t pk zc_level zc_name zc_pubkey = ZoneCert{..}
    where
    zc_cert = Certificate{..}
    ct_id = issuer ++ "_" ++ (show ct_create)
    ct_create = timeToTimestamp t
    ct_sig = L.unpack $ R.sign pk (L.pack serialized)
    serialized = serialize (ct_id, ct_create)
              ++ serialize (zc_level, zc_name, zc_pubkey)

class Hashable a where
    mkserial :: a -> [Word8]
    getsig   :: a -> Signature
    verify   :: PubKey -> a -> ReaderT Env (ErrorT String IO) ()
    verify pk x = do
        case R.verify pk (L.pack $ mkserial x) (L.pack $ getsig x) of
            False -> fail "Signature does not match"
            True -> return ()

instance Hashable Certificate where
    mkserial c = serialize (ct_id c, ct_create c)
    getsig     = ct_sig

instance Hashable ZoneCert where
    mkserial zc = (mkserial $ zc_cert zc)
               ++ (serialize (zc_level zc
                             , zc_name zc
                             , zc_pubkey zc
                             ))
    getsig = getsig . zc_cert

instance Hashable ZoneInfo where
    mkserial zi = (serialize $ zi_attrs zi)
               ++ (mkserial $ zi_cert zi)
    getsig = getsig . zi_cert

instance Hashable (String, ZoneInfo) where
    mkserial (s, zi) = (serialize s) ++ (mkserial zi)
    getsig (_, zi) = getsig zi
