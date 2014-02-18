{-# LANGUAGE FlexibleInstances, RecordWildCards, ScopedTypeVariables #-}
module Security where

import Data.Word
import Data.Time.Clock
import qualified Codec.Crypto.RSA as R
import Crypto.Random
import qualified Data.ByteString.Lazy as L
import Control.Monad.Error
import Control.Monad.Reader
import Data.List.Split
import Data.List

import Concurrency
import ServerConfig
import Zones
import Communication
import SecData
import Utils
import Attributes
import QAT

verifyMsg :: Msg -> ReaderT Env (ErrorT String IO) ()
verifyMsg (ZInfo _ p zi) = do
    pk <- getCaKey (parentOf p)
    zc <- case zi_zc zi of
        Just x -> return x
        _ -> fail "Received ZMI without Zone Certificate"
    (verify pk zc)
        `addTrace`
        "Verifying ZoneCert"
    when ((zc_name zc) /= (last $ pathStoL p))
         (fail $ "Zone name does not match Zone Certificate")
    (verify (zc_pubkey $ zc) (p, zi))
        `addTrace`
        "Verifying ZMI Certificate"
  `addTrace`
  ("Verifying ZInfo for " ++ p)
verifyMsg (RmiReq _ (SetZoneAttrs fc)) = do
    verifyCC $ fc_cc fc
    verify (cc_pubkey $ fc_cc fc) fc
    let allowedAttrs = cc_attrs $ fc_cc fc
    when (not $ null allowedAttrs)
         (mapM_ (attrIn allowedAttrs) (fc_attrs fc))
    where
    attrIn allowedAttrs (n,_) =
        when (not $ n `elem` allowedAttrs)
             (fail $ "Trying to update protected field " ++ n)
verifyMsg (RmiReq _ (InstallQuery qc)) = verifyQC qc
verifyMsg _ = return ()

verifyQC qc = do
    verifyCC $ qc_cc qc
    verify (cc_pubkey $ qc_cc qc) qc
    verifyAttrs
    where
    verifyAttrs = do
        let (QAT sels _ _ ) = qc_code qc
        let attrs = map (\(Qsel _ n) -> n) sels
        when (not $ null $ cc_attrs $ qc_cc qc)
             (forM_ attrs
                    (withinAllowed $ cc_attrs $ qc_cc qc))
    withinAllowed allowed n = do
        when (not $ n `elem` allowed)
             (fail $ "Query would update protected field: " ++ n)
verifyCC cc = do
    let authPath = pathStoL (cc_author cc)
    myPath <- asks $ c_path . e_conf
    when (not $ authPath `isPrefixOf` myPath)
         (fail $ "Client Certificate signed by an unknown CA")
    when ( (not $ null $ cc_zones cc)
        && (not $ myPath `elem` (cc_zones cc)) )
         (fail $ "This zone is protected")
    pk <- getCaKey $ cc_author cc
    verify pk cc

getCaKey path = do
    z <- embedSTM $ getByPath_stm path
    case z_kca z of
        Just pk -> return pk
        _ -> fail "CA public key unknown"

generateKeys :: IO (PrivKey,PubKey)
generateKeys = do
    g::SystemRandom <- newGenIO
    let (pub,priv,_) = R.generateKeyPair g 1024
    return (priv,pub)

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

instance Hashable ClientCert where
    mkserial cc = (serialize (cc_author cc, cc_pubkey cc, cc_zones cc, cc_attrs cc)) ++ (mkserial $ cc_cert cc)
    getsig = getsig . cc_cert

signCC :: PrivKey -> String -> PubKey -> [[String]] -> [String] -> UTCTime -> Certificate
signCC priv auth pub zones attrs t = Certificate{..}
    where
    ct_id = auth ++ "_CA_" ++ (show ct_create)
    ct_create = timeToTimestamp t
    ct_sig = L.unpack $ R.sign priv (L.pack serialized)
    serialized = serialize ( (auth, pub, zones, attrs)
                           , (ct_id, ct_create)
                           )

instance Hashable QueryCert where
    mkserial qc = (serialize (qc_code qc, qc_name qc, qc_minL qc, qc_maxL qc)) ++ (mkserial $ qc_cert qc)
    getsig = getsig . qc_cert

signQC :: PrivKey -> QAT -> String -> Int -> Int -> UTCTime -> Certificate
signQC priv q n minL maxL t = Certificate{..}
    where
    ct_id = "Client_" ++ (show ct_create)
    ct_create = timeToTimestamp t
    ct_sig = L.unpack $ R.sign priv (L.pack serialized)
    serialized = serialize ( (q, n, minL, maxL)
                           , (ct_id, ct_create)
                           )

instance Hashable FeedCert where
    mkserial fc = (serialize $ fc_attrs fc) ++ (mkserial $ fc_cert fc)
    getsig = getsig . fc_cert

signFC :: PrivKey -> [(String,Attribute)] -> UTCTime -> Certificate
signFC priv attrs t = Certificate{..}
    where
    ct_id = "Client_" ++ (show ct_create)
    ct_create = timeToTimestamp t
    ct_sig = L.unpack $ R.sign priv (L.pack serialized)
    serialized = serialize (attrs, ct_id, ct_create)

instance Hashable ZoneCert where
    mkserial zc = (mkserial $ zc_cert zc)
               ++ (serialize (zc_level zc
                             , zc_name zc
                             , zc_pubkey zc
                             ))
    getsig = getsig . zc_cert

signZone :: String -> UTCTime -> PrivKey -> Int -> String -> PubKey -> ZoneCert
signZone issuer t pk zc_level zc_name zc_pubkey = ZoneCert{..}
    where
    zc_cert = Certificate{..}
    ct_id = issuer ++ "_" ++ (show ct_create)
    ct_create = timeToTimestamp t
    ct_sig = L.unpack $ R.sign pk (L.pack serialized)
    serialized = serialize (ct_id, ct_create)
              ++ serialize (zc_level, zc_name, zc_pubkey)

instance Hashable ZoneInfo where
    mkserial zi = (serialize $ zi_attrs zi)
               ++ (mkserial $ zi_cert zi)
    getsig = getsig . zi_cert

instance Hashable (String, ZoneInfo) where
    mkserial (s, zi) = (serialize s) ++ (mkserial zi)
    getsig (_, zi) = getsig zi

signZMI :: PrivKey -> String -> UTCTime -> ZoneAttrs -> String -> Certificate
signZMI pk issuer t attrs path = 
    Certificate{..}
    where
    ct_id = issuer ++ "_" ++ (show tstamp)
    ct_sig = L.unpack $ R.sign pk (L.pack serialized)
    ct_create = tstamp
    tstamp = timeToTimestamp t
    serialized = (serialize path)
              ++ (serialize attrs)
              ++ (serialize ct_id)
              ++ (serialize ct_create)
        `myTrace`
        ("signZMI " ++ path ++ " " ++ (show attrs))

instance Hashable a => Hashable (Maybe a) where
    mkserial (Just x) = mkserial x
    mkserial Nothing = []
    getsig (Just x) = getsig x
    getsig Nothing = []
