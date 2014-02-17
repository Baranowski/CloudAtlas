module SecData where

import Data.Word
import qualified Codec.Crypto.RSA as R

import QAT
import Attributes

type Signature = [Word8]
type PubKey = R.PublicKey
type PrivKey = R.PrivateKey

data Certificate = Certificate { ct_id     :: String
                               , ct_create :: Integer
                               , ct_sig    :: Signature
                               }
                               deriving(Show,Read)

data ZoneCert = ZoneCert { zc_level  :: Int
                         , zc_name   :: String
                         , zc_pubkey :: PubKey
                         , zc_cert   :: Certificate
                         }
                         deriving(Show,Read)

data ClientCert = ClientCert { cc_author  :: String
                             , cc_pubkey  :: PubKey
                             , cc_zones   :: [[String]]
                             , cc_attrs   :: [String]
                             , cc_cert    :: Certificate
                             }
                             deriving (Show,Read)

data QueryCert = QueryCert { qc_code   :: QAT
                           , qc_name   :: String
                           , qc_minL   :: Int
                           , qc_maxL   :: Int
                           , qc_cert   :: Certificate
                           , qc_cc     :: ClientCert
                           }
                           deriving (Show,Read)

data FeedCert = FeedCert { fc_attrs   :: [(String, Attribute)]
                         , fc_cert     :: Certificate
                         , fc_cc       :: ClientCert
                         }
                         deriving (Show,Read)
