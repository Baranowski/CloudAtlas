module SecData where

import Data.Word
import qualified Codec.Crypto.RSA as R

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
