module ServerConfig where

import Control.Concurrent.STM
import qualified Control.Monad as Mo
import Control.Monad.Error
import Data.ConfigFile as C
import Data.List.Split
import Network.Socket hiding (recvFrom, sendTo)

import Zones

debug = True

data GossipStrategy = RoundRobin | ExpRR | Random | ExpRandom

data Config = Config { c_port :: PortNumber
                     , c_host :: String
                     , c_path :: [String]
                     , c_g_freq :: Int
                     , c_g_strategy :: GossipStrategy
                     , c_qu_fr :: Int
                     }

data Env = Env { e_zones :: TVar Zone
               , e_contacts :: TVar [Contact]
               , e_conf :: Config
               }

readConfig path = do
    runErrorT $ do
        cp <- Mo.join $ liftIO $ C.readfile C.emptyCP path
        port <- C.get cp "" "port"
        host <- C.get cp "" "host"
        path <- C.get cp "" "zone"
        gfreq <- C.get cp "" "gossip_frequency"
        strt <- C.get cp "" "gossip_strategy"
        qufr <- C.get cp "" "query_frequency"
        strategy <- case strt of
            "round-robin" -> return RoundRobin
            "exp-round-robin" -> return ExpRR
            "random" -> return Random
            "exp-random" -> return ExpRandom
            _ -> fail $ "Unrecognized gossip strategy: " ++ strt
        return $ Config { c_port = fromIntegral $ read port
                        , c_host = host
                        , c_path = splitOn "/" path
                        , c_g_freq = (read gfreq) * 1000 * 1000
                        , c_g_strategy = strategy
                        , c_qu_fr = (read qufr) * 1000 * 1000
                        }
