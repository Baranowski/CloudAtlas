{-# LANGUAGE RecordWildCards #-}
module Main where

import System.IO
import System.Exit
import System.Environment
import System.Directory
import Control.Applicative

import Data.List.Split
import Data.List
import Data.Time.Clock

import Security
import SecData

unknown = do
    prog <- getProgName
    hPutStrLn stderr $ "Usage:"
    hPutStrLn stderr $ "    " ++ prog ++ " [database dir] --create-zone [zone name]"
    hPutStrLn stderr $ "    " ++ prog ++ " [database dir] --init [prefix]"
    hPutStrLn stderr $ "    " ++ prog ++ " [database dir] --client-cert [allowed zones] [allowed attributes]"
    exitFailure

main = do
    args <- getArgs
    case args of
        [dbDir,"--create-zone",zone_name] -> createZone dbDir zone_name
        [dbDir,"--init",prefix] -> initDB dbDir prefix
        [dbDir,"--client-cert",allowed_zones,allowed_attrs] -> clientCert dbDir allowed_zones allowed_attrs
        _ -> unknown

clientCert dbDir zsS asS = do
    (priv,cc_pubkey) <- generateKeys
    prefix <- readFile $ dbDir ++ "/prefix"
    let [zs,as] = map (splitOn ",") [zsS,asS]
    let cc_zones = map (splitOn "/") zs
    let cc_attrs = as
    let cc_author = case prefix of
                        "" -> "/"
                        x -> x
    caPriv <- read <$> (readFile $ dbDir ++ "/ca.priv")
    t <- getCurrentTime
    let cc_cert = signCC caPriv cc_author cc_pubkey cc_zones cc_attrs t
    let cert = ClientCert{..}
    let fileRoot = "client_" ++ (show $ ct_create cc_cert)
    writeFile (fileRoot ++ ".priv") (show priv)
    writeFile (fileRoot ++ ".cert") (show cert)

initDB dbDir prefix = do
    (priv,pub) <- generateKeys
    createDirectoryIfMissing True dbDir
    writeFile (dbDir ++ "/ca.priv") (show priv)
    writeFile (dbDir ++ "/ca.pub") (show pub)
    writeFile (dbDir ++ "/prefix") prefix

createZone dbDir zone_name = do
    let zpath = tail $ splitOn "/" zone_name 
    let level = length zpath
    let zoneDir = intercalate "/" (dbDir:zpath)
    createDirectoryIfMissing True zoneDir
    caPriv <- read <$> (readFile $ dbDir ++ "/ca.priv")
    prefix <- readFile $ dbDir ++ "/prefix"
    (priv,pub) <- generateKeys
    writeFile (zoneDir ++ "/zone.priv") (show priv)
    t <- getCurrentTime
    let zc = signZone (prefix ++ "_CA") t caPriv level (last zpath) pub
    writeFile (zoneDir ++ "/zone.cert") (show zc)

