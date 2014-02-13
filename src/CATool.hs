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

unknown = do
    prog <- getProgName
    hPutStrLn stderr $ "Usage:"
    hPutStrLn stderr $ "    " ++ prog ++ " [database dir] --create-zone [zone name]"
    hPutStrLn stderr $ "    " ++ prog ++ " [database dir] --init"
    exitFailure

main = do
    args <- getArgs
    case args of
        [dbDir,"--create-zone",zone_name] -> createZone dbDir zone_name
        [dbDir,"--init"] -> initDB dbDir
        _ -> unknown

initDB dbDir = do
    (priv,pub) <- generateKeys
    createDirectoryIfMissing True dbDir
    writeFile (dbDir ++ "/ca.priv") (show priv)
    writeFile (dbDir ++ "/ca.pub") (show pub)

createZone dbDir zone_name = do
    let zpath = tail $ splitOn "/" zone_name 
    let level = length zpath
    let zoneDir = intercalate "/" (dbDir:zpath)
    createDirectoryIfMissing True zoneDir
    caPriv <- read <$> (readFile $ dbDir ++ "/ca.priv")
    (priv,pub) <- generateKeys
    writeFile (zoneDir ++ "/zone.priv") (show priv)
    t <- getCurrentTime
    let zc = signZone "CA" t caPriv level (last zpath) pub
    writeFile (zoneDir ++ "/zone.cert") (show zc)

