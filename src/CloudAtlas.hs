module Main where

import qualified Parser
import System.IO
import System.Exit

panic msg = do
    hPutStrLn stderr msg
    exitFailure

main = do
    qText <- getContents
    qList <- case Parser.parse qText of
        Left err -> panic (show err) >> return []
        Right qList -> return qList
    putStrLn $ show qList