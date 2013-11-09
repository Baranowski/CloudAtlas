module Main where

import Parser
import System.IO
import System.Exit
import qualified Data.Map as M

import Hardcoded
import Zones
import QAT

panic msg = do
    hPutStrLn stderr msg
    exitFailure

myself = ["uw", "violet07"]

installQueries qList (Zone attribs children) myself@(h:x:xs) = 
    Zone newAttribs newChildren
    where
    newAttribs = attribs `M.union` (M.fromList (queriesToAttribs qList))
    newChildren = map (installInChosen qList myself) children
    queriesToAttribs :: [(String, QAT)] -> [(String, Attribute)]
    queriesToAttribs = map (\(name, qat) -> (name, Aquery (Just qat)))
    installInChosen qList myself@(h:x:xs) z@(Zone attribs _) =
        if (M.lookup "name" attribs)==(Just (Astr (Just h)))
            then installQueries qList z (x:xs)
            else z

installQueries qList zones _ = zones

installAndPerform qList zones myself =
    performQueries (installQueries qList zones myself)

performQueries = id
printAttribs = show

main = do
    qText <- getContents
    qList <- case parse qText of
        Left err -> panic (show err) >> return []
        Right qList -> return qList
    let newZones = installAndPerform qList zones myself 
    putStrLn $ printAttribs newZones

