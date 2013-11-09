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

installQueries qList z@(Zone attribs children) myself = 
    case myself of
        h:x:xs -> Zone newAttribs newChildren
        h:xs -> Zone newAttribs children
        _ -> z
    where
    newAttribs = attribs `M.union` (M.fromList (queriesToAttribs qList))
    newChildren = map (installInChosen qList myself) children
    queriesToAttribs :: [(String, QAT)] -> [(String, Attribute)]
    queriesToAttribs = map (\(name, qat) -> (name, Aquery (Just qat)))
    installInChosen qList myself@(h:x:xs) z@(Zone attribs _) =
        if (M.lookup "name" attribs)==(Just (Astr (Just h)))
            then installQueries qList z (x:xs)
            else z

installAndPerform qList zones myself =
    performQueries (installQueries qList zones myself)

performQueries = id

main = do
    qText <- getContents
    qList <- case parse qText of
        Left err -> panic (show err) >> return []
        Right qList -> return qList
    let newZones = installAndPerform qList zones myself 
    putStr $ printAttribs newZones

