module Interpreter(performQueries) where

import Control.Monad.Error
import Data.Either
import Data.List

performQueries :: Zone -> Either String Zone
performQueries z@(Zone attribs children) = do
    newKids <- mapM performQueries children
    let queries = map getQuery (filter isNonNullQuery attribs)
    newAttrsNested <- mapM (runReaderT performQ newKids) queries
    let newAttrs = concat newAttrsNested
    let updatedAttrs = (M.fromList newAttrs) `M.union` attribs
    return $ Zone updatedAttrs newKids

type Interpretation res = ReaderT [Zone] (Either String res)

pred :: Qexpr -> Zone -> Interpretation Bool
pred e z = do
    val <- eval [z] e
    case val of
        [Abool (Just x)] -> return x
        [Abool Nothing] -> return False
        [x] -> Left $ "Expected boolean value, got " ++ (printAType x)
        _ -> Left "Expected single value"

performQ :: QAT -> [(String, Attribute)]
performQ (QAT sels wh ordering) = do
    let rordering = reverse ordering
    let wh = fromMaybe Etrue wh
    origKids <- ask
    kids <- filterM (pred wh) origKids
    kids <- sortByM (cmp rordering) kids
    mapM evalSel kids sels
    where
        evalSel kids (Qsel e n) = do
            val <- eval kids e
            return (n, val)
