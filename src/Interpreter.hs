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

performQ :: QAT -> ReaderT [Zone] (Either String [(String, Attribute)])
performQ (QAT sels wh ordering) = do
    let wh = fromMaybe Etrue wh
    origKids <- ask
    kids <- filterM (pred wh) origKids
    kids <- sortByM (cmp ordering) kids
    mapM evalSel kids sels
    where
        evalSel kids (Qsel e n) = do
            val <- eval kids e
            return (n, val)
