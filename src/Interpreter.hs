module Interpreter(performQueries) where

import Prelude hiding (pred)
import Control.Monad.Error
import Control.Monad.Reader
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Map as M

import Zones
import QAT

performQueries :: Zone -> Either String Zone
performQueries z@(Zone attribs children) = do
    newKids <- mapM performQueries children
    let atL = map snd (M.toList attribs)
    let queries = map getQuery (filter isNonNullQuery atL)
    newAttrsNested <- mapM (\x -> runReaderT (performQ x) newKids) queries
    let newAttrs = concat newAttrsNested
    let updatedAttrs = (M.fromList newAttrs) `M.union` attribs
    return $ Zone updatedAttrs newKids

type Interpretation res = (ReaderT [Zone] (Either String)) res

left = lift . Left

pred :: Qexpr -> Zone -> Interpretation Bool
pred e z = do
    val <- evalChk [z] e
    case val of
        [Abool (Just x)] -> return x
        [Abool Nothing] -> return False
        [x] -> left $ "Expected boolean value, got " ++ (printAType x)
        _ -> left "Expected single value"

iSortM f [] = return $ []
iSortM f (x:xs) = do
    newxs <- iSortM f xs
    insertM f x newxs

insertM f x [] = return $ [x]
insertM f x (y:ys) = do
    lt <- f x y
    if lt
        then return $ x:y:ys
        else do
            newys <- insertM f x ys
            return $ y:newys

cmpLT [] _ _ = return True
cmpLT ((Qorder e ord nord):ros) x y = do
    xVal <- evalChk [x] e
    yVal <- evalChk [y] e
    (xv, yv) <- case (xVal, yVal) of
        ([xv], [yv]) -> return (xv, yv)
        _ -> left "Ordering expression returned multiple results"
    case (isNull xv, isNull yv) of
        (True, False) -> return (nord == Onfirst)
        (False, True) -> return (nord == Onlast)
        (True, True) -> return False
        (False, False) -> case ord of
            Oasc -> go x y
            Odesc -> go y x
    where
    go x y = return True
    {-
    Aint (Just ia)
    Astr (Just sa)
    Atime (Just ta)
    Aset Int (Just sa)
    Alist Int (Just la)
    Abool (Just ba)
    Aquery (Just qa)
    Acontact (Just sa)
    Aduration (Just da)
    Afloat (Just da)
    -}

performQ :: QAT -> Interpretation [(String, Attribute)]
performQ (QAT sels whOld ordering) = do
    let rordering = reverse ordering
    let wh = fromMaybe Etrue whOld
    origKids <- ask
    kids <- filterM (pred wh) origKids
    kids <- iSortM (cmpLT rordering) kids
    mapM (evalSel kids) sels
    where
        evalSel kids (Qsel e n) = do
            val <- evalChk kids e
            case val of
                [res] -> return (n, res)
                _ -> left "Expected single value as a result"

checkCol [] = return ()
checkCol [a] = return ()
checkCol (a:b:xs) =
    when (not (sameType a b))
        (left "Type mismatch within single column")

checkCols as bs = do
    checkCol as
    checkCol bs
    when ((length as) /= (length bs))
        (left "Two columns have different lengths")

evalChk zs e = do
    v <- eval zs e
    checkCol v
    return v

eval :: [Zone] -> Qexpr -> Interpretation [Attribute]
eval zs (Eor (e:es)) = do
    v <- evalChk zs e
    foldM (combine zs) v es
    where
    combine zs acc e = do
        v <- evalChk zs e
        checkCols acc v
        zipWithM or' acc v
    or' (Abool (Just x)) (Abool (Just y)) =
        return $ Abool (Just (x || y))
    or' (Abool _) (Abool _) = return $ Abool Nothing
    or' _ _ = left "Trying to apply 'OR' a to non-boolean value"
eval zs (Eadd e ops) = do
    v <- evalChk zs e
    foldM (combine zs) v ops
    where
    combine zs acc (op, e) = do
        v <- evalChk zs e
        checkCols acc v
        zipWithM (go op) acc v
    go OpAdd (Astr (Just x)) (Astr (Just y)) =
        return (Astr (Just (x ++ y)))
    go OpAdd (Astr _) (Astr _) = return $ Astr Nothing
    go OpAdd (Atime (Just x)) (Aduration (Just d)) =
        return $ Atime (Just x) -- TODO
    go OpAdd (Atime _) (Aduration _) =
        return $ Atime Nothing
    go OpAdd d@(Aduration _) t@(Atime _) = go OpAdd t d
    go OpSub (Atime (Just x)) (Atime (Just y)) =
        return (Aduration (Just "")) -- TODO
    go OpSub (Atime _) (Atime _) = return $ Atime Nothing
    go OpAdd (Alist _ (Just x)) (Alist _ (Just y)) =
        return $ Alist 0 $ Just $ x ++ y
    go OpAdd (Alist _ _) (Alist _ _) =
        return $ Alist 0 Nothing
    go OpAdd (Aset _ (Just x)) (Aset _ (Just y)) =
        return $ Aset 0 (Just (x ++ y)) -- TODO
    go OpAdd (Aset _ _ ) (Aset _ _) =
        return $ Aset 0 Nothing
    go op (Aint (Just x)) (Aint (Just y)) =
        return $ Aint (Just ((oper op) x y))
    go op (Aint Nothing) (Aint Nothing) =
        return $ Aint Nothing
    go op (Afloat (Just x)) (Afloat (Just y)) =
        return $ Afloat (Just ((oper op) x y))
    go op (Afloat Nothing) (Afloat Nothing) =
        return $ Afloat Nothing
    go _ _ _ =
        left "Additive operation for unsupported types"
    oper OpAdd = (+)
    oper OpSub = (-)
eval zs (Emul e ops) = do
    v <- evalChk zs e
    foldM (combine zs) v ops
    where
    combine zs acc (op, e) = do
        v <- evalChk zs e
        checkCols acc v
        zipWithM (go op) acc v
    go OpMul (Aint (Just x)) (Aint (Just y)) =
        return $ Aint (Just (x*y))
    go OpMul (Afloat (Just x)) (Afloat (Just y)) =
        return $ Afloat (Just (x*y))
    go OpDiv (Aint (Just x)) (Aint (Just y)) = do
        when (y == 0) (left "Division by zero")
        return $ Aint (Just (x `div` y))
    go OpDiv (Afloat (Just x)) (Afloat (Just y)) = do
        when (y == 0.0) (left "Division by zero")
        return $ Afloat (Just (x / y))
    go OpMod (Aint (Just x)) (Aint (Just y)) = do
        when (y == 0) (left "Division by zero")
        return $ Aint (Just (x `mod` y))
    go _ (Aint _) (Aint _) = return $ Aint Nothing
    go OpMul (Afloat _) (Afloat _) = return $ Afloat Nothing
    go OpDiv (Afloat _) (Afloat _) = return $ Afloat Nothing
    go _ _ _ = left "Multiplication or division for unsupported types"
