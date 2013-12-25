module Interpreter(performQueries) where

import System.Random
import Prelude hiding (pred)
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import Data.List
import Data.Maybe
import Data.Time.Clock
import qualified Data.Map as M
import Text.Regex.Posix

import Zones
import QAT
import Utils

type IerSt = (StdGen, UTCTime)
performQueries :: ZoneS -> WriterT [String] (StateT IerSt Identity) ZoneS
performQueries z@(ZoneS attribs []) = return z
performQueries z@(ZoneS attribs children) = do
    newKids <- mapM performQueries children
    let atL = M.elems attribs
    let queries = map getQuery (filter isNonNullQuery atL)
    newAttrsNested <- mapM (singleQuery newKids) queries
    let newAttrs = concat newAttrsNested
    specialAttrs <- special attribs newKids
    let updatedAttrs = specialAttrs `M.union` (M.fromList newAttrs)
    return $ ZoneS updatedAttrs newKids
    where
    special attrs newKids = do
        let filtered = (M.filterWithKey isSpecial attrs)
        t <- gets snd
        contactAttrs <- singleQuery newKids contactQ
        let computed = ("freshness", Atime $ Just t):contactAttrs
        let specials = filtered `M.union` (M.fromList computed)
        return specials
        where
        isSpecial n (Aquery _) = True
        isSpecial n _ = n `elem` [ "timestamp"
                                 , "owner"
                                 , "name"
                                 , "level"
                                 ]
    contactQ = QAT [Qsel (Eapp "random" [Eint 3, Eapp "unfold" [Evar "contacts"]]) "contacts"] Nothing []
    singleQuery newKids q = do
        res <- lift $ runErrorT $ runReaderT (performQ q) newKids
        case res of
            Left err -> do
                tell [err]
                return []
            Right x -> return x

type Interpretation res = ReaderT [ZoneS] (ErrorT String (StateT IerSt Identity)) res

left = fail

pred :: Qexpr -> ZoneS -> Interpretation Bool
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

cmpLT :: [Qorder] -> ZoneS -> ZoneS -> Interpretation Bool
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
            Oasc -> go xv yv
            Odesc -> go yv xv
    where
    go ax ay = do
        ord <- cmpAL ax ay
        case ord of
            LT -> return True
            EQ -> cmpLT ros x y
            GT -> return False

performAbs eval whOld ordering = do
    let rordering = reverse ordering
    let wh = fromMaybe Etrue whOld
    origKids <- ask
    kids <- filterM (pred wh) origKids
    kids <- iSortM (cmpLT rordering) kids
    eval kids

performQ :: QAT -> Interpretation [(String, Attribute)]
performQ q@(QAT sels whOld ordering) = (do
    performAbs evalQ whOld ordering
    ) `addTrace` ("in query: " ++ (show q))
    where
    evalQ kids = mapM (evalSel kids) sels
    evalSel kids (Qsel e n) = do
        val <- evalChk kids e
        case val of
            [res] -> return (n, res)
            _ -> left "Expected single value as a result"
performNested :: Qnested -> Interpretation [Attribute]
performNested (Qnested e whOld ordering) = do
    performAbs evalQ whOld ordering
    where
    evalQ kids = evalChk kids e

checkCol [] = return ()
checkCol [a] = return ()
checkCol (a:b:xs) = do
    when (not (sameType a b))
        (left $ "Type mismatch within single column (" ++ (printAType a) ++ " and " ++ (printAType b) ++ ")")
    checkCol (b:xs)

checkCols cols = do
    forM_ cols checkCol
    let maxLen = foldl (\i xs -> max i (length xs)) 0 cols
    mapM (extend maxLen) cols
    where
    extend len col = do
        if (length col)==1
            then return $ replicate len (head col)
            else if (length col) == len
                then return col
                else left "Columns lengths do not match"

evalChk zs e = do
    v <- eval zs e
    checkCol v
    return v

eval :: [ZoneS] -> Qexpr -> Interpretation [Attribute]
eval zs (Eor (e:es)) = do
    v <- evalChk zs e
    foldM (combine zs) v es
    where
    combine zs acc e = do
        v <- evalChk zs e
        [acc, v] <- checkCols [acc, v]
        zipWithM or' acc v
    or' (Abool (Just x)) (Abool (Just y)) =
        return $ Abool (Just (x || y))
    or' (Abool _) (Abool _) = return $ Abool Nothing
    or' _ _ = left "Trying to apply 'OR' a to non-boolean value"
eval zs (Eand (e:es)) = do
    v <- evalChk zs e
    foldM (combine zs) v es
    where
    combine zs acc e = do
        v <- evalChk zs e
        [acc, v] <- checkCols [acc, v]
        zipWithM and' acc v
    and' (Abool (Just x)) (Abool (Just y)) =
        return $ Abool (Just (x && y))
    and' (Abool _) (Abool _) = return $ Abool Nothing
    and' _ _ = left "Trying to apply 'AND' a to non-boolean value"
eval zs (Eadd e ops) = do
    v <- evalChk zs e
    foldM (combine zs) v ops
    where
    combine zs acc (op, e) = do
        v <- evalChk zs e
        [acc, v] <- checkCols [acc, v]
        zipWithM (go op) acc v
    go OpAdd (Astr (Just x)) (Astr (Just y)) =
        return (Astr (Just (x ++ y)))
    go OpAdd (Astr _) (Astr _) = return $ Astr Nothing
    go OpAdd (Atime (Just x)) (Aduration (Just d)) =
        return $ Atime $ Just $ (diffTConvert (picosecondsToDiffTime $d*dPrecDiff)) `addUTCTime` x
    go OpAdd (Atime _) (Aduration _) =
        return $ Atime Nothing
    go OpAdd d@(Aduration _) t@(Atime _) = go OpAdd t d
    go OpSub (Atime (Just x)) (Atime (Just y)) =
        return $ Aduration $ Just $ round $ (toRational $ x `diffUTCTime` y) * (toRational dPrecision)
    go OpSub (Atime _) (Atime _) = return $ Atime Nothing
    go OpAdd (Alist _ (Just x)) (Alist _ (Just y)) =
        return $ Alist 0 $ Just $ x ++ y
    go OpAdd (Alist _ _) (Alist _ _) =
        return $ Alist 0 Nothing
    go OpAdd (Aset _ (Just x)) (Aset _ (Just y)) =
        return $ Aset 0 (Just (x `union` y))
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
        [acc, v] <- checkCols [acc, v]
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
eval _ Etrue = return [Abool $ Just True]
eval _ Efalse = return [Abool $ Just False]
eval _ (Estr s) = return [Astr $ Just s]
eval _ (Eint i) = return [Aint $ Just $ fromIntegral i]
eval _ (Efloat f) = return [Afloat $ Just f]
eval zs (Evar s) =
    mapM (getAOrFail s) zs
    where
    getAOrFail name z =
        case getAttr name z of
            Just x -> return x
            Nothing -> left $ "Attribute '" ++ name ++ "' not present"
eval zs (Eneg e) = do
    v <- evalChk zs e
    mapM neg v
    where
    neg (Aint (Just x)) = return $ Aint (Just (-x))
    neg (Aint _) = return $ Aint Nothing
    neg (Afloat (Just x)) = return $ Afloat (Just (-x))
    neg (Afloat _) = return $ Afloat Nothing
    neg _ = left "Trying to negate non-numeric value"
eval zs (Enot e) = do
    v <- evalChk zs e
    mapM not' v
    where
    not' (Abool (Just x)) = return $ Abool (Just (not x))
    not' (Abool (Nothing)) = return $ Abool Nothing
    not' _ = left "Trying 'NOT' on a non-boolean value"
eval zs (Erexp e r) = do
    v <- evalChk zs e
    mapM (rexp r) v
    where
    rexp r (Astr (Just s)) =
        return $ Abool (Just (r =~ s))
    rexp r (Astr _) = return $ Abool Nothing
    rexp r _ = left "Trying to match regexp to a non-string"
eval _ (Equery q) = performNested q
eval zs (Erel rel ea eb) = do
    va <- evalChk zs ea
    vb <- evalChk zs eb
    zipWithM (go rel) va vb
    where
    go rel x y = if (isNull x) || (isNull y)
        then return $ Abool Nothing
        else do
            ord <- cmpAL x y
            let desired = case rel of
                            Rle -> [LT, EQ]
                            Rlt -> [LT]
                            Rge -> [GT, EQ]
                            Rgt -> [GT]
                            Req -> [EQ]
                            Rne -> [LT, GT]
            return $ Abool (Just (ord `elem` desired))
eval zs (Eapp name esOld) = do
    vs <- mapM (evalChk zs) esOld
    builtin name vs

cmpAL x y = cmpA x y
cmpA :: Attribute -> Attribute -> Interpretation Ordering
cmpA (Aint (Just a)) (Aint (Just b)) = return $ compare a b
cmpA (Astr (Just a)) (Astr (Just b)) = return $ compare a b
cmpA (Aduration (Just a)) (Aduration (Just b)) = return $ compare a b
cmpA (Atime (Just a)) (Atime (Just b)) = return $ compare a b
cmpA (Afloat (Just a)) (Afloat (Just b)) = return $ compare a b
cmpA _ _ = fail "Trying to compare values of incompatible or unsupported types"

{- BUILTIN FUNCTIONS -}
builtin :: String -> [[Attribute]] -> Interpretation [Attribute]
builtin "first" [nl, col] = do
    n <- case nl of
        [Aint (Just x)] -> return x
        _ -> fail "Invalid numeric argument to 'first' or 'last'"
    return [Alist 0 (Just (take (fromIntegral n) col))]
builtin "last" [nl, col] = builtin "first" [nl, reverse col]
builtin "random" [nl, col] = do
    n <- case nl of
        [Aint (Just x)] -> return x
        _ -> fail "Invalid numeric argument to 'random'"
    inds <- makeInds (length col) (min n (fromIntegral $ length col)) []
    return [Aset 0 $ Just $ map ((!!) col) inds]
    where
      makeInds bound 0 acc = return acc
      makeInds bound n acc = do
        g <- gets fst
        let (i,newG) = randomR (0, bound-1) g
        modify $ \(_,t) -> (newG,t)
        if i `elem` acc
            then makeInds bound n acc
            else makeInds bound (n-1) (i:acc)
builtin "count" [col] = return [Aint (Just $ fromIntegral $ length col)]
builtin "min" [x:xs] = extremeCmp x xs LT
builtin "max" [x:xs] = extremeCmp x xs GT
builtin "sum" [x:xs] = do
    res <- foldM add x xs
    return [res]
    where
    add (Aint (Just a)) (Aint (Just b)) = return $ Aint (Just (a+b))
    add atA@(Aint _) (Aint Nothing) = return $ atA
    add (Aint Nothing) atB@(Aint _) = return $ atB
    add (Afloat (Just a)) (Afloat (Just b)) = return $ Afloat (Just (a+b))
    add atA@(Afloat _) (Afloat Nothing) = return $ atA
    add (Afloat Nothing) atB@(Afloat _) = return $ atB
    add (Aduration (Just a)) (Aduration (Just b)) = return $ Aduration (Just (a+b))
    add atA@(Aduration _) (Aduration Nothing) = return $ atA
    add (Aduration Nothing) atB@(Aduration _) = return $ atB
    add _ _ = fail "Applying 'sum' to unsupported value type"
builtin "avg" [col] = do
    [s] <- builtin "sum" [col]
    let n = length $ filter (\x -> not $ isNull x) col
    case n of
        0 -> return [s]
        _ -> case s of
            (Aint (Just x)) -> return [Afloat (Just ((fromIntegral x)/(fromIntegral n)))]
            (Afloat (Just x)) -> return [Afloat (Just (x/(fromIntegral n)))]
            (Aduration (Just x)) -> return [Aduration $ Just $ round $ (fromIntegral x)/(fromIntegral n)]
            _ -> fail "Applying 'avg' to unsupported value type"
builtin "land" [col] = do
    return [Abool (Just (all isTrue col))]
    where
    isTrue x = x == Abool (Just True)
builtin "lor" [col] = do
    return [Abool (Just (any isTrue col))]
    where
    isTrue x = x == Abool (Just True)
builtin "epoch" [] = return [Atime (Just epoch)]
builtin "unfold" [col] = do -- TODO FIXME
    res <- mapM go col
    return $ concat res
    where
    go (Alist _ Nothing) = return []
    go (Alist _ (Just xs)) = return xs
    go (Aset _ Nothing) = return []
    go (Aset _ (Just xs)) = return xs
builtin "distinct" [col] = do
    foldM go [] col
    where
    go xs x = return $ xs `union` [x]
builtin "now" [] = do
    t <- gets snd
    return [Atime $ Just t]
builtin name [col] =
    mapM (aBuiltin name) col
builtin name _ = fail $ "Function '" ++ name ++ "': unknown function or unsupported argument list"
aBuiltin "ceil" x = absRound ceiling x
aBuiltin "floor" x = absRound floor x
aBuiltin "round" x = absRound round x
aBuiltin "size" (Astr (Just s)) = return $ Aint (Just $ fromIntegral $ length s)
aBuiltin "size" (Alist _ (Just l)) = return $ Aint (Just $ fromIntegral (length l))
aBuiltin "size" (Aset _ (Just s)) = return $ Aint (Just $fromIntegral (length s))
aBuiltin "size" (Astr Nothing) = return $ Aint Nothing
aBuiltin "size" (Alist _ Nothing) = return $ Aint Nothing
aBuiltin "size" (Aset _ Nothing) = return $ Aint Nothing
aBuiltin "to_string" a = return $ Astr (Just (printAVal a))
aBuiltin "to_boolean" (Astr (Just "TRUE")) =
    return $ Abool (Just True)
aBuiltin "to_boolean" (Astr (Just "FALSE")) =
    return $ Abool (Just False)
aBuiltin "to_boolean" (Astr Nothing) =
    return $ Abool Nothing
aBuiltin "to_integer" (Astr (Just x)) = case reads x of
    [(i, "")] -> return $ Aint (Just i)
    _ -> fail $ "Cannot convert to integer: " ++ x
aBuiltin "to_integer" (Astr Nothing) = return $ Aint Nothing
aBuiltin "to_integer" (Afloat (Just x)) =
    return $ Aint $ Just $ round x
aBuiltin "to_integer" (Afloat Nothing) =
    return $ Aint Nothing
aBuiltin "to_integer" (Aduration Nothing) =
    return $ Aint Nothing
aBuiltin "to_integer" (Aduration (Just x)) =
    return $ Aint (Just $ fromIntegral x)
aBuiltin "to_double" (Aint (Just x)) =
    return $ Afloat (Just $ fromIntegral x)
aBuiltin "to_double" (Aint Nothing) =
    return $ Afloat Nothing
aBuiltin "to_double" (Astr (Just s)) = case reads s of
    [(i, "")] -> return $ Afloat (Just i)
    _ -> fail $ "Cannot convert to double: " ++ s
aBuiltin "to_double" (Astr Nothing) =
    return $ Afloat Nothing
aBuiltin "to_time" (Astr (Just s)) =
    return $ Atime $ Just $ timeFromStr s
aBuiltin "to_time" (Astr Nothing) =
    return $ Atime Nothing
aBuiltin "to_duration" (Astr Nothing) =
    return $ Aduration Nothing
aBuiltin "to_duration" (Astr (Just x)) =
    case durFromStr x of
        Just d -> return $ Aduration $ Just $ fromIntegral d
        Nothing -> fail $ "'to_duration': Invalid duration format"
aBuiltin name _ = fail $ "'" ++ name ++ "': unknown function or unsupported argument type"

absRound f (Afloat (Just x)) =
    return $ Afloat $ Just $ fromIntegral $ f x
absRound f (Afloat Nothing) = return $ Afloat  Nothing

extremeCmp x xs desired = do
    res <- foldM go x xs
    return [res]
    where
    go xv y = do
        case (isNull xv, isNull y) of
            (False, False) -> do
                ord <- cmpA xv y
                if (ord == desired)
                    then return xv
                    else return y
            (False, True) -> return xv
            (True, _) -> return y
