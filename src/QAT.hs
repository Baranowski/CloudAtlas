{-# LANGUAGE FlexibleInstances #-}
module QAT where

import Data.List

class PPShow a where
    ppshow :: a -> String

data QAT =
    QAT [Qsel] (Maybe Qexpr) [Qorder]
    deriving (Eq,Show,Read)
instance PPShow QAT where
    ppshow (QAT sels mbWhere orders) =
        "SELECT " ++ (concat $ intersperse ", " (map ppshow sels)) ++
            " " ++ (whereClause mbWhere) ++
            " " ++ (orderClause orders)

whereClause :: Maybe Qexpr -> String
whereClause Nothing = ""
whereClause (Just e) = "WHERE " ++ (ppshow e)

orderClause :: [Qorder] -> String
orderClause [] = ""
orderClause xs = "ORDER BY " ++ (concat $ intersperse ", " (map ppshow xs))

data Qnested =
    Qnested Qexpr (Maybe Qexpr) [Qorder]
    deriving (Eq,Show,Read)
instance PPShow Qnested where
    ppshow (Qnested e mbWhere orders) =
        "SELECT " ++ (ppshow e) ++
            " " ++ (whereClause mbWhere) ++
            " " ++ (orderClause orders)

data Qsel =
    Qsel Qexpr String
    deriving (Eq,Show,Read)
instance PPShow Qsel where
    ppshow (Qsel e s) = "(" ++ (ppshow e) ++ ") AS " ++ s

data Qorder =
    Qorder Qexpr Order NullOrder
    deriving (Eq,Show,Read)
instance PPShow Qorder where
    ppshow (Qorder e ord nord) = (ppshow e) ++ " " ++ (ppshow ord) ++ " " ++ (ppshow nord)

data Order = Oasc | Odesc
    deriving (Eq,Show,Read)
instance PPShow Order where
    ppshow Oasc = "ASC"
    ppshow Odesc = "DESC"

data NullOrder = Onfirst | Onlast
    deriving (Eq,Show,Read)
instance PPShow NullOrder where
    ppshow Onfirst = "NULLS FIRST"
    ppshow Onlast = "NULLS LAST"

data Qexpr = Eor [Qexpr]
           | Eand [Qexpr]
           | Enot Qexpr
           | Erel CmpRel Qexpr Qexpr
           | Erexp Qexpr String
           | Eadd Qexpr [(AddOper, Qexpr)]
           | Emul Qexpr [(MulOper, Qexpr)]
           | Eneg Qexpr
           | Etrue
           | Efalse
           | Equery Qnested
           | Estr String
           | Efloat Double
           | Eint Int
           | Evar String
           | Eapp String [Qexpr]
    deriving (Eq,Show,Read)
instance PPShow Qexpr where
    ppshow (Eor es) = "(" ++ (concat $ intersperse " OR " (map ppshow es)) ++ ")"
    ppshow (Eand es) = "(" ++ (concat $ intersperse " AND " (map ppshow es)) ++ ")"
    ppshow (Enot e) = "NOT " ++ ppshow e
    ppshow (Erel r e1 e2) = (ppshow e1) ++ " " ++ (ppshow r) ++ " " ++ (ppshow e2)
    ppshow (Erexp e s) = (ppshow e) ++ " REGEXP \"" ++ s ++ "\""
    ppshow (Eadd e l) = "(" ++ (ppshow e) ++ (concatMap addShow l) ++ ")"
    ppshow (Emul e l) = "(" ++ (ppshow e) ++ (concatMap mulShow l) ++ ")"
    ppshow (Eneg e) = "-" ++ (ppshow e)
    ppshow Etrue = "TRUE"
    ppshow Efalse = "FALSE"
    ppshow (Equery q)= "(" ++ (ppshow q) ++ ")"
    ppshow (Estr s) = "\"" ++ s ++ "\""
    ppshow (Efloat d) = show d
    ppshow (Eint i) = show i
    ppshow (Evar s) = s
    ppshow (Eapp s es) = s ++ "(" ++ (concat $ intersperse ", " (map ppshow es)) ++ ")"

mulShow :: (MulOper, Qexpr) -> String
mulShow (m, e) = " " ++ (ppshow m) ++ " " ++ (ppshow e)

addShow :: (AddOper, Qexpr) -> String
addShow (m, e) = " " ++ (ppshow m) ++ " " ++ (ppshow e)

data MulOper = OpMul | OpDiv | OpMod
    deriving (Eq,Show,Read)
instance PPShow MulOper where
    ppshow OpMul = "*"
    ppshow OpDiv = "/"
    ppshow OpMod = "%"
data AddOper = OpAdd | OpSub
    deriving (Eq,Show,Read)
instance PPShow AddOper where
    ppshow OpAdd = "+"
    ppshow OpSub = "-"

data CmpRel = Rle
            | Rlt
            | Rge
            | Rgt
            | Req
            | Rne
    deriving (Eq,Show,Read)
instance PPShow CmpRel where
    ppshow Rle = "<="
    ppshow Rlt = "<"
    ppshow Rge = ">="
    ppshow Rgt = ">"
    ppshow Req = "="
    ppshow Rne = "!="
