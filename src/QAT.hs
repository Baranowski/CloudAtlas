{-# LANGUAGE FlexibleInstances #-}
module QAT where

import Data.List

data QAT =
    QAT [Qsel] (Maybe Qexpr) [Qorder]
    deriving (Eq)
instance Show QAT where
    show (QAT sels mbWhere orders) =
        "SELECT " ++ (concat $ intersperse ", " (map show sels)) ++
            " " ++ (whereClause mbWhere) ++
            " " ++ (orderClause orders)

whereClause Nothing = ""
whereClause (Just e) = "WHERE " ++ (show e)
orderClause [] = ""
orderClause xs = "ORDER BY " ++ (concat $ intersperse ", " (map show xs))

data Qnested =
    Qnested Qexpr (Maybe Qexpr) [Qorder]
    deriving (Eq)
instance Show Qnested where
    show (Qnested e mbWhere orders) =
        "SELECT " ++ (show e) ++
            " " ++ (whereClause mbWhere) ++
            " " ++ (orderClause orders)

data Qsel =
    Qsel Qexpr String
    deriving (Eq)
instance Show Qsel where
    show (Qsel e s) = "(" ++ (show e) ++ ") AS " ++ s

data Qorder =
    Qorder Qexpr Order NullOrder
    deriving (Eq)
instance Show Qorder where
    show (Qorder e ord nord) = (show e) ++ " " ++ (show ord) ++ " " ++ (show nord)

data Order = Oasc | Odesc
    deriving (Eq)
instance Show Order where
    show Oasc = "ASC"
    show Odesc = "DESC"

data NullOrder = Onfirst | Onlast
    deriving (Eq)
instance Show NullOrder where
    show Onfirst = "NULLS FIRST"
    show Onlast = "NULLS LAST"

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
    deriving (Eq)
instance Show Qexpr where
    show (Eor es) = "(" ++ (concat $ intersperse " OR " (map show es)) ++ ")"
    show (Eand es) = "(" ++ (concat $ intersperse " AND " (map show es)) ++ ")"
    show (Enot e) = "NOT " ++ show e
    show (Erel r e1 e2) = (show e1) ++ " " ++ (show r) ++ " " ++ (show e2)
    show (Erexp e s) = (show e) ++ " REGEXP \"" ++ s ++ "\""
    show (Eadd e l) = "(" ++ (show e) ++ (concatMap mulShow l) ++ ")"
    show (Emul e l) = "(" ++ (show e) ++ (concatMap addShow l) ++ ")"
    show (Eneg e) = "-" ++ (show e)
    show Etrue = "TRUE"
    show Efalse = "FALSE"
    show (Equery q)= "(" ++ (show q) ++ ")"
    show (Estr s) = "\"" ++ s ++ "\""
    show (Efloat d) = show d
    show (Eint i) = show i
    show (Evar s) = s
    show (Eapp s es) = s ++ "(" ++ (concat $ intersperse ", " (map show es)) ++ ")"

mulShow (m, e) = " " ++ (show m) ++ " " ++ (show e)
addShow (m, e) = " " ++ (show m) ++ " " ++ (show e)

data MulOper = OpMul | OpDiv | OpMod
    deriving (Eq)
instance Show MulOper where
    show OpMul = "*"
    show OpDiv = "/"
    show OpMod = "%"
data AddOper = OpAdd | OpSub
    deriving (Eq)
instance Show AddOper where
    show OpAdd = "+"
    show OpSub = "-"

data CmpRel = Rle
            | Rlt
            | Rge
            | Rgt
            | Req
            | Rne
    deriving (Eq)
instance Show CmpRel where
    show Rle = "<="
    show Rlt = "<"
    show Rge = ">="
    show Rgt = ">"
    show Req = "="
    show Rne = "!="
