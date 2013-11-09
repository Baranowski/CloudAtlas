module QAT where

data QAT =
    QAT [Qsel] (Maybe Qexpr) [Qorder]
    deriving (Show,Eq)

data Qnested =
    Qnested Qexpr (Maybe Qexpr) [Qorder]
    deriving (Show,Eq)

data Qsel =
    Qsel Qexpr String
    deriving (Show,Eq)

data Qorder =
    Qorder Qexpr Order NullOrder
    deriving (Show,Eq)

data Order = Oasc | Odesc
    deriving (Show,Eq)
data NullOrder = Onfirst | Onlast
    deriving (Show,Eq)

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
           | Elist [Qexpr]
           | Ebraces
           | Esquare
           | Evar String
           | Eapp String [Qexpr]
    deriving (Show,Eq)

data MulOper = OpMul | OpDiv | OpMod
    deriving (Show,Eq)
data AddOper = OpAdd | OpSub
    deriving (Show,Eq)
data CmpRel = Rle
            | Rlt
            | Rge
            | Rgt
            | Req
            | Rne
    deriving (Show,Eq)
