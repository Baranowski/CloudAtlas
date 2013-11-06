module QAT where

data QAT =
    QAT [Qsel] (Maybe Qexpr) [Qorder]
    deriving (Show)

data Qsel =
    Qsel Qexpr (Maybe String)
    deriving (Show)

data Qorder =
    Qorder Qexpr Order NullOrder
    deriving (Show)

data Order = Oasc | Odesc
    deriving (Show)
data NullOrder = Onfirst | Onlast
    deriving (Show)

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
           | Equery QAT
           | Estr String
           | Efloat Double
           | Eint Int
           | Elist [Qexpr]
           | Ebraces
           | Esquare
           | Evar String
           | Eapp String [Qexpr]
    deriving (Show)

data MulOper = OpMul | OpDiv | OpMod
    deriving (Show)
data AddOper = OpAdd | OpSub
    deriving (Show)
data CmpRel = Rle
            | Rlt
            | Rge
            | Rgt
            | Req
            | Rne
    deriving (Show)

