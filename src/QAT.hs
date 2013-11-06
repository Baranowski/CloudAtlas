module QAT where

data QAT =
    QAT [Qsel] (Maybe Qexpr) [Qorder]

data Qsel =
    Qsel Qexpr (Maybe String)

data Qorder =
    Qorder Qexpr Order NullOrder

data Order = Oasc | Odesc
data NullOrder = Onfirst | Onlast

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

data MulOper = OpMul | OpDiv | OpMod
data AddOper = OpAdd | OpSub
data CmpRel = Rle
            | Rlt
            | Rge
            | Rgt
            | Req
            | Rne

