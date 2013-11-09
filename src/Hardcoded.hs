module Hardcoded(zones) where

import qualified Data.Map as M

zones =
Zone (M.fromList
[ ("level", Aint (Just 0))
, ("name", Astr (Just null))
, ("owner", Astr (Just "/uw/violet07"))
, ("Ttimestamp", Atime (Just $ timeFromStr "2012/11/09 $ timeFromStr "20:10:17.342"))
, ("contacts", Aset Tcontact Nothing) ({}))
, ("cardinality", Aint (Just 0))
]) [
    (Zone (M.fromList
    [ ("level", Aint (Just 1))
    , ("name", Astr (Just "uw"))
    , ("owner", Astr (Just "/uw/violet07"))
    , ("Ttimestamp", Atime (Just $ timeFromStr "2012/11/09 $ timeFromStr "20:8:13.123"))
    , ("contacts", Aset 0 Tcontact Nothing))
    , ("cardinality", Aint (Just 0))
    ]) [
        (Zone (M.fromList
        [ ("level", Aint (Just 2))
        , ("name", Astr (Just "violet07"))
        , ("owner", Astr (Just "/uw/violet07"))
        , ("Ttimestamp", Atime (Just $ timeFromStr "2012/11/09 18:00:00.000"))
        , ("contacts", Aset 3 Tcontact (Just ["UW1A", "UW1B", "UW1C"]))
        , ("cardinality", Aint (Just 1))
        , ("members", Aset 3 Tcontact) ({UW1}))
        , ("creation", Atime (Just $ timeFromStr "2011/11/09 20:8:13.123"))
        , ("cpu_usage", Afloat (Just 0.9))
        , ("num_cores", Aint (Just 3))
        , ("has_ups", Abool (Just NULL))
        , ("some_names", Alist 3 Tstr) (["tola", "tosia"]))
        , ("expiry", Aduration (Just +13 12:00:00.000))
        ]) []),
        (Zone (M.fromList
        [ ("level", Aint (Just 2))
        , ("name", Astr (Just "khaki31"))
        , ("owner", Astr (Just "/uw/khaki31"))
        , ("Ttimestamp", Atime (Just $ timeFromStr "2012/11/09 20:03:00.000"))
        , ("contacts", Aset 3 Tcontact) ({UW2A}))
        , ("cardinality", Aint (Just 1))
        , ("members", Aset  Tcontact (Just 3)) ({UW2A}))
        , ("creation", Atime (Just $ timeFromStr "2011/11/09 20:12:13.123"))
        , ("cpu_usage", Afloat (Just NULL))
        , ("num_cores", Aint (Just 3))
        , ("has_ups", Abool (Just false))
        , ("some_names", Alist 3 Tstr) (["agatka", "beatka", "celina"]))
        , ("expiry", Aduration (Just -13 11:00:00.000))
        ]) []),
        (Zone (M.fromList
        [ ("level", Aint (Just 2))
        , ("name", Astr (Just "khaki13"))
        , ("owner", Astr (Just "/uw/khaki13"))
        , ("Ttimestamp", Atime (Just $ timeFromStr "2012/11/09 21:03:00.000"))
        , ("contacts", Aset 3 Tcontact) ({UW3A, UW3B}))
        , ("cardinality", Aint (Just 1))
        , ("members", Aset 3 Tcontact) ({UW3B}))
        , ("creation", Atime (Just NULL))
        , ("cpu_usage", Afloat (Just 0.1))
        , ("num_cores", Aint (Just NULL))
        , ("has_ups", Abool (Just true))
        , ("some_names", Alist 3 Tstr) ([]))
        , ("expiry", Aduration (Just NULL))
        ]) [])
    ]),
    (Zone (M.fromList
    [ ("level", Aint (Just 1))
    , ("name", Astr (Just "pjwstk"))
    , ("owner", Astr (Just "/pjwstk/whatever01"))
    , ("Ttimestamp", Atime (Just $ timeFromStr "2012/11/09 20:8:13.123"))
    , ("contacts", Aset Tcontact Nothing) ({}))
    , ("cardinality", Aint (Just 0))
    ]) [
        (Zone (M.fromList
        [ ("level", Aint (Just 2))
        , ("name", Astr (Just "whatever01"))
        , ("owner", Astr (Just "/pjwstk/whatever01"))
        , ("Ttimestamp", Atime (Just $ timeFromStr "2012/11/09 21:12:00.000"))
        , ("contacts", Aset 3 Tcontact) ({PJ1}))
        , ("cardinality", Aint (Just 1))
        , ("members", Aset 3 Tcontact) ({PJ1}))
        , ("creation", Atime (Just $ timeFromStr "2012/10/18 07:03:00.000"))
        , ("cpu_usage", Afloat (Just 0.1))
        , ("num_cores", Aint (Just 7))
        , ("php_modules", Alist 3 Tstr) (["rewrite"]))
        ]) []),
        (Zone (M.fromList
        [ ("level", Aint (Just 2))
        , ("name", Astr (Just "whatever02"))
        , ("owner", Astr (Just "/pjwstk/whatever02"))
        , ("Ttimestamp", Atime (Just $ timeFromStr "2012/11/09 21:13:00.000"))
        , ("contacts", Aset 3 Tcontact) ({PJ2}))
        , ("cardinality", Aint (Just 1))
        , ("members", Aset 3 Tcontact) ({PJ2}))
        , ("creation", Atime (Just $ timeFromStr "2012/10/18 07:04:00.000"))
        , ("cpu_usage", Afloat (Just 0.4))
        , ("num_cores", Aint (Just 13))
        , ("php_modules", Alist 3 Tstr) (["odbc"]))
        ]) [])
    ])
]
