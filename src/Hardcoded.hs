module Hardcoded(zones) where

import Data.Bits
import Network.Socket
import qualified Data.Map as M
import Zones

defContact = ("10.0.0.1", "12345")
zones =
    ZoneS (M.fromList
    [ ("level", Aint (Just 0))
    , ("name", Astr Nothing)
    , ("owner", Astr (Just "/uw/violet07"))
    , ("timestamp", Atime (Just $ timeFromStr "2012/11/09 20:10:17.342"))
    , ("freshness", Atime (Just $ timeFromStr "2000/01/01 12:00:00.000"))
    , ("contacts", Aset 0 Nothing)
    , ("cardinality", Aint (Just 0))
    ]) [
        (ZoneS (M.fromList
        [ ("level", Aint (Just 1))
        , ("name", Astr (Just "uw"))
        , ("owner", Astr (Just "/uw/violet07"))
        , ("timestamp", Atime (Just $ timeFromStr "2012/11/09 20:08:13.123"))
        , ("freshness", Atime (Just $ timeFromStr "2000/01/01 12:00:00.000"))
        , ("contacts", Aset 0 Nothing)
        , ("cardinality", Aint (Just 0))
        ]) [
            (ZoneS (M.fromList
            [ ("level", Aint (Just 2))
            , ("name", Astr (Just "violet07"))
            , ("owner", Astr (Just "/uw/violet07"))
            , ("timestamp", Atime (Just $ timeFromStr "2012/11/09 18:00:00.000"))
            , ("freshness", Atime (Just $ timeFromStr "2000/01/01 12:00:00.000"))
            , ("contacts", Aset 0 Nothing)
            , ("cardinality", Aint (Just 1))
            , ("members", Aset 3 (Just [(Acontact (Just defContact))]))
            , ("creation", Atime (Just $ timeFromStr "2011/11/09 20:08:13.123"))
            , ("cpu_usage", Afloat (Just 0.9))
            , ("num_cores", Aint (Just 3))
            , ("has_ups", Abool Nothing)
            , ("some_names", Alist 3 (Just [(Astr (Just "tola")), (Astr (Just "tosia"))]))
            , ("expiry", Aduration (durFromStr "+13 12:00:00.000"))
            ]) []),
            (ZoneS (M.fromList
            [ ("level", Aint (Just 2))
            , ("name", Astr (Just "khaki31"))
            , ("owner", Astr (Just "/uw/khaki31"))
            , ("timestamp", Atime (Just $ timeFromStr "2012/11/09 20:03:00.000"))
            , ("freshness", Atime (Just $ timeFromStr "2000/01/01 12:00:00.000"))
            , ("contacts", Aset 0 Nothing)
            , ("cardinality", Aint (Just 1))
            , ("members", Aset 3 (Just [(Acontact (Just defContact))]))
            , ("creation", Atime (Just $ timeFromStr "2011/11/09 20:12:13.123"))
            , ("cpu_usage", Afloat Nothing)
            , ("num_cores", Aint (Just 3))
            , ("has_ups", Abool (Just False))
            , ("some_names", Alist 3 (Just [(Astr (Just "agatka")), (Astr (Just "beatka")), (Astr (Just "celina"))]))
            , ("expiry", Aduration (durFromStr "-13 11:00:00.000"))
            ]) []),
            (ZoneS (M.fromList
            [ ("level", Aint (Just 2))
            , ("name", Astr (Just "khaki13"))
            , ("owner", Astr (Just "/uw/khaki13"))
            , ("timestamp", Atime (Just $ timeFromStr "2012/11/09 21:03:00.000"))
            , ("freshness", Atime (Just $ timeFromStr "2000/01/01 12:00:00.000"))
            , ("contacts", Aset 0 Nothing)
            , ("cardinality", Aint (Just 1))
            , ("members", Aset 3 (Just [(Acontact (Just defContact))]))
            , ("creation", Atime Nothing)
            , ("cpu_usage", Afloat (Just 0.1))
            , ("num_cores", Aint Nothing)
            , ("has_ups", Abool (Just True))
            , ("some_names", Alist 3 (Just []))
            , ("expiry", Aduration Nothing)
            ]) [])
        ]),
        (ZoneS (M.fromList
        [ ("level", Aint (Just 1))
        , ("name", Astr (Just "pjwstk"))
        , ("owner", Astr (Just "/pjwstk/whatever01"))
        , ("timestamp", Atime (Just $ timeFromStr "2012/11/09 20:08:13.123"))
        , ("freshness", Atime (Just $ timeFromStr "2000/01/01 12:00:00.000"))
        , ("contacts", Aset 0 Nothing)
        , ("cardinality", Aint (Just 0))
        ]) [
            (ZoneS (M.fromList
            [ ("level", Aint (Just 2))
            , ("name", Astr (Just "whatever01"))
            , ("owner", Astr (Just "/pjwstk/whatever01"))
            , ("timestamp", Atime (Just $ timeFromStr "2012/11/09 21:12:00.000"))
            , ("freshness", Atime (Just $ timeFromStr "2000/01/01 12:00:00.000"))
            , ("contacts", Aset 0 Nothing)
            , ("cardinality", Aint (Just 1))
            , ("members", Aset 3 (Just [(Acontact (Just defContact))]))
            , ("creation", Atime (Just $ timeFromStr "2012/10/18 07:03:00.000"))
            , ("cpu_usage", Afloat (Just 0.1))
            , ("num_cores", Aint (Just 7))
            , ("php_modules", Alist 3 (Just [(Astr (Just "rewrite"))]))
            ]) []),
            (ZoneS (M.fromList
            [ ("level", Aint (Just 2))
            , ("name", Astr (Just "whatever02"))
            , ("owner", Astr (Just "/pjwstk/whatever02"))
            , ("timestamp", Atime (Just $ timeFromStr "2012/11/09 21:13:00.000"))
            , ("freshness", Atime (Just $ timeFromStr "2000/01/01 12:00:00.000"))
            , ("contacts", Aset 0 Nothing)
            , ("cardinality", Aint (Just 1))
            , ("members", Aset 3 (Just [(Acontact (Just defContact))]))
            , ("creation", Atime (Just $ timeFromStr "2012/10/18 07:04:00.000"))
            , ("cpu_usage", Afloat (Just 0.4))
            , ("num_cores", Aint (Just 13))
            , ("php_modules", Alist 3 (Just [(Astr (Just "odbc"))]))
            ]) [])
        ])
    ]
