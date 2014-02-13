module Parser(parse, parseSingle) where

import qualified Text.ParserCombinators.Parsec as P (parse)
import Text.ParserCombinators.Parsec hiding (parse)

import QAT

type MyParser st res = GenParser Char st res

wordFirstChar :: [Char]
wordFirstChar =  ['a'..'z'] ++ ['A'..'Z'] ++ "_"

wordChars :: [Char]
wordChars = ['a'..'z'] ++ ['A'..'Z'] ++ "_" ++ ['0'..'9']

reservedKeywords :: [String]
reservedKeywords =
    [ "SELECT"
    , "AS"
    , "OR"
    , "AND"
    , "NOT"
    , "REGEXP"
    , "TRUE"
    , "FALSE"
    , "WHERE"
    , "ORDER"
    , "BY"
    , "ASC"
    , "DESC"
    , "NULLS"
    , "FIRST"
    , "LAST"
    ]

mylex p = do
    result <- p
    spaces
    return result

wordParser :: MyParser st String
wordParser = do
    first <- oneOf wordFirstChar
    others <- many (oneOf wordChars)
    return $  first:others

keyword :: String -> MyParser st ()
keyword s = do
    string s
    lookAhead $ noneOf wordChars
    return ()

idParser :: MyParser st String
idParser = try $ do
    word <- wordParser
    if word `elem` reservedKeywords
        then unexpected $ "reserved keyword: \"" ++ word ++ "\""
        else return word

topParser :: MyParser st [(String, QAT)]
topParser = do
    spaces
    r <- namedQueryParser `sepBy` (mylex $ char ';')
    eof
    return r

namedQueryParser :: MyParser st (String, QAT)
namedQueryParser = do
    char '&'
    n <- mylex idParser 
    mylex $ char ':'
    q <- queryParser
    return ('&':n, q)

selListParser = selItemParser `sepBy` (mylex $ char ',')
selAnonListParser = selAnonItemParser `sepBy` (mylex $ char ',')

selAnonItemParser = eParser

selItemParser :: MyParser st Qsel
selItemParser = do
    sel_expr <- mylex eParser
    mylex $ keyword "AS"
    as <- idParser
    return $ Qsel sel_expr as

{---------------------}
{- EXPRESSIONS BEGIN -}
{---------------------}

-- eAbs[A]Parser nextParser opParserL constr
-- Use nextParser to parse sequence of expressions, separated by operators
-- recognized by members of opParserL.
-- Assumption: every operator parser returns a constant value

-- When there is only one operator parser, we can ignore its value
eAbsParser nextParser [opParser] constr = do
    eL <- (mylex nextParser) `sepBy1` (mylex opParser)
    case eL of
        [e] -> return e
        _ -> return $ constr eL

-- When there are more operator parsers, the result will be more complex:
-- We associate an operator with every expression except for the first one
eAbsAParser nextParser opParserL constr = do
    eFirst <- mylex nextParser
    eL <- many $ do
        op <- mylex $ choice opParserL
        e <- mylex nextParser
        return (op, e)
    case eL of
        [] -> return eFirst
        _ -> return $ constr eFirst eL

eParser :: MyParser st Qexpr
eParser = eOrParser
eOrParser = eAbsParser eAndParser [try $ keyword "OR"] Eor
eAndParser = eAbsParser eNotParser [try $ keyword "AND"] Eand
eNotParser = try ( do
    mylex $ keyword "NOT"
    e <- eNotParser
    return (Enot e) ) <|> eRelParser
eRelParser = do
    e1 <- mylex eAddParser
    opMbe <- mylex $ optionMaybe $ choice [
        try $ string "<=" >> return Rle,
        try $ string "<" >> return Rlt,
        try $ string ">=" >> return Rge,
        try $ string ">" >> return Rgt,
        try $ string "==" >> return Req,
        try $ string "!=" >> return Rne ]
    case opMbe of
        Nothing -> do
            rexpMbe <- mylex $ optionMaybe $ try $ keyword "REGEXP"
            case rexpMbe of
                Nothing -> return e1
                Just _ -> do
                    Estr str <- mylex eStrParser
                    return $ Erexp e1 str
        Just op -> do
            e2 <- mylex eAddParser
            return $ Erel op e1 e2
eAddParser = eAbsAParser eMulParser [
    (char '+') >> return OpAdd,
    (char '-') >> return OpSub] Eadd
eMulParser = eAbsAParser eUnaryParser [
    (char '*') >> return OpMul,
    (char '/') >> return OpDiv,
    (char '%') >> return OpMod ] Emul
eUnaryParser = ( do
       mylex $ char '-'
       e <- eBasicParser
       return $ Eneg e )
   <|> eBasicParser
eBasicParser =
    eStrParser
    <|> (try eFloatParser)
    <|> eIntParser
    <|> try (keyword "TRUE" >> return Etrue)
    <|> try (keyword "FALSE" >> return Efalse)
    <|> ( do
        mylex $ char '('
        res <- (try nestedParser >>= return . Equery) <|> eParser
        spaces
        char ')'
        return res
        )
    <|> ( do
        i <- mylex idParser
        args <- optionMaybe $ do
            mylex $ char '('
            res <- mylex eListParser
            char ')'
            return res
        case args of
            Nothing -> return $ Evar i
            Just xs -> return $ Eapp i xs
        )
    <?> "Terminal expression"
eStrParser = do
    char '"'
    str <- many (noneOf "\\\"" <|> (char '\\' >> anyChar))
    char '"'
    return $ Estr str
eFloatParser = do
    Eint i1 <- eIntParser
    char '.'
    Eint i2 <- eIntParser
    return $ Efloat ((fromIntegral i1) + (fromIntegral i2)/(fromIntegral $ denom i2))
    where
      denom x = denom' x 10
      denom' x y = if y > x
                    then y
                    else denom' x (y*10)
eIntParser = do
    str <- many1 (oneOf ['0'..'9'])
    let i = (read str) :: Int
    return $ Eint i
eListParser = (mylex eParser) `sepBy` (mylex $ char ',')

{-------------------}
{- EXPRESSIONS END -}
{-------------------}

whereParser = do
    mylex $ keyword "WHERE"
    mylex eParser
orderParser = do
    mylex $ keyword "ORDER BY"
    (mylex orderItemParser) `sepBy` (mylex $ char ',')
orderItemParser = do
    e <- mylex eParser
    ordM <- mylex $ optionMaybe (
            (try $ keyword "ASC" >> return Oasc)
            <|> (try $ keyword "DESC" >> return Odesc)
            )
    nullM <- mylex $ optionMaybe (
             (try $ keyword "NULLS FIRST" >> return Onfirst)
             <|> ( try $ keyword "NULLS LAST" >> return Onlast)
             )
    let ord = case ordM of
                Nothing -> Oasc
                Just x -> x
    let null = case nullM of
                Nothing -> Onlast
                Just x -> x
    return $ Qorder e ord null

absQParser selsParser qConstr = do
    mylex $ keyword "SELECT"
    sel_list <- mylex $ selsParser
    mbe_where <- mylex $ optionMaybe $ try whereParser
    mbe_order <- mylex $ optionMaybe $ try orderParser
    let orders = case mbe_order of
                    Nothing -> []
                    Just xs -> xs
    return $ qConstr sel_list mbe_where orders

nestedParser :: MyParser st Qnested
nestedParser = absQParser selAnonItemParser Qnested

queryParser :: MyParser st QAT
queryParser = absQParser selListParser QAT

parse :: String -> Either ParseError [(String, QAT)]
parse qText = P.parse topParser "(stdin)" qText

parseSingle :: String -> Either ParseError QAT
parseSingle qText = P.parse queryParser "" qText
