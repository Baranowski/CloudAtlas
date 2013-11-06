module Parser(parse) where

import qualified Text.ParserCombinators.Parsec as P (parse)
import Text.ParserCombinators.Parsec hiding (parse)

import QAT

type MyParser st res = GenParser Char st res

wordFirstChar =  ['a'..'z'] ++ ['A'..'Z'] ++ "_"
wordChars = ['a'..'z'] ++ ['A'..'Z'] ++ "_" ++ ['0'..'9']
reservedKeywords =
    [ "SELECT"
    , "AS"
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
    r <- namedQueryParser `sepBy` (char ';')
    eof
    return r

namedQueryParser :: MyParser st (String, QAT)
namedQueryParser = do
    char '&'
    n <- mylex idParser 
    mylex $ char ':'
    q <- queryParser
    return ('&':n, q)

selListParser = selItemParser `sepBy` (char ',' >> spaces)

selItemParser :: MyParser st Qsel
selItemParser = 
    sel_expr <- mylex eParser
    mbe_as <- (try ( do
        mylex $ keyword "AS"
        i <- mylex idParser
        return $ Just i )
     <|> return Nothing)
    return Qsel sel_expr mbe_as

{---------------------}
{- EXPRESSIONS BEGIN -}
{---------------------}

-- exprAbs[A]Parser nextParser opParserL constr
-- Use nextParser to parse sequence of expressions, separated by operators
-- recognized by members of opParserL.
-- Assumption: every operator parser returns a constant value

-- When there is only one operator parser, we can ignore its value
exprAbsParser nextParser [opParser] constr = do
    eL <- (mylex nextParser) `sepBy1` (mylex opParser)
    case eL of
        [e] -> return e
        _ -> return $ constr eL

-- When there are more operator parsers, the result will be more complex:
-- We associate an operator with every expression except for the first one
exprAbsAParser nextParser opParserL constr = do
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
exprRelParser = do
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
                    str <- mylex eStrParser
                    return $ Erexp e1 str
        Just op -> do
            e2 <- mylex eAddParser
            return $ Erel op e1 e2
eAddParser = exprAbsAParser eMulParser [
    (char '+') >> return OpAdd,
    (char '-') >> return OpSub] Eadd
eMulParser = exprAbsAParser eUnaryParser [
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
        res <- (try queryParser >>= return . Equery) <|> eParser
        spaces
        char ')'
        return res
        )
    <|> do
        mylex $ char '<'
        res <- mylex eListParser
        char '>'
        return Elist res
    <|> try (string "{}" >> return Ebraces)
    <|> try (string "[]" >> return Esquare)
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
    return Estr str
eFloatParser = do
    Eint i1 <- eIntParser
    char '.'
    Eint i2 <- eIntParser
    return $ Efloat (i1 + (i2 + 0.0)/(denom i2))
eIntParser = do
    str <- many1 (oneOf ['0'..'9'])
    let i = (read str) :: Int
    return $ Eint i
eListParser = (mylex eParser) `sepBy` (mylex $ char ',')

{-------------------}
{- EXPRESSIONS END -}
{-------------------}

queryParser :: MyParser st QAT
queryParser = do
    mylex $ keyword "SELECT"
    sel_list <- mylex $ selListParser
    mbe_where <- mylex $ optionMaybe $ try whereParser
    mbe_order <- mylex $ optionMaybe $ try orderParser
    return $ QAT sel_list mbe_where mbe_order

parse :: String -> Either ParseError [(String, QAT)]
parse qText = P.parse topParser "(stdin)" qText
