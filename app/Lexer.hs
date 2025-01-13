module Lexer (
    lexify, lexifyIndividual
) where

import Types
import Text.Regex

import Data.Maybe

skipSpace :: String -> String
skipSpace (' ':xs) = skipSpace xs
skipSpace ('\t':xs) = skipSpace xs
skipSpace xs = xs

regexHelper :: String -> String -> Maybe (String, String)
regexHelper pat str = do
    (_, x, xs, _) <- matchRegexAll (mkRegexWithOpts ('^': pat) False True) str
    return (x, skipSpace xs)

printLexer :: Lexer
printLexer str = do 
    (_, xs) <- regexHelper "PRINT" str
    return (TPrint, xs)

ifLexer :: Lexer
ifLexer str = do
    (_, xs) <- regexHelper "IF" str
    return (TIf, xs)

thenLexer :: Lexer
thenLexer str = do
    (_, xs) <- regexHelper "THEN" str
    return (TThen, xs)

elseifLexer :: Lexer
elseifLexer str = do
    (_, xs) <- regexHelper "ELSEIF" str
    return (TElseIf, xs)

endifLexer :: Lexer
endifLexer str = do
    (_, xs) <- regexHelper "ENDIF" str
    return (TEndIf, xs)

elseLexer :: Lexer
elseLexer str = do
    (_, xs) <- regexHelper "ELSE" str
    return (TElse, xs)

whileLexer str = do
    (_, xs) <- regexHelper "WHILE" str
    return (TWhile, xs)

doLexer str = do
    (_, xs) <- regexHelper "DO" str
    return (TDo, xs)

endwhileLexer str = do
    (_, xs) <- regexHelper "ENDWHILE" str
    return (TEndWhile, xs)

keywordLexer :: Lexer
keywordLexer = ifLexer <|> thenLexer <|> elseifLexer <|> endifLexer <|> elseLexer
    <|> whileLexer <|> doLexer <|> endwhileLexer

numLitLexer :: Lexer 
numLitLexer str = do
    ( x, xs) <- regexHelper "[-]?[0-9]+" str
    return (TIntLit (read x :: Integer), xs)

removeQuotes :: String -> String
removeQuotes ('\"':xs) = removeBackQuotes xs
removeQuotes _ = error "Invalid for removeQuotes"

removeBackQuotes :: String -> String
removeBackQuotes ['\"'] = []
removeBackQuotes (x:xs) = x : removeBackQuotes xs
removeBackQuotes _ = error "Invalid for removeBackQuotes"

stringLitLexer :: Lexer
-- stringLitLexer str = do
--     (x, xs) <- regexHelper "\"([^\"]*(\\\")*)*\"" str
--     return (TStringLit (removeQuotes x), xs)
stringLitLexer ('\"':xs) = tillClosingQuote xs ""
stringLitLexer _ = Nothing 

tillClosingQuote :: String -> String -> Maybe (Token, String)
tillClosingQuote [] _ = Nothing
tillClosingQuote ('\"':xs) result = Just (TStringLit (reverse result), skipSpace xs)
tillClosingQuote ('\\':'\"':xs) result = tillClosingQuote xs result
tillClosingQuote (x:xs) result = tillClosingQuote xs (x:result)

symbolLexer :: Lexer
symbolLexer str = do
    (x, xs) <- regexHelper "[a-zA-Z][a-zA-Z0-9]*" str
    return (TSymbol x, xs)

newlineLexer :: Lexer
newlineLexer ('\n':xs) = Just (TNewline, skipSpace xs)
newlineLexer _ = Nothing

sepLexer :: Lexer
sepLexer (',':xs) = Just (TCommSep, skipSpace xs)
sepLexer (';':xs) = Just (TSemiSep, skipSpace xs)
sepLexer _ = Nothing

opLexer :: Lexer
opLexer ('+':xs) = Just (TAdd, skipSpace xs)
opLexer ('-':xs) = Just (TSub, skipSpace xs)
opLexer ('*':xs) = Just (TMul, skipSpace xs)
opLexer ('/':xs) = Just (TDiv, skipSpace xs)
opLexer ('<':'=':xs) = Just (TLe, skipSpace xs)
opLexer ('>':'=':xs) = Just (TLe, skipSpace xs)
opLexer ('<':xs) = Just (TLt, skipSpace xs)
opLexer ('>':xs) = Just (TGt, skipSpace xs)
opLexer ('=':xs) = Just (TEq, skipSpace xs)
opLexer _ = Nothing

parenLexer :: Lexer
parenLexer ('^':'(':xs) = Just (TOpenParen, skipSpace xs)
parenLexer ('^':')':xs) = Just (TClosedParen, skipSpace xs)
parenLexer _ = Nothing

(<|>) :: Lexer -> Lexer -> Lexer
(<|>) l1 l2 str = do
    let res1 = l1 str
    if isNothing res1 then l2 str else res1

lexifyIndividual :: Lexer
lexifyIndividual = keywordLexer <|> printLexer <|> numLitLexer <|> sepLexer <|> stringLitLexer 
    <|> symbolLexer <|> opLexer <|> parenLexer <|> newlineLexer 

lexify :: String -> [Types.Token]
lexify [] = []
lexify str = case lexifyIndividual str of 
    Nothing -> error $ "Error parsing \"" ++ str ++ "\""
    Just (x, xs) -> x : lexify xs
