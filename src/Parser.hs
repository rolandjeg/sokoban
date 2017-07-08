module Parser where

import Data.Char
import Data.Maybe

data Parser a = P (String -> Maybe (a,String))

runParser :: Parser a -> String -> Maybe (a,String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p input = case runParser p input of
                  Just (result, "") -> Just result
                  _                 -> Nothing

noParser :: Parser a
noParser = P (\_ -> Nothing)

pureParser :: a -> Parser a
pureParser x  = P (\input -> Just (x,input))

instance Functor Parser where
    fmap f p = P p' where
        p' input = case runParser p input of
                     Just (result, rest) -> Just (f result, rest)
                     Nothing             -> Nothing

instance Applicative Parser where
    pure = pureParser
    fp <*> fx = P go where
        go input = case runParser fp input of
                     Just (function ,rest) -> case runParser fx rest of
                                                Just (arg,rest') -> Just (function arg,rest')
                                                Nothing          -> Nothing

                     Nothing               -> Nothing 

instance Monad Parser where
    return = pureParser
    fa >>= k = P $ \input -> do
        (x,rest) <- runParser fa input
        runParser (k x) rest

anyChar :: Parser Char
anyChar = P go where
    go "" = Nothing
    go (x:xs) = Just (x,xs)

char :: Char -> Parser ()
char c = do
    c1 <- anyChar
    if c == c1 then return ()
               else noParser

anyCharBut :: Char -> Parser Char
anyCharBut c = do
    c1 <- anyChar
    if c /= c1 then return c1
               else noParser

anyCharButL :: [Char] -> Parser Char
anyCharButL c = do
    c1 <- anyChar
    if c1 `elem` c then noParser
                   else return c1

alphaNum :: Parser Char
alphaNum = do
    c <- anyChar
    if isAlphaNum c 
       then return c
       else noParser

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = P go where 
    go input = case runParser p1 input of 
                 Just r -> Just r
                 Nothing -> runParser p2 input

many :: Parser a -> Parser [a]
many p = ((:) <$> p <*> many p) `orElse` return []

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p1 p2 = ((:) <$> p1 <*> (many (p2 >> p1))) `orElse` return []

parseCSV :: Parser [[String]]
parseCSV = many parseLine
    where
        parseLine = parseCell `sepBy` char ',' <* char '\n'
        parseCell = do
            char '"'
            content <- many (anyCharBut '"')
            char '"'
            return content

type Identifier = String
type Declaration = (Identifier, String)
type Section = (Identifier, [Declaration])
type INIFile = [Section]

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

parseINI :: Parser INIFile
parseINI = many parseSection where
    parseSection :: Parser Section
    parseSection = do
        x <- parseSectionHeader
        y <- many parseLine
        return (x,catMaybes y)

    parseSectionHeader :: Parser Identifier
    parseSectionHeader = do
        _ <- many (char ' ')
        char '[' 
        x <- parseIdent
        char ']' 
        _ <- many (char ' ')
        char '\n'
        return x


    parseLine :: Parser (Maybe Declaration)
    parseLine = parseDecl `orElse` parseCommentLine `orElse` parseEmptyLine

    parseDecl :: Parser (Maybe Declaration)
    parseDecl = do
        _ <- many (char ' ')
        x <- parseIdent
        _ <- many (char ' ')
        _ <- char '='
        _ <- many (char ' ')
        y <- parseString
        return (Just (x,y))

    parseIdent :: Parser Identifier
    parseIdent = many1 alphaNum

    parseString = do 
        x <- many1 (anyCharButL ['\n', '#'])
        char '\n' `orElse` parseComment
        return x

    parseCommentLine = do
        parseComment
        return Nothing

    parseComment :: Parser ()
    parseComment = do
        _ <- many (char ' ')
        char '#'
        _ <- many (anyCharBut '\n')
        char '\n'
        return ()

    parseEmptyLine = do
        parseEmpty
        return Nothing

    parseEmpty :: Parser ()
    parseEmpty = do
        _ <- many (char ' ')
        char '\n'
        return ()
