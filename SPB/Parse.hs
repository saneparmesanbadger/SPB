{-# LANGUAGE OverloadedStrings #-}
module SPB.Parse where

import qualified Data.Text as T

import Control.Monad (liftM)
import Text.Parsec hiding (spaces)
import Text.Parsec.String (Parser (..))
import Text.Parsec.Combinator
import Text.Parsec.Token hiding (symbol)
import Text.Parsec.Language (haskellDef)
import System.Environment
import Debug.Trace (trace)
import SPB.Base
import SPB.Interpreter

lexer = makeTokenParser haskellDef
lexeme' = lexeme lexer

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser CSExp
parseString = do char '"'
                 x <- many chs
                 char '"'
                 pos <- fmap sourceLine $ getPosition
                 return $ 
                   (\x -> (Context pos, SE $ [c0 $ SAtom $ ASymbol $ T.pack "quote", x])) $
                   (Context pos, SE $ map (c0 . SAtom . AChar) x)
--Character replacement code from: 
--  http://codereview.stackexchange.com/questions/2406/parsing-strings-with-escaped-characters-using-parsec                   
  where
    chs = escaped <|> noneOf "\""
    escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
    escapedChar code replacement = char code >> return replacement
    codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
    replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']
    
parseAtom :: Parser CSExp
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               pos <- fmap sourceLine $ getPosition
               return $ (Context pos, SAtom $ ASymbol $ T.pack atom)
                 
parseNumber :: Parser CSExp
parseNumber = fmap (c0 . SAtom . AInt . read) $ do
  char1 <- choice [char '-', digit]
  chars <- case char1 of
    '-' -> many1 digit
    _ -> many digit
  return $ char1:chars

parseFloat :: Parser CSExp
parseFloat = do
  char1 <- choice [char '-', digit]
  d1 <- case char1 of
    '-' -> many1 digit
    _ -> many digit
  char '.'
  d2 <- many1 digit
  return . c0 . SAtom . AFloating $ ( (read $ [char1] ++ d1 ++ "." ++ d2) :: Double)

parseList = do
  pos <- fmap sourceLine $ getPosition
  --lexeme' $ parens lexer $ 
  char '('  
  optional spaces
  z <- fmap (\x -> (Context pos, SE x)) $ lexeme' $ sepEndBy parseExpr spaces
  char ')'
  return z
  
parseQuoted :: Parser CSExp
parseQuoted = do
  char '\''
  x <- parseExpr
  pos <- fmap sourceLine $ getPosition
  return $ (Context pos, SE $ [c0 $ SAtom $ ASymbol $ T.pack "quote", x])
    
parseBare :: Parser CSExp
parseBare = do
  str <- string "(bare"
  chars <- many chs --manyTill anyChar (try inner)
  string ")"
  pos <- fmap sourceLine $ getPosition
  return $ (Context pos, SE [c0 $ SAtom $ ASymbol "quote"
                              , c0 $ SE $ map (c0 . SAtom . AChar) chars])
    where
      chs = transform <|> noneOf ")"
      transform = do
        c <- string "\\)"
        return ')'

parseComment :: Parser CSExp
parseComment = do
  char ';'
  many (noneOf "\n")
  char '\n'
  optional (whiteSpace lexer)
  parseExpr

parseLambda :: Parser CSExp
parseLambda = do
  string "(\\"
  optional spaces
  pos <- fmap sourceLine $ getPosition
  args <- parseList
  optional spaces
  body <- parseExpr
  char ')'
  let z =  (Context pos, SE $ [c0 $ SAtom $ ASymbol $ T.pack "lambda", args, body])
  return $ 
    trace (T.unpack (prettyPrint z)) z

parseExpr = parseExpr'

parseExpr' :: Parser CSExp
parseExpr' = 
  parseComment
  <|> try parseFloat
  <|> try parseNumber
  <|> (parseAtom <?> "Parse Atom broekd")
  <|> parseString
  <|> try parseQuoted
  <|> try parseLambda
  <|> try parseBare
  <|> try parseList

--         <|> try parseString
