module Parse where


import Syntax

import Prelude
import System.IO 
import Data.Ratio
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Text.Parsec (ParsecT)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
   emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "if"
                                      , "then"
                                      , "else"
                                      , "fi"
                                      , "while"
                                      , "do"
                                      , "od"
                                      , "skip"
                                      , "true"
                                      , "false"
                                      , "~"
                                      , "&&"
                                      , "||"
                                      , "hid"
                                      , "vis"
                                      , "print"
                                      , "leak"
                                      , "observe"
                                      ]

            , Token.reservedOpNames = ["+"
                                      , "-"
                                      , "*"
                                      , "/"
                                      , "<"
                                      , ">"
                                      , "~"
                                      , ":="
                                      , "<="
                                      , ">="
                                      , "=="
                                      , "&&"
                                      , "||"
                                      ]
            }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    -- parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
brackets   = Token.brackets   lexer -- exterior choice
angles     = Token.angles     lexer -- interior choice
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
natural    = Token.natural    lexer

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement =   sequenceOfStmt -- <|> parens statement

sequenceOfStmt :: Parser Stmt
sequenceOfStmt =
  do list <- (endBy1 statement' semi)
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' =   parens statement'
           <|> eChoiceStmt
           <|> ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt
           <|> vidStmt
           <|> leakStmt

eChoiceStmt :: Parser Stmt
eChoiceStmt = 
  do expr  <- brackets expression
     stmt1 <- statement
     stmt2 <- statement
     return $ Echoice stmt1 stmt2 expr

ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- expression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     reserved "fi"
     return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- expression
     reserved "do"
     stmt <- statement
     reserved "od"
     return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- expression
     return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

vidStmt :: Parser Stmt
vidStmt = 
  do reserved "vis" 
     var <- identifier
     return $ Vis var

leakStmt :: Parser Stmt
leakStmt = 
  do reserved "leak" <|> reserved "print" <|> reserved "observe"
     expr <- expression
     return $ Leak expr

ichoiceExpr :: Parser Expr
ichoiceExpr = 
  do expr <- angles expression
     expr1 <- expression
     expr2 <- expression
     return $ Ichoice expr1 expr2 expr

decimalRat :: Monad m => ParsecT String u m Rational
decimalRat = 
  do ns <- many digit
     ms <- (char '.' >> many digit) <|> return []
     let pow10 = toInteger $ length ms
     let (Right n) = parse natural "" (ns ++ ms)
     return (n % (10 ^ pow10))

expression :: Parser Expr
expression = ichoiceExpr <|> buildExpressionParser operators term 

operators = [  [Prefix (reservedOp "-"  >> return (Neg             ))          ]
             , [Prefix (reservedOp "~"  >> return (Not             ))          ]
             , [Infix  (reservedOp "*"  >> return (ABinary Multiply)) AssocLeft,
                Infix  (reservedOp "/"  >> return (ABinary Divide  )) AssocLeft,
                Infix  (reservedOp "+"  >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOp "-"  >> return (ABinary Subtract)) AssocLeft]
             , [Infix  (reservedOp "&&" >> return (BBinary And     )) AssocLeft,
                Infix  (reservedOp "||" >> return (BBinary Or      )) AssocLeft]
             ]

term =  parens expression
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))
     <|> liftM Var identifier
     <|> liftM RationalConst decimalRat
     <|> rExpression

rExpression =
  do a1 <- expression
     op <- relation
     a2 <- expression
     return $ RBinary op a1 a2

relation =   (reservedOp ">" >> return Gt)
         <|> (reservedOp "<" >> return Lt)
         <|> (reservedOp "<=" >> return Le)
         <|> (reservedOp ">=" >> return Ge)
         <|> (reservedOp "==" >> return Eq)

-- Output only
parseString :: String -> Stmt
parseString str =
        case parse whileParser "" str of
          Left e  -> error $ show e
          Right r -> r

parseFile :: String -> IO Stmt
parseFile file =
        do program  <- readFile file
           case parse whileParser "" program of
                Left e  -> print e >> fail "parse error"
                Right r -> return r
