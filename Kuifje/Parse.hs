{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs, StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Functor.Identity

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
                                      , "|"
                                      , ","
                                      , "@"
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
braces     = Token.braces     lexer 
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
natural    = Token.natural    lexer

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement =
  do list <- (endBy1 statement' semi)
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else Seq list

stmtTmp :: Parser Stmt
stmtTmp = buildExpressionParser sOperators statement'
         <?> "Can't found"

sOperators = [[Infix (do whiteSpace
                         reservedOp "["
                         expr <- expression
                         reservedOp "]"
                         return $ \ x y -> (Echoice x y expr)
                     ) AssocLeft
               ]
            ]

statement' :: Parser Stmt
statement' = buildExpressionParser sOperators sTerm

sTerm :: Parser Stmt
sTerm = parens statement'
    <|> brackets statement
    <|> braces statement
    <|> assignStmt
    <|> ifStmt
    <|> whileStmt
    <|> skipStmt
    <|> vidStmt
    <|> leakStmt

eChoiceStmt :: Parser Stmt
eChoiceStmt = 
  do expr  <- (whiteSpace >> expression)
     reserved "|"
     list <- (endBy1 statement' (reserved "|"))
     let stmt1 = head list
     let stmt2 = head $ tail list
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
  do {-expr <- angles expression
     expr1 <- expression
     reserved "|"
     expr2 <- expression
     list <- (sepBy1 expression (reserved "|"))
     let expr1 = head list
     let expr2 = head list
     let expr = head list
     -}
     expr1 <- expression 
     reserved "|"
     expr2 <- expression
     reserved "|"
     expr <- expression
     return $ Ichoice expr1 expr2 expr

        {-
probExpr = Parser (Expr, Expr)
probExpr = do list <- (sepBy1 expression (reserved "@"))
              return $ (head list, head (tail list))
ichoiceExpr' :: Parser Expr
ichoiceExpr' =
        do list <- (sepBy1 probExpr (reserved ","))


addList ls = if length ls == 1 
                then fst (head ls)
                else Ichoice (head ls) (addList (tail ls))
                -}


-- decimalRat :: Monad m => ParsecT String u m Rational
decimalRat = 
  do ns <- many digit
     ms <- (char '.' >> many digit) <|> return []
     let pow10 = toInteger $ length ms
     let (Right n) = parse natural "" (ns ++ ms)
     return (n % (10 ^ pow10))

expression :: Parser Expr
expression = whiteSpace >> expression'

expression':: Parser Expr
expression' = (angles ichoiceExpr) 
         -- <|> (brackets ichoiceExpr') 
         <|> buildExpressionParser operators term 
         <?> "Can't found"

-- operators :: forall a. Show a => [[Operator Char st (Expr a)]]
operators = [  [Prefix (reservedOp "-"  >> return (Neg             ))          ]
             , [Prefix (reservedOp "~"  >> return (Not             ))          ]
             , [Infix  (reservedOp "*"  >> return (ABinary Multiply)) AssocLeft,
                Infix  (reservedOp "/"  >> return (ABinary Divide  )) AssocLeft,
                Infix  (reservedOp "+"  >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOp "-"  >> return (ABinary Subtract)) AssocLeft]
             , [Infix  (reservedOp "&&" >> return (BBinary And     )) AssocLeft,
                Infix  (reservedOp "||" >> return (BBinary Or      )) AssocLeft]
             , [Infix (do whiteSpace
                          reservedOp "<"
                          expr <- expression
                          reservedOp ">"
                          return $ \ x y -> ( Ichoice x y expr)
                      ) AssocLeft
               ]
             ]

term :: Parser Expr
term = parens expression
   <|> (reserved "true"  >> return (BoolConst True ))
   <|> (reserved "false" >> return (BoolConst False))
   <|> tExpression
   <|> liftM RationalConst decimalRat
   <|> rExpression

tExpression = 
  try 
  (do a1 <- (liftM Var identifier)
      op <- relation
      a2 <- expression
      return $ RBinary op a1 a2)
  <|>
  do a1 <- (liftM Var identifier)
     return a1

vExpression = 
  do a1 <- (liftM Var identifier) 
     op <- relation
     a2 <- expression 
     return $ RBinary op a1 a2

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
