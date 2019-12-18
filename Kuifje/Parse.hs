module ParseWhile where


import System.IO 
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


data BExpr = BoolConst Bool
           | Not BExpr 
           | BBinary BBinOp BExpr BExpr 
           | RBinary RBinOp AExpr AExpr 
           deriving (Show)

data BBinOp = And 
            | Or 
            deriving (Show)

data RBinOp = Gt
            | Ge
            | Lt
            | Le
            | Eq
            deriving (Show)

data AExpr = Var String 
           | IntConst Integer 
           | Neg AExpr 
           | ABinary ABinOp AExpr AExpr 
           | Ichoice AExpr AExpr AExpr
           deriving (Show)

data Expr = AExpr | BExpr deriving (Show)

data ABinOp = Add 
            | Subtract 
            | Multiply 
            | Divide 
            deriving (Show)

data Stmt = Seq [Stmt] 
          | SVar String
          | Assign String AExpr 
          | If BExpr Stmt Stmt 
          | While BExpr Stmt 
          | Skip 
          | Leak AExpr
          | Vis String
          | Echoice Stmt Stmt AExpr
          deriving (Show)

languageDef =
   emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "var"
                                      , "if"
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
            , Token.reservedOpNames = ["+", "-", "*", "/", ":=", "<=", ">="
                                      , "==", "<", ">", "&&", "||", "~"
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
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace


whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement =   sequenceOfStmt -- <|> parens statement

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
           <|> varStmt
           <|> vidStmt
           <|> leakStmt

eChoiceStmt :: Parser Stmt
eChoiceStmt = 
  do expr  <- brackets aExpression
     stmt1 <- statement
     stmt2 <- statement
     return $ Echoice stmt1 stmt2 expr

ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- bExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     reserved "fi"
     return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- bExpression
     reserved "do"
     stmt <- statement
     reserved "od"
     return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

varStmt :: Parser Stmt
varStmt = 
  do reserved "var" <|> reserved "hid"
     var <- identifier
     return $ SVar var

vidStmt :: Parser Stmt
vidStmt = 
  do reserved "vis" 
     var <- identifier
     return $ Vis var

leakStmt :: Parser Stmt
leakStmt = 
  do reserved "leak" <|> reserved "print" <|> reserved "observe"
     expr <- aExpression
     return $ Leak expr

ichoiceExpr :: Parser AExpr     
ichoiceExpr = 
  do expr <- angles aExpression
     expr1 <- aExpression
     expr2 <- aExpression
     return $ Ichoice expr1 expr2 expr


aExpression :: Parser AExpr
aExpression = ichoiceExpr <|> buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
             , [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft,
                Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft,
                Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft]
              ]

bOperators = [ [Prefix (reservedOp "~" >> return (Not             ))          ]
             , [Infix  (reservedOp "&&" >> return (BBinary And     )) AssocLeft,
                Infix  (reservedOp "||"  >> return (BBinary Or      )) AssocLeft]
             ]

aTerm =  parens aExpression
     <|> liftM Var identifier
     <|> liftM IntConst integer

bTerm =  parens bExpression
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))
     <|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
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
