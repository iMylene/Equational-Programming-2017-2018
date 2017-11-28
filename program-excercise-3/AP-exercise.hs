module AP where

import System.Environment
import Data.Map (Map, (!))
import qualified Data.Map as D (insert, empty)
import Data.Set
import Data.Function
import Control.Monad (mapM_)
import Control.Monad.State
import Control.Applicative ((<$>), (<*>), (<*))
import Text.Parsec hiding (State)
import Text.Parsec.Char (endOfLine)
import Text.Parsec.String (Parser)

-- | Types defining the AST
type Identifier = String
data Program    = Program [Statement] deriving (Show)
data Statement  = Assign Identifier Expression
                | Print Expression
                | Comment String deriving (Show)
data Expression = Union Expression Expression
                | SymDiff Expression Expression
                | Complement Expression Expression
                | Intersection Expression Expression
                | Identifier Identifier
                | Complex Expression
                | Set (Set Number) deriving (Show)
type Operator   = (Expression -> Expression -> Expression)
type Number     = Integer


-- | Parsers that follow the EBNF
program = undefined

assignStmnt :: Parser Statement
assignStmnt = undefined

printStmnt :: Parser Statement
printStmnt = undefined

comment :: Parser Statement
comment = undefined

identifier :: Parser Identifier
identifier = undefined

expression :: Parser Expression
expression = undefined

term :: Parser Expression
term = undefined

factor :: Parser Expression
factor = undefined

complexFactor :: Parser Expression
complexFactor = undefined

set :: Parser Expression
set = undefined

addOp :: Parser Operator
addOp = undefined

mulOp :: Parser Operator
mulOp = undefined

naturalNumber :: Parser Number
naturalNumber = undefined

positiveNumber :: Parser String
positiveNumber = undefined 

number = zero <|> notZero
zero = char '0'
notZero = oneOf "123456789"
-- letter is already defined
eoln = endOfLine
-- spaces is already defined


-- | Interpreter
type InterpreterState = StateT (Map Identifier (Set Number)) IO (Set Number)

class Eval a where
  eval :: a -> InterpreterState

instance Eval Program where
  eval (Program stmnts) = mapM_ eval stmnts >> return empty

instance Eval Statement where
  eval (Assign name expr) = eval expr
                            >>= modify . (D.insert name)
                            >> return empty
  eval (Print  expr) = eval expr
                       >>= lift . print . elems
                       >> return empty
  eval (Comment _) = return empty

execBinOp f = (liftM2 f) `on` eval -- Evaluate both sides of operator

instance Eval Expression where
  eval (Union a b) = execBinOp union a b
  eval (SymDiff a b) = execBinOp symdiff a b
    where symdiff x y = (x \\ y) `union` (y \\ x)
  eval (Complement a b) = execBinOp difference a b
  eval (Intersection a b) = execBinOp intersection a b
  eval (Identifier id) = (!id) <$> get
  eval (Complex expr) = eval expr
  eval (Set s) = return s


-- | Entrypoint: reading, parsing and executing files
main = getArgs >>= mapM_ executeFile
  where executeFile filename = do
          input <- readFile filename
          case parse program filename input of
            Left  err    -> putStrLn $ show $ err
            Right result -> runStateT (eval result) D.empty >> return ()
