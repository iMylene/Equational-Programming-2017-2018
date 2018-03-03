module Practicum3 where

{-
Name:           Mylène Martodihardjo
VU-net id:      mmo440
Student number: 2509676
Discussed with: 
Remarks:        
Sources:        
-}

-- Exercises Arithmetical Expressions
data IntExp  = Lit Int | Add IntExp IntExp | Mul IntExp IntExp
  deriving Show

showintexp :: IntExp -> String
showintexp (Lit x)   = show x
showintexp (Add x y) = "(" ++ showintexp x ++ "+" ++ showintexp y ++ ")"
showintexp (Mul x y) = "(" ++ showintexp x ++ "*" ++ showintexp y ++ ")"

evalintexp :: IntExp -> Int
evalintexp (Lit x)   = x
evalintexp (Add x y) = evalintexp x + evalintexp y
evalintexp (Mul x y) = evalintexp x * evalintexp y

-- Exercises Combinatory Logic
data Term = S | K | I | App Term Term

instance Show Term where
  show a = showterm a

showterm :: Term -> String
showterm S = "S"
showterm K = "K"
showterm I = "I"
showterm (App x y) = "(" ++ showterm x ++ showterm y ++ ")"

isredex :: Term -> Bool
isredex t = case t of
  (App I x)                 -> True
  (App (App K x) y)         -> True
  (App (App (App S x) y) z) -> True
  otherwise                 -> False

isnormalform :: Term -> Bool
isnormalform t = case (isredex t) of
  True  -> False
  False -> case t of
             I -> True
             K -> True
             S -> True
             (App K x)         -> isnormalform x
             (App S x)         -> isnormalform x
             (App (App S x) y) -> (isnormalform (App S x)) && (isnormalform y)
             otherwise         -> False

headstep :: Term -> Term
headstep t = case (isredex t) of
  False -> t
  True  -> case t of
             (App I x)                 -> x
             (App (App K x) y)         -> x
             (App (App (App S x) y) z) -> (App (App x z) (App y z))

-- Exercises Equational Specifications
data Thing = C | D | W | X
  deriving (Show, Eq, Bounded, Enum)

nxt :: Thing -> Thing
nxt t = case t of
  C -> W
  W -> D
  D -> D
  X -> X

-- 
data I = Undefined2
  deriving (Show, Eq, Bounded, Enum)

s :: I -> I
s = undefined

p :: I -> I
p = undefined

