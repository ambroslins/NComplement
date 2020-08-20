module Expression where

data Expression
  = Natural Integer
  | Neg Expression
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  deriving (Show)

eval :: Expression -> Integer
eval e = case e of
  Natural x -> x
  Neg x -> - (eval x)
  Add x y -> eval x + eval y
  Sub x y -> eval x - eval y
  Mul x y -> eval x * eval y
  Div x y -> eval x `div` eval y
