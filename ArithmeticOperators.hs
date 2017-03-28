module ArithmeticOperators where

    -- Define all possible operations and make them showable
    data Op = Add | Mul | Sub | Div
    instance Show Op where
        show Add = "+"
        show Sub = "-"
        show Mul = "*"
        show Div = "/"

    -- Check if an operation with 2 values is valid
    -- Example if x - y < 0 is not valid
    valid::Op -> Int -> Int -> Bool
    valid Add _ _ = True
    valid Mul _ _ = True
    valid Sub x y = x > y
    valid Div x y = x `mod` y == 0

    -- Apply the operations
    apply::Op -> Int ->Int -> Int
    apply Add x y = x + y
    apply Mul x y = x * y
    apply Sub x y = x - y
    apply Div x y = x `div` y
