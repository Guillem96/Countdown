module NumericExpressions where
    import ArithmeticOperators

    -- Define possible expressions as a recursive type
    data Expr = Val Int | App Op Expr Expr

    -- This expression "App Add ( Val 1) (App Mul (Val 2) (Val 3))" will show as follows:
    --"1+(2*3)"
    instance Show Expr where
        show (Val n)        = show n
        show (App o x y)    = brak x ++ show o ++ brak y
                            where
                                brak (Val n)    = show n
                                brak e          = "(" ++ show e ++ ")"

    -- Extract the values of an operation
    -- Will be using this to check if used vaules are in the given sequence
    values::Expr -> [Int]
    values (Val n) = [n]
    values (App _ x y) = values x ++ values y

    -- Evaluates an expression
    -- List of one integer if it's correct
    -- Empty list if an operation is not valid
    eval::Expr -> [Int]
    eval (Val n)        = [ n | n > 0 ]
    eval (App o x y)    = [ apply o x y |   x <- eval x,
                                            y <- eval y,
                                            valid o x y ]

    -- Declaring some expressions for testing
    expr1::Expr
    expr1 = App Add (Val 1) (App Mul (Val 2) (Val 3))

    expr2::Expr
    expr2 = App Sub (Val 2) (Val 3)
