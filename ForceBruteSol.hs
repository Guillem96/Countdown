module ForceBruteSol where
    import Combinatorial
    import NumericExpressions
    import ArithmeticOperators

    -- Calculating the solution with force brute
    -- Split the solution in all possible ways
    split::[a] -> [([a],[a])]
    split [] = []
    split [_] = []
    split (x:xs) = ([x],xs) : [ (x:ls, rs) | (ls, rs) <- split xs ]

    -- Calculate all possible expressions
    operators::[Op]
    operators = [Add, Mul, Sub, Div]

    -- Calculate all possible operations
    exprs::[Int] -> [Expr]
    exprs [] = []
    exprs [x] = [Val x]
    exprs xs = [ e | (ls, rs)   <- split xs
                        , x     <- exprs ls
                        , y     <- exprs rs
                        , e     <- combine x y
                        ]

    -- Combine 2 expressions with all possible operators
    combine::Expr -> Expr -> [Expr]
    combine x y = [ App o x y | o <- operators ]


    -- Returns all possible solutions
    solutions::Int -> [Int] -> [Expr]
    solutions x xs = [ e | xs' <- choices xs, e <- exprs xs', eval e == [x]]
