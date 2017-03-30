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

    type Result = (Expr, Int)

    -- Calculate all possible operations
    results::[Int] -> [Result]
    results [] = []
    results [x] = [(Val x, x)]
    results xs = [ e | (ls, rs) <- split xs
                        , x     <- results ls
                        , y     <- results rs
                        , e     <- combine x y
                        ]

    -- Combine 2 expressions with all possible operators
    combine::Result -> Result -> [Result]
    combine (x, rx) (y, ry) = [ (App o x y, apply o rx ry) | o <- operators, valid o rx ry ]


    -- Returns all possible solutions
    solutions::Int -> [Int] -> [Expr]
    solutions x xs = [ e | xs' <- choices xs, (e,r) <- results xs', r == x ]
