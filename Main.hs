module Main where
    import ForceBruteSol

    main::IO()
    main = do
        -- Solving using force brute
        -- Arround 10 seconds to solve this problem
        print $ solutions 765 [1,3,7,10,25,50]
        return ()
