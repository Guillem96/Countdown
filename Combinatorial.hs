module Combinatorial where
    -- This module has useful methods that return all combinations
    -- of a a certain list that stisfy certain properties.

    -- Return all possible subsequences from a list of numbers
    subs::[a] -> [[a]]
    subs [] = [[]]
    subs (x:xs) = yss ++ map (x:) yss where
        yss = subs xs

    -- Return all possible ways of introduce an element in a list
    interleave::a -> [a] -> [[a]]
    interleave x  [] = [[x]]
    interleave y (x:xs) = (y:x:xs) : (map (x:) $ interleave y xs)

    -- Return all possible permutations
    permutations::[a] -> [[a]]
    permutations [] = [[]]
    permutations (x:xs) = concat $ map (interleave x) $ permutations xs

    -- Gives all possible list made with a sequence of numbers
    choices::[a] -> [[a]]
    choices = concat . map permutations . subs -- all permutations of all subsequences
