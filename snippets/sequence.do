let main -> (
    [1 2 3 4 5 6 7 8 9 10] 
        (dup *) map 
        (1 +) map
        print

    [1 2 3 4 5 6 7 8 9 10]
        (dup *) map
        (2 % 0 =) filter
        (print) foreach

    [1 2 3 4 5]
        0 (+) fold
        print
)