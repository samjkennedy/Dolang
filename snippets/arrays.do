let main -> (
    [1] [2] concat print
    [1 2 3] [4 5 6] [7 8] [9] concat concat concat print
    [true false] [true] concat print

    [true false] 
        (not) map 
        print

    [] (dup *) map print

    [1 2 3] dup print print

    0 10 range
        (2 % 0 =) split
    print print
)