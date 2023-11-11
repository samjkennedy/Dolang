let avg int array -> int (
    len swap
    0 (+) fold swap
    /
)

let first int array -> int array int (
    0 pick
)
let last int array -> int array int (
    len 1 - pick
)

let main -> (
    [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20] 
        dup avg dup print pick print
        first print
        last print
        pop
)