let fib int -> int (
    {
        0: (0)
        1: (1)
        _: (dup 1 - fib swap 2 - fib +)
    } match
)

let main -> (
    [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19]
        (fib) map
    print
)