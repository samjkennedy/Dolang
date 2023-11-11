let fib 0 -> int (0)
let fib 1 -> int (1)
let fib int -> int (
    dup 1 - fib swap 2 - fib +
)


let main -> (
    [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19]
        (fib) map
    print
)