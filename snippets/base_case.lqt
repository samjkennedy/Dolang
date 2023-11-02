let fib 0 -> int (0)
let fib 1 -> int (1)
let fib int -> int (
    dup 1 - fib swap 2 - fib +
)

let factorial 0 -> int (1)
let factorial int -> int (
    dup 1 - factorial *
)

let main -> (
    0 20 range
        (fib) map
        print

    0 8 range
        (factorial) map
        print
)