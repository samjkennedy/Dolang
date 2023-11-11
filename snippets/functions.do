let square int -> int (dup *)
let even int -> bool (2 % 0 =)
let odd int -> bool (even not)
let inc int -> int (1 +)

let sum int array -> int (
    0 (+) fold
)

let all bool array -> bool (
    true (and) fold
)
let any bool array -> bool (
    false (or) fold
)
let none bool array -> bool (
    any not
)

let main -> (

    [1 2 3 4]
        (square) map
        (inc) map
        (even) filter
        sum
        print

    [true true] all print
    [true false] all print
    [true false] any print
    [false false] any print
    [false false] none print

    [2 4 6 8]
        (even) map
        all 
    print

    [2 4 6 8 11]
        (square) map
        (odd) map
        none 
    print
)
