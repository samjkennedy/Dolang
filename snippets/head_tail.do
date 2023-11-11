let head int array -> int (
    0 pick swap pop
)

let tail int array -> int array (
    dup len 1 swap slice
)

let uncons int array -> int array int (
    dup head swap tail swap
)

let main -> (
    [1 2 3] uncons print print
)