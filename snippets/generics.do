let head 'a array -> 'a (
    0 pick swap pop
)

let tail 'a array -> 'a array (
    len 1 swap slice
)

let main -> (
    [1 2 3] head print
    [1 2 3] tail print
    [true false] head print
    [true false] tail print
)
