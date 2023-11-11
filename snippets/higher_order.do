let applytwice 'in ('in -> 'out) -> 'out (
    dup rot swap call
    swap call
)
let square int -> int (dup *)
let negate bool -> bool (not)

let mapp 'a array ('a -> 'b) -> 'b array (
    map
)

let main -> (
    5 (square) applytwice print

    0 11 range
        ((square) applytwice) mapp
        print

    [true false true false]
        ((negate) applytwice) mapp
        print
)