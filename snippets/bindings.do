let main -> (
    4 5
    set a b (
        a print
        b print
        a b + print
    )

    (dup *)
    set square (
        5 square do print
        6 square do print
    )

    6 true
    set a b (
        a print
        b print
        a 4 + print
        b true and print
    )

    1 
    set a (
        a print
        2 a +
        set b (
            a print
            b print
        )
        a print
    )
)