let any bool array -> bool (
    false (or) fold
)

let uppercase c: char -> bool (
    c int cast 'A' int cast >=
    c int cast 'Z' int cast <=
    and
)

let main -> (
    "Hello " "world" concat print
    "Hello"
        (print) foreach

    "Hello" 
        ('e' =) map 
    any print

    "Hello" len print pop

    "Characters On uppercase OnLy"
        (uppercase) filter
        print
)