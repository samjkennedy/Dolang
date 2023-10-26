let divisible int int -> bool (% 0 =)

let fizzbuzz int -> (
    dup dup 
    3 divisible swap 5 divisible and 
        ("FizzBuzz" print)
        (
            dup 3 divisible 
                ("Fizz" print) 
                (
                    dup 5 divisible 
                        ("Buzz" print) 
                        (print)
                    if
                )
            if
        )
    if
)

let main -> (
    [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30]
        (fizzbuzz) foreach
)