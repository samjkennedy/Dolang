let mapp list: 'a array func: ('a -> 'b) -> 'b array (
    list func map
)

let main -> (
    [true false] (2 *) 
        mapp 
    print
)