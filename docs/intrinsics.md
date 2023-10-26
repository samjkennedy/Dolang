# Intrinsics 

These are all the built in operations of Loquat

## Stack manipulation

Functions that perform some manipulation to the values on the stack.

| Name | Signature    | Description                                              |
|------|--------------|----------------------------------------------------------|
| dup  | a -> a a     | Duplicates the top of the stack.                         |
| pop  | a ->         | Pops the top of the stack.                               |
| swap | a b -> b a   | Swaps the top two elements of the stack.                 |
| over | a b -> a b a | Copies the element below the top of the stack.           |
| rot  | a b c -> b c a |Moves the element 2 below to the top of the stack.           |
| .    | a -> a       | Identity. Pops the top stack element and pushes it back. |

## Arithmetic

| Name | Signature          | Description                                    |
|------|--------------------|------------------------------------------------|
| +    | int int -> int     | Sums the top two elements on the stack.        |
| -    | int int -> int     | Subtracts the second element from the first.   |
| *    | int int -> int     | Multiplies the top two elements on the stack.  |
| /    | int int -> int     | Divides the top element by the second.         |
| %    | int int -> int     | Computes the remainder of the division.        |

## Comparisons

| Name | Signature             | Description                                               |
|------|-----------------------|-----------------------------------------------------------|
| >    | int int -> bool       | Checks if the second element is greater than the first.   |
| <    | int int -> bool       | Checks if the second element is less than the first.  |
| >=   | int int -> bool       | Checks if the first element is greater than or equal to the second. |
| <=   | int int -> bool       | Checks if the second element is less than or equal to than the first. |
| =    | int int -> bool       | Checks if the two elements are equal.                     |
| not  | bool -> bool          | Negates the boolean value.                                |
| and  | bool bool -> bool     | Returns true if both elements are true, otherwise false.   |
| or   | bool bool -> bool     | Returns true if at least one element is true, otherwise false. |


## Higher order functions

Functions that take functions as their arguments to perform some operation over an array.

| Name    | Signature                | Description                                                                                |
|---------|--------------------------|--------------------------------------------------------------------------------------------|
| filter  | [a] fn(a -> bool) -> [a] | Filters the array, keeping elements that match the given condition.                        |
| fold    | [a] a fn(a a -> a) -> a  | Reduces an array into a single value using an accumulator and a function.                  |
| foreach | [a] fn(a -> ) ->         | Applies a function to every element of an array.                                           |
| map     | [a] fn(a -> b) -> [b]    | Applies a function to each element in the array, returning a new array of mapped elements. |
| call    | a.. fn(a... -> b...) -> b... | Calls the function on the top of the stack, consuming its arguments and placing its returns on the stack. |
| split    | [a] fn(a -> bool) -> [a] [a] | Splits the list based on the predicate into two lists, with those passing on top. |

## Array functions

Functions that perform operations on arrays

| Name   | Signature          | Description                                                                                                          |
|--------|--------------------|----------------------------------------------------------------------------------------------------------------------|
| concat | [a] [a] -> [a]     | Concatenates two arrays, popping them off the stack.                                                                 |
| len    | [a] -> [a] int         | Pushes the length (number of elements) of an array onto the stack, popping the array.                                |
| pick   | [a] int -> [a] a       | Pushes the element of the array given by the index on the top of the stack.                                          |
| slice  | [a] int int -> [a] | Slices an array, pushing a new array given by the indices on top of the stack. These are in the order <start> <end>. |
| zip    | [a] [a] -> [[a]]   | Joins two arrays elementwise into pairs, the arrays must have the same length.                                       |

## Control flow

Functions that influence the control flow of the program

| Name         | Signature                              | Description                                                                                                                                      |
|--------------|----------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------|
| if           | bool fn(a... -> b...) fn(a... -> b...) | Runs one of the two functions on top of the stack based on the value of the bool underneath. If true executes the bottom function, else the top. |
| return | n/a                       | Exits the given function with whatever is on the stack                                                                                                      |
| <identifier> | fn(a... -> b...)                       | Calls the function given by the identifier.                                                                                                      |