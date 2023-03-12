module CollatzConjecture

let isEven(number : int) = number % 2 = 0

let steps (number: int): int option = 
    let rec compute(n:int) (steps:int) : int =
        if( n = 1 ) then steps
        else
            if (isEven n) 
                then compute (n/2) (steps + 1)
            else 
                compute (3 * n + 1) (steps + 1)
    if number <= 0 then None
    else 
        Some (compute number 0)