module GuessingGame

let reply (guess: int): string = 
    match guess with
    | 42 -> "Correct"
    | i when i = 41 || i = 43 -> "So close"
    | i when i < 41 -> "Too low"
    | i when i > 43 -> "Too high"
    | _ -> ""
