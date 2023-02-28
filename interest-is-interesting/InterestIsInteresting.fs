module InterestIsInteresting

let interestRate (balance: decimal): single =
    match balance with
    | x when x < 0m -> 3.213f
    | x when x < 1000m -> 0.5f
    | x when x < 5000m -> 1.621f
    | _ -> 2.475f

let interest (balance: decimal): decimal =
   balance * (decimal)(interestRate balance) / 100m

let annualBalanceUpdate(balance: decimal): decimal =
   balance + interest balance

let amountToDonate(balance: decimal) (taxFreePercentage: float): int =
   match balance with
   | x when x <=0m -> 0
   |_ -> (int)( (float)balance *  (taxFreePercentage / (float)100 * (float)2))