module PizzaPricing
type Pizza = 
    | Margherita 
    | Caprese
    | Formaggio
    | ExtraSauce of Pizza
    | ExtraToppings of Pizza

let pizzaPrice (pizza: Pizza): int = 
    let rec calculate pizza total =
        match pizza with
        | Margherita -> 7 + total
        | Caprese -> 9 + total
        | Formaggio -> 10 + total
        | ExtraSauce p -> calculate p (1 + total)
        | ExtraToppings p -> calculate p (2 + total)
    calculate pizza 0

let orderPrice(pizzas: Pizza list): int = 
    let rec calcPrice list total = 
        match list with
        | [] -> total
        | head :: tail -> calcPrice tail (total + (pizzaPrice head))
    
    let price = calcPrice pizzas 0
    
    match List.length pizzas  with
        | 1 -> 3 + price
        | 2 -> 2 + price
        | _ -> price