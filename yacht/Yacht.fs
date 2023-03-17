module Yacht

type Category = 
    | Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | FullHouse
    | FourOfAKind
    | LittleStraight
    | BigStraight
    | Choice
    | Yacht

type Die =
    | One 
    | Two 
    | Three
    | Four 
    | Five 
    | Six


let dieValue die  = 
    match die with
    | One -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6


let scoreByCount (dice :Die list) (dieNumber : Die)= 
    let count = dice |> Seq.filter(fun elem->elem = dieNumber )  |> Seq.length
    match dieNumber with
    | One -> count
    | Two -> 2 * count
    | Three -> 3 * count
    | Four -> 4 * count
    | Five -> 5 * count
    | Six -> 6 * count

let fullHouseScore (dice : Die list)=
    let groups = dice |> Seq.countBy(fun x->x)
    
    if (groups |> Seq.length <> 2) then 0
    else
        let (a,countA) = groups |> Seq.head
        let (b,countB) = groups |> Seq.last
        if countA = 3 || countB = 3 then int(countA) * (dieValue a) + int(countB) * (dieValue b)
        else 0

let choiceScore (dice : Die list) =
    dice |> Seq.sumBy(fun elem -> dieValue elem)

let yachtScore (dice : Die list) =
    let groups = dice |> Seq.countBy(fun x->x)

    if groups |> Seq.length <> 1 then 0
    else 50

let fourOfKindScore (dice : Die list) = 
    let groups = dice |> Seq.countBy(fun x->x)

    if groups |> Seq.length = 1 then 
        let (a,countA) = groups |> Seq.head
        if countA = 5 then 4 * dieValue a
        else 0
    elif groups |> Seq.length <> 2 then 0
    else 
        let (a,countA) = groups |> Seq.head
        let (b,countB) = groups |> Seq.last
        if countA >= 4 then 4 * dieValue a
        elif countB >= 4 then 4 * dieValue b
        else 0

let littleStraightScore (dice : Die list) =
    let groups = dice |> Seq.countBy(fun x->x)

    if  groups |> Seq.length <> 5 then 0
    else
        let min = dice |> Seq.minBy(fun elem -> dieValue elem)
        let max = dice |> Seq.maxBy(fun elem -> dieValue elem)

        if min = Die.One && max = Die.Five then 30
        else 0


let bigStraightScore (dice : Die list) =
    let groups = dice |> Seq.countBy(fun x->x)

    if  groups |> Seq.length <> 5 then 0
    else
        let min = dice |> Seq.minBy(fun elem -> dieValue elem)
        let max = dice |> Seq.maxBy(fun elem -> dieValue elem)

        if min = Die.Two && max = Die.Six then 30
        else 0



let score (category : Category) (dice : Die list)= 
    match category with
    | Ones -> scoreByCount dice One
    | Twos -> scoreByCount dice Two
    | Threes -> scoreByCount dice Three
    | Fours -> scoreByCount dice Four
    | Fives -> scoreByCount dice Five
    | Sixes -> scoreByCount dice Six
    | FullHouse -> fullHouseScore dice
    | Choice -> choiceScore dice
    | FourOfAKind -> fourOfKindScore dice
    | LittleStraight -> littleStraightScore dice
    | BigStraight -> bigStraightScore dice
    | Yacht -> yachtScore dice