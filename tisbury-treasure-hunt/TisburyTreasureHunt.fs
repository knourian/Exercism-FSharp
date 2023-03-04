module TisburyTreasureHunt

let inline charToInt c = int c - int '0'

let getCoordinate (line: string * string): string =
    let (treasure, coordinate) = line
    coordinate

let convertCoordinate (coordinate: string): int * char = 
    (coordinate[0] |> charToInt ,coordinate[1])

let compareRecords (azarasData: string * string) (ruisData: string * (int * char) * string) : bool = 
    let (treasure, coordinate2) = azarasData
    let (location, coordinate, quadrant) = ruisData
    convertCoordinate coordinate2 = coordinate


let createRecord (azarasData: string * string) (ruisData: string * (int * char) * string) : (string * string * string * string) =
    let (treasure, coordinate2) = azarasData
    let (location, coordinate, quadrant) = ruisData

    if compareRecords azarasData ruisData then (coordinate2,location,quadrant,treasure)
    else ("","","","")
