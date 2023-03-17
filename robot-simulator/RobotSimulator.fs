module RobotSimulator

type Direction =
    | North
    | East
    | South
    | West

type Position = int * int


let create direction position =
    match direction with
    | Direction.North -> (Direction.North, position)
    | Direction.East -> (Direction.East, position)
    | Direction.South -> (Direction.South, position)
    | Direction.West -> (Direction.West, position)

let turnLeft robot =
    let (direction, position) = robot

    match direction with
    | Direction.North -> create West position
    | Direction.East -> create North position
    | Direction.South -> create East position
    | Direction.West -> create South position

let turnRight robot =
    let (direction, position) = robot

    match direction with
    | Direction.North -> create East position
    | Direction.East -> create South position
    | Direction.South -> create West position
    | Direction.West -> create North position

let advance robot =
    let (direction, position) = robot
    let (x, y) = position

    match direction with
    | Direction.North -> create direction (x, y + 1)
    | Direction.East -> create direction (x + 1, y)
    | Direction.South -> create direction (x, y - 1)
    | Direction.West -> create direction (x - 1, y)

let doInstruction instruction robot =
    match instruction with
    | 'R' -> turnRight robot
    | 'L' -> turnLeft robot
    | 'A' -> advance robot
    | _ -> robot

let move (instructions : string) robot =
    let rec next remianInstuction robot=
        match remianInstuction with
        | "" -> robot
        | _-> 
            let head = remianInstuction.[0]
            let tail = remianInstuction.[1..]
            next tail (doInstruction head robot)
    next instructions robot
