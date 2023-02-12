module CarsAssemble

let productionRateBase = 221
let minutePerHour = 60

let successRate (speed: int): float =
    if speed = 0 then 0.0
    elif speed <=4 then 1.0
    elif speed <=8 then 0.9
    elif speed = 9 then 0.8
    else 0.77

let productionRatePerHour (speed: int): float =
    float ((successRate speed) * (float productionRateBase) * (float speed))

let workingItemsPerMinute (speed: int): int =
    int ((productionRatePerHour speed) / (float minutePerHour))