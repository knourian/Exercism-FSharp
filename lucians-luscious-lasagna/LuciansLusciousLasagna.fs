module LuciansLusciousLasagna

let expectedMinutesInOven = 40
let layerPreparationTime = 2

let remainingMinutesInOven elapsedTime = expectedMinutesInOven - elapsedTime

let preparationTimeInMinutes layerNum = layerPreparationTime * layerNum

let elapsedTimeInMinutes layerNum elapsedTime = preparationTimeInMinutes layerNum + elapsedTime
