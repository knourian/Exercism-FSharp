module BirdWatcher

let lastWeek: int[] = [| 0; 2; 5; 3; 7; 8; 4 |]

let yesterday(counts: int[]): int =
  let length = counts |> Array.length
  let index = length - 2
  counts|> Array.item index

let total(counts: int[]): int =
  counts|> Array.sum

let dayWithoutBirds(counts: int[]): bool =
  counts |> Array.exists (fun elem -> elem = 0)

let incrementTodaysCount(counts: int[]): int[] =
  let last = (counts |> Seq.last) + 1
  let index = (counts |> Seq.length)  - 1
  counts|> Array.updateAt index  last

let oddWeek(counts: int[]): bool =
  match counts with
  | [| |] -> false
  | [|_;0;_;0;_;0;_|] -> true
  | [|_;10;_;10;_;10;_|] -> true
  | [|5;_;5;_;5;_;5|] -> true
  | _ -> false