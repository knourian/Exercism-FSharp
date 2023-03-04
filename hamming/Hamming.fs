module Hamming

let distance (strand1: string) (strand2: string): int option = 
    if (strand1|> Seq.length)  <> (strand2 |> Seq.length) then None
    else
        let strand = strand1 |>Seq.zip strand2
        strand 
        |> Seq.filter (fun (elem1 ,elem2 ) -> elem1 <> elem2)
        |> Seq.length
        |> Some
            