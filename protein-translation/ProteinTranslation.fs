module ProteinTranslation

let codons (nucleotide:string) = 
    match nucleotide with 
    | "AUG" -> "Methionine"
    | "UUU" | "UUC" -> "Phenylalanine"
    | "UUA" | "UUG" -> "Leucine"
    | "UCU" | "UCC" | "UCA" | "UCG" -> "Serine"
    | "UAU" | "UAC" -> "Tyrosine"
    | "UGU" | "UGC" -> "Cysteine"
    | "UGG" -> "Tryptophan"
    | "UAA" | "UAG" | "UGA" -> "Stop"
    | _ -> failwith "Invalid codon"

let proteins( rna:string) = 
    let rec next (remianRna : string) (result: string list)=
        match remianRna with
        | x when x.Length < 3 -> result
        | _-> 
            let head = remianRna.[0..2]
            let cod = codons head
            if cod = "Stop" then result
            else 
                let tail = remianRna.[3..]
                next tail (result @ [cod])
    next rna list.Empty