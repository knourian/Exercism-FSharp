module ProteinTranslation

let codons (nucleotide:string) = 
    match nucleotide with 
    | "" -> ""
    | "AUG" -> "Methionine"
    | "UUU" -> "Phenylalanine"
    | "UUC" -> "Phenylalanine"
    | "UUA" -> "Leucine"
    | "UUG" -> "Leucine"
    | "UCU" -> "Serine"
    | "UCC" -> "Serine"
    | "UCA" -> "Serine" 
    | "UCG" -> "Serine" 
    | "UAU" -> "Tyrosine"
    | "UAC" -> "Tyrosine"
    | "UGU" -> "Cysteine"
    | "UGC" -> "Cysteine"
    | "UGG" -> "Tryptophan"
    | "UAA" -> "Stop"
    | "UAG" -> "Stop"
    | "UGA" -> "Stop"
    | _ -> ""





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