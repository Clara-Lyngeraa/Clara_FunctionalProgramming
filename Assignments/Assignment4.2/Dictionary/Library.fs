module Dictionary

type Dictionary = Dict of List<string>

let empty (u: unit) = Dict List.empty

let insert s (Dict(dict)) = Dict(s :: dict)
let lookup s (Dict(dict)) = List.contains(s) dict

printfn "%A"  (empty ())
printfn "%A" (lookup "HELLO" (empty () |> insert "HELLO"))