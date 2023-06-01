module MultiSet

type MultiSet<'a when 'a : comparison>  = MS of Map<'a, uint32> 

let empty = MS Map.empty

let isEmpty (MS(s)) = Map.isEmpty(s)

let size (MS(s)) = Map.fold (fun acc key value -> acc + value ) 0u s 

let contains a (MS(s))= Map.containsKey(a) s

let numItems a (MS(s)) = 
    match s.TryFind(a) with
    | Some s -> s
    | None -> 0u

let add a n (MS(s)) =
    match s.TryFind(a) with
    | None -> MS(s.Add(a,n))
    | Some x when x > 0u -> s.Add(a,x+n) |> MS
    | _ -> MS s

// s.Add(a,n) |> MS

let addSingle a ms = add a 1u ms

let remove a n (MS(s) as ms) = 
    let x = numItems a ms
    if x > n 
    then MS(s.Add(a, x-n))
    else s.Remove(a) |> MS

let removeSingle a (MS(s) as ms) = remove a 1u ms

let fold f acc (MS(s) as ms) = 
    Map.fold f acc s

let foldBack f (MS(s)) acc = 
    Map.foldBack f s acc

let ofList lst = List.fold (fun acc key  ->  addSingle key acc) empty lst

let rec help (k, v) = 
    match v with
    | 0u -> []
    | _ -> k :: help (k, (v-1u))

let toList (MS(s) as ms) = 
    fold (fun acc key value -> acc @ help (key, value) ) [] ms

let map f (MS(s) as ms) = 
    fold (fun acc key value  ->  add (f key) value acc) empty ms
   
let union (MS s1 as ms1) (MS s2 as ms2) =
    fold (fun acc key value ->
        if contains key ms2
        then
            if numItems key ms1 > numItems key ms2
            then add key (numItems key ms1) acc
            else add key (numItems key ms2) acc
        else acc) empty ms1

let sum (MS s1 as ms1) (MS s2 as ms2) =
    fold (fun acc key value ->
        if contains key ms2
        then add key (value+ (numItems key ms2)) acc
        else acc
            ) empty ms1

let subtract (MS s1 as ms1) (MS s2 as ms2) = 
  fold (fun acc key value -> remove key value acc) ms1 ms2

let intersection (MS s1 as ms1) (MS s2 as ms2) =
    fold (fun acc key value ->
        match numItems key ms1 with
        | 0u -> acc
        | s1value when s1value < value  -> add key s1value acc
        | _ -> add key value acc
        ) empty ms2
