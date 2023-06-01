open System
open Exam2021_2

let testQ1 () =
    (* Testsfor Q1.1 *)
    printfn "Testing Question 1"
    (*
    printfn "%A" (length Nil)
    printfn "%A" (length (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Nil))))))
    *)

    ()

let testQ2 () =
    printfn "Testing Question 2"
    // place debug prints for Q2 here
    
    printfn "%A" (foo [1;3] [2;4;5])
    printfn "%A" (fooTail [1;3] [2;4;5])
    
    ()

let testQ3 () =
    printfn "Testing Question 3"
    
    (*printfn "%A" (approxSquare 5 0)
    printfn "%A" (approxSquare 5 1)
    printfn "%A" (approxSquare 5 2)
    printfn "%A" (quadratic 5 (-4) (-1) 1)
    printfn "%A" (quadratic 5 (-3) (-1) 1)
    printfn "%A" (quadratic 5 (-3) (-1) 2)
    printfn "%A" (quadratic 5 (-3) (-1) 3)*)
    (*printfn "%A" ([1..10] |> List.map (fun x -> (x, -(x + 1), -(x + 2))) |>
           fun eqs -> parQuadratic eqs 3 5)
    
    printfn "%A" (gcd 15 10)
    printfn "%A" (mkRat 5 6 |> Option.get |> ratToString)
    printfn "%A" (mkRat -15 10 |> Option.get |> ratToString)
    printfn "%A" (mkRat 15 -10 |> Option.get |> ratToString)
    printfn "%A" (mkRat -15 -10 |> Option.get |> ratToString)
    printfn "%A" (mkRat 0 5 |> Option.get |> ratToString)
    printfn "%A" (mkRat 5 0)*)
    
    (*let r1 = mkRat 2 3 |> Option.get
    let r2 = mkRat 3 4 |> Option.get
    printfn "%A" (plus r1 r2 |> Option.get |> ratToString)
    printfn "%A" (minus r1 r2 |> Option.get |> ratToString)
    printfn "%A" (minus r2 r2 |> Option.get |> ratToString)
    printfn "%A" (mult r1 r2 |> Option.get |> ratToString)
    printfn "%A" (div r1 r2 |> Option.get |> ratToString)
    printfn "%A" (div r1 (minus r2 r2 |> Option.get))*)
    
    let r1 = mkRat 2 3 |> Option.get
    let r2 = mkRat 3 4 |> Option.get
    
    printfn "%A" (r1 |> evalSM (smPlus r2) |> Option.get |> snd |> ratToString)
    printfn "%A" (r1 |> evalSM (smMinus r2) |> Option.get |> snd |> ratToString)
    printfn "%A" (r1 |> evalSM (smMult r2) |> Option.get |> snd |> ratToString)
    printfn "%A" (r1 |> evalSM (smDiv r2) |> Option.get |> snd |> ratToString)
    // place debug prints for Q3 here
    ()

let testQ4 () =
    printfn "Testing Question 4"
    // place debug prints for Q4 here
    ()

[<EntryPoint>]
let main argv =
    testQ3 ()
    0 // return an integer exit code
