open System
open Exam2021

let testQ1() =
    (* Testsfor Q1.1 *)

    
    (*
    printfn "%A" (move 10 North (C (0, 0)))
    printfn "%A" (turnRight North)
    printfn "%A" (turnLeft North)*)
    
    (*printfn "%A" (step (P (C (0, 0), North)) TurnRight)
    printfn "%A" (step (P (C (0, 0), North)) TurnLeft)
    printfn "%A" (step (P (C (0, 0), North)) (Forward 10))*)
    (*printfn "%A" (walk (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft])
    printfn "%A" (walk2 (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft])*)
    (*printfn "%A" (path (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft])
    printfn "%A" (path2 (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft])
    printfn "%A" (path3 (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft])
    printfn "%A" (path (P (C (0, 0), North))
       [Forward 5; TurnRight; Forward 5; TurnRight;
        Forward 5; TurnRight; Forward 5])
    printfn "%A" (path2 (P (C (0, 0), North))
       [Forward 5; TurnRight; Forward 5; TurnRight;
        Forward 5; TurnRight; Forward 5])
    printfn "%A" (path3 (P (C (0, 0), North))
       [Forward 5; TurnRight; Forward 5; TurnRight;
        Forward 5; TurnRight; Forward 5])*)
    

    ()

let testQ2() =
    // place debug prints for Q2 here
    
    printfn "%A" (baz 40)
    printfn "%A" (barbaz 40)
    ()

let testQ3 () =
    // place debug prints for Q3 here
    (*printfn "%A" (elToString [1;1;1;3;2;2;1])
    printfn "%A" (elFromString "1113221")
    printfn "%A" (elFromString "1113221")*)
    (*printfn "%A" ("1" |> elFromString |> nextElement |> elToString)
    printfn "%A" ("1" |> elFromString |> nextElement |> nextElement |> elToString)
    printfn "%A" ("1" |> elFromString |> nextElement |> nextElement |>
         nextElement |> nextElement |> nextElement |> elToString)
    printfn "%A" ("11111111112222222" |> elFromString |> nextElement |> elToString)*)
    ()

let testQ4 () =
    (*printfn "%A" (ringToList (ringFromList [1;2;3;4;5]))
    printfn "%A" (length (ringFromList [1;2;3;4;5]))
    printfn "%A" (ringToList (ringFromList [1;2;3;4;5]))*)
    printfn "%A" (ringToList (empty : int ring))
    printfn "%A" ([1;2;3;4;5] |> ringFromList |> push 6 |> ringToList)
    printfn "%A" ([1;2;3;4;5] |> ringFromList |> peek)
    printfn "%A" (([] : int list) |> ringFromList |> peek)
    printfn "%A" ("---")
    printfn "%A" ([1;2;3;4;5] |> ringFromList |> ccw |> ccw |> ringToList)
    printfn "%A" ([1;2;3;4;5] |> ringFromList |> cw |> cw |> ringToList)
    // place debug prints for Q4 here
    ()

[<EntryPoint>]
let main argv =
    testQ4 ()
    0 // return an integer exit code
