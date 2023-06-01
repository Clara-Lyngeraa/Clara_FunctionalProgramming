module Exam2020

    open System

(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile work again.

   Do not remove the line (even though that does work) because you may inadvertantly
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the line as is, but load ExamInteractive.fsx into the interactive environment
   *)
(* module Exam2020 = *)

(* 1: Insertion sort *)

(* Question 1.1 *)

    let rec insert e lst =
        match lst with
        | [] -> [e]
        | x :: xs when e <= x -> e::x::xs
        | x :: xs -> x :: insert e xs
            
    let rec insertionSort lst =
        match lst with
        | [] -> []
        | x :: xs -> insert x (insertionSort xs)
    
(* Question 1.2 *)

    let insertTail e lst =
        let rec aux list acc =
            match list with
            | [] -> e::acc
            | x :: xs when e <= x -> (List.rev <| e::x::xs) @ acc
            | x :: xs -> aux xs (x::acc)
        aux lst [] |> List.rev
        
    let rec insertionSortTail lst =
        let rec aux lst acc =
            match lst with
           | [] -> acc
           | x :: xs -> aux xs (insertTail x acc)
        aux lst []

(* Question 1.3 *)

    (* 
    Q: Why are the higher-order functions from the List library 
    not a good fit to implement insert?

    A: 
    When we insert we want to break the function two times (we have two base cases)
    higher order functions does not support this. They do not support stopping the iteration
    You can return in the middle of a fold
    *)

    let insertionSort2 lst =
        List.fold (fun acc elem -> insertTail elem acc) [] lst

(* Question 1.4 *)

    let rec insertBy f e lst =
       let rec aux list acc =
            match list with
            | [] -> e::acc
            | x :: xs when f e <= f x ->  (List.rev <| e::x::xs) @ acc
            | x :: xs -> aux xs (x::acc)
       aux lst [] |> List.rev
       
    let rec insertionSortBy f lst =
        let rec aux lst acc =
            match lst with
           | [] -> acc
           | x :: xs -> aux xs (insertBy f x acc)
        aux lst []

(* 2: Code Comprehension *)
    let rec foo x = 
        function 
        | y :: ys when x = y -> ys
        | y :: ys            -> y :: (foo x ys)

    let rec bar x =
        function
        | []        -> []
        | xs :: xss -> (x :: xs) :: bar x xss 

    let rec baz =
        function
        | [] -> []
        | [x] -> [[x]]
        | xs  -> 
            let rec aux =
                function
                | []      -> []
                | y :: ys -> ((foo y >> baz >> bar y) xs) @ (aux ys)
            aux xs

    let a y = foo y >> baz >> bar y
(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo,  bar, and baz?

    A: 
    foo: 'a -> 'a list -> 'a list
    bar: 'a -> 'a list list -> 'a list list
    baz: 'a list -> 'a list list 


    Q: What do functions foo, bar, and baz do? 
       Focus on what they do rather than how they do it.

    A: 
    foo: removes any item from the given list that is equal to x
    bar: Adds x to every list in the list
    baz: 


    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: 
    
    foo: removeFromList //removeSingle
    bar: addFirst //prependOnList
    baz: //getCombinations
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The function foo generates a warning during compilation: 
    Warning: Incomplete pattern matches on this expression.

    
    Q: Why does this happen, and where? 

    A: 
    This happen because we have a match expression that f# is not sure 
    will be completed. Because there is no [] base case or 
    a match statement that picks up the rest of the possible matches


    Q: For these particular three functions will this incomplete 
       pattern match ever cause problems for any possible execution of baz? 
       If yes, why; if no, why not.

    A: 
    no, because baz makes the check if the given list is empty, 

    *)

    let rec foo2 x = 
        function
        | [] -> []
        | y :: ys when x = y -> ys
        | y :: ys            -> y :: (foo x ys)

(* Question 2.3 *) 

    (* 
    In the function baz there is a sub expression foo y >> baz >> bar y

    Q: What is the type of this expression

    A: 'a -> 'a list -> 'a list list


    Q: What does it do? Focus on what it does rather than how it does it.

    A: It removes y, then recursivly scramples the list using baz, and then 
    adds y in the beginngen again
    
    //It takes an input parameter, and finds all combinations of the list, starting with
    this parameter. The list MUST contain the parameter. It removes the first occurence
    of the parameter in the list, and places the parameter at the front of the list.
    

    *)

(* Question 2.4 *)

    let bar2 x lst =
        List.fold (fun acc elem -> (x :: elem) :: acc) [] lst |> List.rev
    
    let bar3 x lst =
        List.map (fun f -> x :: f) lst


(* Question 2.5 *)

        (*
        let rec baz =
        function
        | [] -> []
        | [x] -> [[x]]
        | xs  -> 
            let rec aux =
                function
                | []      -> []
                | y :: ys -> ((foo y >> baz >> bar y) xs) @ (aux ys)
            aux xs*)
            
    (*
    let baz2 lst =
        match lst with
            | [] -> []
            | [x] -> [[x]]
            | xs  ->
                List.fold (fun acc elem -> (foo elem >> baz >> bar elem)xs @ acc) [] xs
                |> List.rev*)
    let rec baz2 =
        function
        | [] -> []
        | [x] -> [[x]]
        | xs  -> List.collect (fun y -> (foo y >> baz >> bar y) xs) xs 
(* Question 2.6 *)

    (*
    
    Q: The function foo is not tail recursive. Why?
    
    A:
    foo 2 [1;2;3]
    1 :: foo 3 [2;3]
    1:: (2 :: foo 1 [3])
    1 :: (2 :: ([]))
    1 :: [2]
    [1;2]
        
    
      let rec foo x = 
        function 
        | y :: ys when x = y -> ys
        | y :: ys            -> y :: (foo x ys)
    *)

    let fooTail x lst =
        let rec aux xs c =
            match xs with
            | [] -> c []
            | y :: ys when x = y -> c ys
            | y :: ys -> aux ys (fun f -> c (y :: f))
        aux lst id

(* 3: Rock Paper Scissors *)

(* Question 3.1 *)

    type shape =
        | Rock
        | Paper
        | Scissors
        
    type result =
        | PlayerOneWin
        | PlayerTwoWin
        | Draw
    
    let mkShape s =
        match s with
        | "rock" -> Rock
        | "paper" -> Paper
        | "scissors" -> Scissors
        | s          -> failwith (sprintf "invalid shape: %s" s) 

    let shapeToString =
       function
       | Rock     -> "rock"
       | Paper   -> "paper"
       | Scissors -> "scissors"

    let resultToString =
        function
        | PlayerOneWin -> "playerOneWin"
        | PlayerTwoWin -> "playerTwoWin"
        | Draw        -> "draw"

    let rps s1 s2 =
        match s1, s2 with
        | Rock, Paper | Paper, Scissors | Scissors, Rock   -> PlayerTwoWin
        | Rock, Scissors | Paper, Rock | Scissors, Paper -> PlayerOneWin
        | Rock, Rock | _,_ -> Draw
        | Paper, Rock -> PlayerOneWin
        | Paper, Scissors -> PlayerTwoWin
        | Paper, Paper -> Draw
        | Scissors, Rock -> PlayerTwoWin
        | Scissors, Paper -> PlayerOneWin
        | Scissors, Scissors -> Draw

(* Question 3.2 *)

    type strategy = (shape * shape) list -> shape

    let parrot s moves =
        match moves with
        | [] -> s
        | (s1,s2) :: xs -> s2
     
    let beatingStrat moves =
        let oppMoves = List.map snd moves
        let mostRock = List.filter (fun x -> x = Rock) oppMoves |> List.length
        let mostPaper = List.filter (fun x -> x = Paper) oppMoves |> List.length
        let mostScissors = List.filter (fun x -> x = Scissors) oppMoves |> List.length
        
        if mostScissors >= mostPaper && mostScissors >= mostRock
            then Rock
        else if mostRock >= mostPaper && mostRock >= mostScissors
            then Paper
        else Scissors

    let roundRobin shapes moves =
        let mutable temp = shapes
        match temp with
        | [] ->
            temp <- shapes
        | x :: xs ->
            temp <- xs
            x

(* Question 3.3 *)

    (* 
    
    Q: It may be tempting to generate a function that calculates your 
       point tuple after n rounds and then use Seq.initInfinite to 
       generate the sequence. This is not a good solution. Why?

    A: <Your answer goes here>
    
    *)

    let bestOutOf strat1 strat2 = failwith "not implemented"

(* Question 3.4 *)

    let playTournament numRounds players =
        let rec firstRound acc =
            function
                | [] -> (acc, [])
                | [x] -> (acc, [x])
                | x :: y :: xs -> firstRound ((x,y)::acc) xs
        
        let rec aux =
            function
                | [] -> None
                | [(_, id)] -> Some id
                | players ->
                    let (pairs, rest) = firstRound [] players
                    pairs |>
                    List.map
                        (fun ((p1, id1), (p2, id2)) ->
                            async {
                                let (p1win, p2win) = bestOutOf p1 p2 |> Seq.item numRounds
                                return
                                    if p1win = p2win
                                        then None
                                    elif p1win > p2win
                                        then Some (p1, id1)
                                    else
                                        Some (p2, id2)
                            }) |>
                        Async.Parallel |>
                        Async.RunSynchronously |>
                        Array.toList |>
                        List.filter Option.isSome |>
                        List.map Option.get |>
                        (fun lst -> aux (lst @ rest))
        aux (List.mapi (fun i x -> (x, i)) players)

(* 4: Revers Polish Notation *)

(* Question 4.1 *)

    type stack = int list

    let emptyStack : stack = List.empty<int>

(* Question 4.2 *)

    type SM<'a> = S of (stack -> ('a * stack) option)

    let ret x = S (fun s -> Some (x, s))
    let fail  = S (fun _ -> None)
    let bind f (S a) : SM<'b> = 
        S (fun s -> 
            match a s with 
            | Some (x, s') -> 
                let (S g) = f x             
                g s'
            | None -> None)
        
    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)

    let evalSM (S f) = f emptyStack 

    let push x = S (fun stack -> Some ((), x::stack))
    let pop  = S (fun stack ->
        match stack with
            | [] -> None
            | x :: xs -> Some (x, xs))

(* Question 4.3 *)

    let write str : SM<unit> = S (fun s -> printf "%s" str; Some ((), s))

    let read =
        let rec aux acc =
            match System.Console.Read() |> char with
            | '\n' when acc = [] -> None
            | c    when System.Char.IsWhiteSpace c -> 
                acc |> List.fold (fun strAcc ch -> (string ch) + strAcc) "" |> Some
            | c -> aux (c :: acc)

        S (fun s -> Some (aux [], s))

    (* 
    
    Q: Consider the definition of write There is a reason that the definition 
       is S (fun s -> printf "%s" str; Some ((), s)) and not just 
       ret (printf "%s" str). For a similar reason, in read, we write 
       S (fun s -> Some (aux [], s)) and not ret (aux []). 
       What is the problem with using ret in both of these cases?
    
    A: 
    In railway programming we have to return Some or None, we therefore can't 
    just return the values we need to return, we need to wrap it in an option type
    
    // Write: With a function like printfn, which is a function with the side effect of
    printing to the console, we want to make sure that it is run when we want it to
    run. For this reason we’re sequencing the function call with returning the
    SM<unit>.
    
    Read: Here it is again important that the order in which we read, is correct. If
    two read functions are called after each other, it is important that it is in that
    order which they read the input
    *)

(* Question 4.4 *)

    (* You may solve this exercise either using monadic operators or 
        using computational expressions. *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()
    
    let isInt (str : string) : bool = System.Int32.TryParse str |> fst

    let calculateRPN () =
        let rec aux () =
            read >>= (fun s ->
                match s with
                | Some "+" -> pop >>= fun x ->
                              pop >>= fun y ->
                                 (x+y) |> push
                                 >>>= aux ()
                | Some "*" -> pop >>= fun x ->
                              pop >>= fun y ->
                                 (x*y) |> push
                                 >>>= aux ()
                | Some "-" -> pop >>= fun x ->
                              pop >>= fun y ->
                                 (x-y) |> push
                                 >>>= aux ()
                | Some str when isInt str -> push (int str)
                                             >>>= aux ()
                | Some _ -> fail
                | None -> pop >>= fun x -> string x |> write
                 )
        aux ()