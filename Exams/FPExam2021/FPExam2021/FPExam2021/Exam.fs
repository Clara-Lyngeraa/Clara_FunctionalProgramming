module Exam2021

    open System.Text.RegularExpressions

(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertantly
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2021 = 
 *)

(* 1: Dungeon crawler *)

(* Question 1.1 *)

    type direction = North | East | South | West
    type coord     = C of int * int

    let move dist dir (coord: coord) =
        match coord with
        | C (x,y) when dir = North -> C (x,y-dist)
        | C (x,y) when dir = South -> C (x,y+dist)
        | C (x,y) when dir = West -> C (x-dist,y)
        | C (x,y) when dir = East -> C (x+dist,y)
            
            
    let turnRight dir =
        match dir with
        | North -> East
        | East -> South
        | South -> West
        | West -> North
        
    let turnLeft dir =
        match dir with
        | North -> West
        | West -> South
        | South -> East
        | East -> North
    
(* Question 1.2 *)

    type position = P of (coord * direction)
    type move     = TurnLeft | TurnRight | Forward of int

    let step p m =
        match p, m with
        | P (coor,dir), TurnRight -> P (coor, turnRight dir)
        | P (coor, dir), TurnLeft -> P (coor, turnLeft dir)
        | P (coor, dir), Forward i -> P (move i dir coor, dir)

(* Question 1.3 *)


    let rec walk p ms =
        match ms with
        | [] -> p
        | m :: ms' -> walk (step p m) ms'
        
    let walk2 p ms =
        List.fold (fun acc elem -> step acc elem) p ms

(* Question 1.4 *)

    let rec path p ms =
        match ms, p with
        | [], P(coord, dir) -> [coord]
        | m :: ms', P(coord, dir) when m <> TurnLeft && m <> TurnRight -> coord :: path (step p m) ms'
        | m :: ms',_ -> path (step p m) ms'

(* Question 1.5 *)

    let path2 p ms =
        let rec aux p ms acc =
            match p, ms with
            | P(coord, dir), [] -> (coord::acc) |> List.rev
            | P(coord, dir), m :: ms' when m <> TurnLeft && m <> TurnRight -> aux (step p m) ms' (coord::acc)
            | _, m::ms' -> aux (step p m) ms' acc      
        aux p ms []

(* Question 1.6 *)

(* Q: Your solution for `path` is not tail recursive. Why? To make a compelling
      argument you should evaluate a function call of the function, similarly to
      what is done in Chapter 1.4 of HR, and reason about that evaluation.
      You need to make clear what aspects of the evaluation tell you that the
      function is not tail recursive. Keep in mind that all steps in an evaluation
      chain must evaluate to the same value
      (```(5 + 4) * 3 --> 9 * 3 --> 27```, for instance).

 let rec path p ms =
        match ms, p with
        | [], P(coord, dir) -> [coord]
        | m :: ms', P(coord, dir) when m <> TurnLeft && m <> TurnRight -> coord :: path (step p m) ms'
        | m :: ms',_ -> path (step p m) ms'
        
   A: 
   path (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft]
   
    path (step (P (C (0, 0), North) TurnRight) [Forward 10; TurnLeft]->
    path (P (C (0, 0), East) [Forward 10; TurnLeft]->
    C(0,0) :: path (step (P (C (0, 0), East) Forward 10) [TurnLeft] ->
    C(0,0) :: path (P (C (10, 0), East) [TurnLeft] ->
    C(0, 0) :: path (P (C (10, 0), North) [] ->
    C(0, 0) :: [C (10, 0)] ->
    [C(0,0);C(10,0)]
    
    As shown above the function doesn't evaluate until the base case is met,
    path is not tail recursive as the elements are appended to a list
    that depends on the recursive call. Until the recursive call is evaluated
    there is no tail to append the elements to.
    They get stored in a stack which can eventually overflow
    if the list is big enough
    
    
 let rec path p ms =
        match ms, p with
        | [], P(coord, dir) -> [coord]
        | m :: ms', P(coord, dir) when m <> TurnLeft && m <> TurnRight -> coord :: path (step p m) ms'
        | m :: ms',_ -> path (step p m) ms'
*)

    let path3 p ms =
        let rec aux p ms c =
            match p, ms with
            | P(coord, dir), [] -> c [coord]
            | P(coord, dir), m :: ms' when m <> TurnLeft && m <> TurnRight -> aux (step p m) ms' (fun result -> c(coord::result))
            | _, m :: ms' -> aux (step p m) ms' c
        aux p ms id

(* 2: Code Comprehension *)
    let foo f =
        let mutable m = Map.empty
        let aux x =
            match Map.tryFind x m with
            | Some y when Map.containsKey x m -> y
            | None   -> 
            m <- Map.add x (f x) m; f x

        aux
    
    let rec bar x =
      match x with 
      | 0 -> 0 
      | 1 -> 1
      | y -> baz (y - 1) + baz (y - 2)

    and baz = foo bar

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A: 
    foo: ('a -> 'b) -> 'a -> 'b
    bar: int -> int
    baz: int -> int


    Q: What do functions foo and baz do (skip bar)?
       Focus on what they do rather than how they do it.
    
     let foo f =
        let mutable m = Map.empty
        let aux x =
            match Map.tryFind x m with
            | Some y when Map.containsKey x m -> y
            | None   -> 
            m <- Map.add x (f x) m; f x

        aux
    A: 
    foo: 
    takes a funtion as argument and adds it to a map if the key
    is not already in the map. The value is the function on x
    
    baz: Calls foo with the bar function that calls baz recursively with the precious
    intergers. Baz creates a map of the fibonacci sequence

    The function foo uses a mutable variable.

    Q: What function does it serve (why is it there)?

    A: 
    It is like a variable in other languages, the mutables are stored in the stack
    

    Q: What would happen if you removed the mutable keyword from the line
       let mutable m = Map.empty? Would the function foo still work?
       If yes, why; if no, why not?

    A: 
    no because the map wouldn't be stored on the stack and the previous values wouldn't be stored

    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: 
    foo: fibMp
    bar: fibBase
    baz: fibonnaciSeq
    
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The code includes the keyword "and".

    
    Q: What function does this keyword serve in general
       (why would you use "and" when writing any program)?

    A: "and" is used to declare mutually recursive functions.
    We use and when we want a term available to us without having to declare 
    it before, like here where bar and baz use each other, this would not be 
    possible without the and keyword


    Q: What would happen if you removed it from this particular program and
       replaced it with a standard "let"
       (change the line "and baz = foo bar" to "let baz = foo bar")?

    A: 
    We wouldn't be able to do that since bar and baz are mutually recursive,
    we therefore could declare one before the other.

    *)

(* Question 2.3 *) 

    (* 
    The function foo generates a warning during compilation:
    "Warning: Incomplete pattern matches on this expression.".

    Q: Why does this happen, and where? 

    A: 
    because there is a pattern rule on the 'Some' match case.
    F# therefore thinks theres a possibility of there is a pattern match
    the program doesn't handle.
    
    Since there is no 'Some' match without a when clause the 
    compiler cannot determine whether the when clauses are exhaustive or not
    
    
    
   // Incomplete pattern matches on this expression. For example, the value '1' 
       may indicate a case not covered by the pattern(s). However, a pattern rule with a 'when' clause might 
       successfully match this value.
    
       Since there is no non-constant match without a when clause the compiler cannot determine whether or not
       the when clauses are exhaustive or not. In this case they are not as described in Q2.1.  


    Q: For these particular three functions will this incomplete pattern match
       ever cause problems for any possible execution of baz? If yes, why;
       if no, why not.

    A: 
    No, because the when clause of foo is redundent, it only
    confirm what has already been matched on.

    Q: The function foo has two redundant computations and is hence not as
       efficient as it could be. What are these two computations and why
       are they redundant?

    A: 
    One of them is containsKey. This is redundant since it has alredy 
    been checked in the pattern matching.
    
    The other one is that the function is called on x two times

    *)

    let foo2 f =
        let mutable m = Map.empty
        let aux x =
            match Map.tryFind x m with
            | Some y  -> y
            | None   ->
                let value = f x
                m  <- Map.add x value m; value

        aux

(* Question 2.4 *)

    let rec barbaz x =
        let baz = foo barbaz
        match x with 
        | 0 -> 0 
        | 1 -> 1
        | y -> baz (y - 1) + baz (y - 2)

    (*

    Q: Without explicitly timing the execution times, compare the execution
       times of baz and barbaz. One is slower than the other.
       Why? You do not have to give exact times, just spot which one is
       slower and explain why.

    A: barbaz is slower than baz it makes more recursive calls to 
    the stack than baz

    *)
(* Question 2.5 *)

    let bazSeq  =
        Seq.initInfinite baz

(* 3: Guess the next sequence element *)

(* Question 3.1 *)

    type element = int list

(* Question 3.2 *)

    let elToString el =
        List.fold (fun acc elem -> acc + string elem) "" el
    let elFromString str =
        List.init (String.length str) (fun i -> (int) str[i] - 48)
(* Question 3.3 *)
    
    let countElem x =
        let startElem = List.item 0 x
        let rec aux lst count rest =
            match lst with
            | [] -> ((count, startElem), rest)
            | x:: xs when x = startElem -> aux xs (count+1) xs
            | _ -> ((count, startElem), rest)
        aux x 0 []
        

    let nextElement el =
         let rec aux xs acc =
            match xs with
            | [] -> elFromString acc
            | xs ->
                let ((count, startElem), rest) = countElem xs
                aux rest (acc + string count + string startElem)
         aux el ""

(* Question 3.4 *)

    let elSeq (el: element) =
        el |> Seq.unfold (fun state -> Some(state, nextElement state))
    let elSeq2 _ = failwith "not implemented"

    (*

    Q: Why would Seq.initInfinite not be an appropriate choice to
       write a function like elSeq?

    A: <Your answer goes here>

    *)

(* Question 3.5 *)

    let compress _ = failwith "not implemented"

(* Question 3.6 *)

    let elParse str = failwith "not implemented"
        
    let elFromString2 _ = failwith "not implemented"

(* 4: Rings *)

(* Question 4.1 *)

    type 'a ring = Ring of 'a list * 'a list

(* Question 4.2 *)

    let length (Ring(a,b): 'a ring) = List.length a + List.length b
        (*let rec aux (a,b) acc1 acc2 =
            match a, b with
            | [],[] -> acc1+acc2
            | x::xs, y::ys -> aux (xs, ys) (acc1+1) acc2+1
            | x::xs, [] -> aux (xs, []) (acc1+1) acc2
            | [], y::ys -> aux ([], ys) acc1 acc2+1
        aux (a,b) 0 0*)
            
            
    let ringFromList lst = Ring([],lst)
        (*match lst with
        | [] -> Ring ([],[])
        | x::xs -> Ring ((x::xs),[])*)
        
    let ringToList (Ring(a,b): 'a ring) = b @ (a |> List.rev)
        (*let rec aux (a,b) acc1 acc2 =
            match a,b with
            | [],[] -> acc2 @ (acc1 |> List.rev)
            | x::xs,y::ys -> aux (xs,ys) (x::acc1) (y::acc2)
            | [], y::ys -> aux ([], ys) acc1 (y::acc2)
            | x::xs, [] -> aux (xs, []) (x::acc1) acc2
        aux (a,b) [] []*)

(* Question 4.3 *)

    let empty = Ring (List.empty, List.empty)
    let push x (Ring(a,b))  = Ring (a,x::b)
    let peek (Ring(a,b)) =
        match Ring(a,b) with
        | Ring ([],[]) -> None
        | Ring (xs,[]) -> Some (List.head(List.rev xs))
        | Ring (_, y::ys) -> Some y
       
        
    let pop (Ring(a,b)) =
        match Ring(a,b) with
        | Ring([],[]) -> None
        | Ring(xs, []) -> Some (Ring ([], List.tail(List.rev xs)))
        | Ring(xs,y::ys) -> Some (Ring(xs,ys))
        (*| x::xs, [] -> Some (Ring(xs,[])
        | xs, y::ys -> Some (xs,ys)
        | _,_ -> None*)
            
    let cw (Ring(a, b)) =
        match a,b with
        | [],[] -> Ring ([],[])
        | [], ys ->
            let ys' = List.rev ys
            Ring (List.tail ys', [List.head ys'])
        | x::xs, ys -> Ring (xs, x::ys)
        
    let ccw (Ring(a, b)) =
        match a,b with
        | [],[] -> Ring ([],[])
        | x::xs, [] ->
            let xs' = List.rev xs
            Ring ([List.head xs'], List.tail xs')
        | xs, y::ys -> (Ring ((y::xs), ys))

(* Question 4.4 *)

    type StateMonad<'a, 'b> = SM of ('b ring -> ('a * 'b ring) option)
    let ret x = SM (fun st -> Some (x, st))
    let bind (SM m) f =
        SM (fun st ->
            match m st with
            | None -> None
            | Some (x, st') ->
                let (SM g) = f x
                g st')

    let (>>=) m f = bind m f
    let (>>>=) m n = m >>= (fun () -> n)
    let evalSM (SM f) s = f s

    let smLength = SM (fun r -> Some(length r, r))
                       
    let smPush x = SM (fun r -> Some((), push x r))
    let smPop _ = failwith "not implemented"
    let smCW = SM (fun r -> Some((), cw r))
    let smCCW = SM (fun r -> Some((), ccw r))

(* Question 4.4 *)

    (* You may solve this exercise either using monadic operators or 
        using computational expressions. *)

    type StateBuilder() =

        member this.Bind(x, f)    = bind x f
        member this.Zero ()       = ret ()
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    (*let ringStep =
        smLength >>= (fun l ->
        if l > 1 then
            smPop >>= fun x ->
            smPop >>= fun y ->
            if x+y % 2 = 0 then
                ret ()
            else
                smPush y >>=
                smPush x >>=
                smCCW
        else
            ret ())*)
   
    
    let iterRemoveSumEven _ = failwith "not implemented"