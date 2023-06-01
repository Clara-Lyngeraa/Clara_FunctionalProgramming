module Exam2021_2

    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Core

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

 //module Exam2021_2 = 


(* 1: Binary lists *)

(* Question 1.1 *)

    type binList<'a, 'b> =
    | Nil
    | Cons1 of 'a * binList<'a, 'b>
    | Cons2 of 'b * binList<'a, 'b>

    let rec length binlst =
        match binlst with
        | Nil -> 0
        | Cons1 (a,b) -> 1 + length b
        | Cons2(b, binList) -> 1+ length binList
    
(* Question 1.2 *)
    let split lst =
        let rec aux lst acc1 acc2 =   
            match lst with
            | Nil -> (List.rev acc1, List.rev acc2)
            | Cons1(a, binList) ->  aux binList (a :: acc1) acc2
            | Cons2(b, binList) -> aux binList acc1 (b::acc2)
        aux lst [] []
            
    let length2 lst : int*int =
        let rec aux lst acc1 acc2 =
            match lst with
            | Nil -> acc1,acc2
            | Cons1(a, binList) -> aux binList (acc1+1) acc2
            | Cons2(b, binList) -> aux binList acc1 (acc2+1)
        aux lst 0 0

(* Question 1.3 *)


    let rec map f g lst =
        match lst with
        | Nil -> Nil 
        | Cons1(c, binList) -> Cons1(f c, map f g binList) 
        | Cons2(c, binList) -> Cons2(g c, map f g binList)
        
(* Question 1.4 *)

    let rec filter f g lst =
        match lst with
        | Nil -> Nil 
        | Cons1(c, binList) when f c -> Cons1(c, filter f g binList)
        | Cons1(c, binList) -> filter f g (binList)
        | Cons2(c, binList) when g c -> Cons2(c, filter f g binList)
        | Cons2(c, binList) -> filter f g binList

(* Question 1.5 *)

    let rec fold f g acc lst =
        match lst with
        | Nil -> acc
        | Cons1(a, binList) -> fold f g (f acc a) binList 
        | Cons2(b, binList) -> fold f g (g acc b) binList 

        
(* 2: Code Comprehension *)
    let rec foo xs ys =
      match xs, ys with
      | [], ys -> ys
      | xs, [] -> xs
      | x :: xs, y :: ys when x < y ->
        x :: (foo xs (y :: ys))
      | x :: xs, y :: ys ->
        y :: (foo (x :: xs) ys)

    and bar =
      function
      | [] -> []
      | [x] -> [x]
      | xs ->
        let (a, b) = List.splitAt (List.length xs / 2) xs
        foo (bar a) (bar b)

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo and bar?

    A: 
    foo: 'a list -> 'a list -> 'a list
    bar: 'a list -> 'a list


    Q: What does the function bar do.
       Focus on what it does rather than how it does it.

    A: 
    It takes a list, and if the list is longer than 1 it splits it
    in two and call the foo function on the two lists where the two lists
    will be sorted into one
    
    Q: What would be appropriate names for functions 
       foo and bar?

    A: 
    foo: merge
    bar: mergeSort
    
    Q: What would be appropriate names of the values a and b in bar.
    
    
    A:
    a: firstHalf
    b: secondHalf
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The code includes the keyword "and".

    
    Q: What function does this keyword serve in general
       (why would you use "and" when writing any program)?

    A: 
    When using "and" it means that we have mutual recursice functions, 
   meaning funcitons that call each other. This means that hte function can 
   call each other no matter order


    Q: What would happen if you removed it from this particular program and
       replaced it with a standard "let"
       (change the line "and bar = " to "let rec bar = ")?
       Explain why the program either does or does not work.

    A: 
    the program does work because foo doesn't call bar
    so as long as they are in the order they are in now, it is no problem.

let rec foo xs ys =
      match xs, ys with
      | [], ys -> ys
      | xs, [] -> xs
      | x :: xs, y :: ys when x < y ->
        x :: (foo xs (y :: ys))
      | x :: xs, y :: ys ->
        y :: (foo (x :: xs) ys)
    *)

(* Question 2.3 *) 
    
    let foo2 xs ys =
        List.unfold (
                     function
                     | [], [] -> None
                     | [], y :: ys -> Some (y, ([], ys))
                     | x :: xs, [] -> Some (x, (xs,[]))
                     | x :: xs, y :: ys when x < y -> Some (x, (xs, y::ys))
                     | x :: xs, y :: ys -> Some (y, (x::xs,ys))
                     ) (xs,ys)
        

(* Question 2.4 *)

    (*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).
    
    let rec foo xs ys =
      match xs, ys with
      | [], ys -> ys
      | xs, [] -> xs
      | x :: xs, y :: ys when x < y ->
        x :: (foo xs (y :: ys))
      | x :: xs, y :: ys ->
        y :: (foo (x :: xs) ys)

    A: 
    foo [1;3] [2;4;5]
    1 :: (foo [3] [2;4;5])
    1 :: (2 ::(foo [3] [4;5]))
    1 :: (2 :: (3 :: (foo [] [4;5])))
    1 :: (2 :: (3 :: ([4;5])))
    
    foo is not recursive s elements are appended to the list that
    depend on a recursive call. Until the recursicve call is evaluated
    there is no list to append to

    *)
(* Question 2.5 *)
    let fooTail xs ys =
        let rec aux xs ys acc1=
            match xs,ys with
            | [], ys -> List.rev acc1 @ ys
            | xs, [] -> xs @ List.rev acc1
            | x :: xs, y :: ys when x < y -> (aux xs (y::ys) (x::acc1)) 
            | x :: xs, y :: ys -> aux (x:: xs) ys (y::acc1)
        aux xs ys []
    
 

(* Question 2.5
     and bar =
      function
      | [] -> []
      | [x] -> [x]
      | xs ->
        let (a, b) = List.splitAt (List.length xs / 2) xs
        foo (bar a) (bar b)
    *)

    (*
    let barTail lst =
        let rec aux xs' c =
            match xs' with
            | [] -> c[]
            | [x] -> c[x]
            | xs ->
                let (firstHalf, secondHalf) = List.splitAt (List.length xs / 2) xs
                foo (aux firstHalf (fun f -> c())) (aux secondHalf)
                *)
                
(* 3: Approximating square roots *)

(* Question 3.1 *)

    let nearestPerfect x =
        let rec aux index prevDist =
            let perfect = index * index
            let currDist = abs (x - perfect)
            if currDist < prevDist
            then aux (index+1) currDist
            else (index-1)
        aux 0 10000
    let rec approxSquare (x: int) num =
        let perfSquare = nearestPerfect x
        match num with
        | 0 -> float perfSquare
        | _ ->
            let rec aux (r: float) i =
                match i with
                | i when i = num -> r
                | i -> aux ((((float) x/r)+r)/2.0) (i+1)
            aux perfSquare 0
    
 

(* Question 3.2 *)

    let quadratic (a:int) (b:int) (c:int) num =
        let x1 = (-(float)b + (approxSquare ((b*b)-(4*(a*c))) num))/float (2*a)
        let x2 = (-float b - approxSquare ((b*b)-(4*(a*c))) num)/float (2*a)
        (x1,x2)

(* Question 3.3 *)

    let parQuadratic eqs numProcesses num =
        List.splitInto numProcesses eqs
            |> List.map (fun eqs ->
                async {
                    return List.fold (fun acc (a,b,c) -> acc@[(quadratic a b c num)]) [] eqs
                }) 
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Array.fold(fun acc r -> acc@r)[]

(* Question 3.4 *)

    let solveQuadratic str num = failwith "not implemented"

(* 4: Rational numbers *)

(* Question 4.1 *)

    type rat = (int*int)

(* Question 4.2 *)

    let rec gcd x y =
        if y = 0 then x
        else gcd y (x%y)
        
    let mkRat n d =
        match d with
        | 0 -> None
        | _ ->
            match n,d with
            | x,y when x < 0 && y < 0 ->
                let g = gcd x y
                Some ((n/g,d/g))
            | x,y when y < 0 ->
                let g = gcd x y 
                Some((-n/g,d/g*(-1)))
            | x,y when x < 0 ->
                let g = gcd x y 
                Some((-n/g,d/g*(-1)))
            | x,y ->
                let g = gcd x y
                Some((x/g,y/g))
            
    let ratToString (rat : rat)= string (fst rat) + " / " + string (snd rat)

(* Question 4.3 *)

    let plus r1 r2 =
        let a = fst r1
        let b = snd r1
        let c = fst r2
        let d : int= snd r2
        let add = mkRat ((a*d)+(b*c)) (b*d)
        match add with
        | None -> None
        | Some (x,y) -> Some(x,y)
        
    let minus r1 r2 =
        let a = fst r1
        let b = snd r1
        let c = fst r2
        let d : int= snd r2
        let min = mkRat ((a*d)-(b*c)) (b*d)
        match min with
        | None -> None
        | Some (x,y) -> Some(x,y)
    let mult r1 r2 =
        let a = fst r1
        let b = snd r1
        let c = fst r2
        let d : int= snd r2
        let mul = mkRat (a*c) (b*d)
        match mul with
        | None -> None
        | Some (x,y) -> Some(x,y)
    let div r1 r2 =
        let a = fst r1
        let b = snd r1
        let c = fst r2
        let d : int= snd r2
        let di = mkRat (a*d) (b*c)
        match di with
        | None -> None
        | Some (x,y) -> Some(x,y)

(* Question 4.4 *)

    type SM<'a> = SM of (rat -> ('a * rat) option)
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

    let smPlus (rat : rat) = SM (fun state ->
        match plus state rat with
        | None -> None
        | Some(i, i1) -> Some((), (i,i1)))
    let smMinus rat = SM (fun state ->
        match minus state rat with
        | None -> None
        | Some(i, i1) -> Some((), (i,i1)))
    let smMult rat = SM (fun state ->
        match mult state rat with
        | None -> None
        | Some(i, i1) -> Some((), (i,i1)))
    let smDiv rat = SM (fun state ->
        match div state rat with
        | None -> None
        | Some(i, i1) -> Some((), (i,i1)))

(* Question 4.5 *)

    (* You may solve this exercise either using monadic operators or 
        using computational expressions. *)

    type StateBuilder() =

        member this.Bind(x, f)    = bind x f
        member this.Zero ()       = ret ()
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let rec calculate lst =
        state {
            match lst with
            | [] -> return ()
            | (rat,f) :: xs ->
                do! f rat
                return! calculate xs
        }