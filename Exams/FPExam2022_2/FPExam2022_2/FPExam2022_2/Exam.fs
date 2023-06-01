module Exam2022_2
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2022_2 = 
 *)

(* 1: Grayscale images *)

    type grayscale =
    | Square of uint8
    | Quad of grayscale * grayscale * grayscale * grayscale
    
    let img = 
      Quad (Square 255uy, 
            Square 128uy, 
            Quad(Square 255uy, 
                 Square 128uy, 
                 Square 192uy,
                 Square 64uy),
            Square 0uy)
    
(* Question 1.1 *)
    let rec maxDepth img = 
        match img with
        | Square i -> 0
        | Quad (a, b, c, d) -> 
            let maxA = maxDepth a
            let maxB = maxDepth b
            let maxC = maxDepth c
            let maxD = maxDepth d
            1 + maxA + maxB + maxC + maxD 
    
(* Question 1.2 *)
    let rec mirror img = 
        match img with
         | Square i -> Square i
         | Quad (a, b, c, d) -> 
            let mirrorA = mirror a
            let mirrorB = mirror b
            let mirrorC = mirror c
            let mirrorD = mirror d
            Quad (mirrorB, mirrorA, mirrorD, mirrorC)

(* Question 1.3 *)
    let rec operate (f: (grayscale -> grayscale -> grayscale -> grayscale -> grayscale)) img = 
        match img with
        | Square i -> Square i
        | Quad (a, b, c, d) -> 
            let operateA = operate f a
            let operateB = operate f b
            let operateC = operate f c
            let operateD = operate f d   
            Quad(operateA, operateB, operateC, operateD)                                  
    
   (*  let mirror2 img = 
        let f a b c d = 
            match a,b ,c ,d with
            | Square v1, S -> Square v
            | Quad (a, b, c, d) -> Quad(b, a, d, c)
        operate f img *)

(* Question 1.4 *)

   (*  let rec compress img = 
        match img with 
        | Square i -> Square i
        | Quad (a, b, c, d) ->
                    match a, b, c, d with
                    | Sqaure v1, Square v2, Square v3, Square v4 ->
                        if v1 = v2 && v2 = v3 && v3 = v4 then Square v1
                        else failwith "nothing"
                    | _,_,_,_ -> 
                        let compressA = compress a
                        let compressB = compress b
                        let compressC = compress c
                        let compressD = compress d
                        Quad (compressA, compressB, compressC, compressD) *)
 
(* 2: Code Comprehension *)
    let rec foo f =
        function
        | []               -> []
        | x :: xs when f x -> x :: (foo f xs)          
        | _ :: xs          -> foo f xs
            
    let rec bar fs xs =
        match fs with
        | []       -> xs
        | f :: fs' -> bar fs' (foo f xs)

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo and bar?

    A: 
    foo = ('a -> bool) -> 'a list -> 'a list
    bar = ('a -> bool) list -> 'a list -> 'a list


    Q: What do the functions foo and  bar do. 
       Focus on what it does rather than how it does it.

    A: 
    foo: Takes a function and a list. If an element of the list evaluates to true
    in the function then the element is kept int the list, otherwise not
    
    bar: Takes a list of functions and a list.
    It executes each function on the list
    
    Q: What would be appropriate names for functions 
       foo and bar?

    A: 
    foo: filter
    bar: map
        
    Q: The function foo uses an underscore `_` in its third case. 
       Is this good coding practice, if so why, and if not why not?
    
    A: This is a good practice, because it makes sure to reach all possible 
    match statements. If the underscore is not used, F# can give an error
    saying that not all cases are met, but this is here avoided.
    *)
        

(* Question 2.2 
Create a non-recursive function bar2 that behaves the same as bar for all possible inputs where any recursive function that you use must be higher-order functions from the List library
any auxiliary functions that you write yourself or take from elsewhere must not be recursive. In particular you may not use foo .

    let rec bar fs xs =
        match fs with
        | []       -> xs
        | f :: fs' -> bar fs' (foo f xs)
*)


    let bar2 fs xs = 
        List.fold (fun _ elem -> 
            List.map elem xs) xs fs

(* Question 2.3 
Create a function baz of type ('a -> bool) list -> 'a -> bool such that 
bar fs xs and foo (baz fs) xs behave exactly the same for all possible 
inputs fs and xs .
*) 

    let baz fs  = 
        match fs with
        | [] -> fun x -> false
        | f :: fs' -> fun x -> f x

(* Question 2.4 *)

    (*

    Q: Only one of the functions `foo` and `bar` is tail recursive. Which one? 
       Demonstrate why the other one is not tail recursive.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

let rec foo f =
        function
        | []               -> []
        | x :: xs when f x -> x :: (foo f xs)          
        | _ :: xs          -> foo f xs
            
    let rec bar fs xs =
        match fs with
        | []       -> xs
        | f :: fs' -> bar fs' (foo f xs)
    
    A: 



    foo is tail recursive, bar is not
    
    bar [fun x -> x%0; fun x -> x%1] [1;2;3;4]
    bar [fun x -> x%1] (foo fun x -> x%0 [1;2;3;4]) 
    bar [fun x -> x%1] (foo fun x -> x%0 [2;3;4]) 
    bar [fun x -> x%1] (2 :: (foo fun x -> x%0 [3;4])) 
    bar [fun x -> x%1] (2 :: (foo fun x -> x%0 [4])) 
    bar [fun x -> x%1] (2 :: 4 :: (foo fun x -> x%0 [])) 
    
    bar [] (foo fun x -> x%1 2 :: 4 :: []) 



    *)
(* Question 2.5 *)
    // only implement the one that is NOT already tail recursive
(* 
    Create a function barTail , that behaves the same way
as  bar  but which is tail recursive and coded using continuations.

    let rec bar fs xs =
        match fs with
        | []       -> xs
        | f :: fs' -> bar fs' (foo f xs) *)
        
(*     let barTail fs xs = 
        let rec aux fs acc =
            match fs with
            | [] -> acc
            | f :: fs' ->  aux fs' ((foo f xs) :: acc)
        aux fs [] *)

    let barTail fs xs = 
        let rec aux fs c =
            match fs with
            | [] -> c xs
            | f :: fs' -> aux fs' (fun a -> c(foo f xs))
        aux fs id 



(* 3: Guess a number *)

    type guessResult = Lower | Higher | Equal
    type oracle =
        { max : int
          f : int -> guessResult }

(* Question 3.1 
Create a function validOracle of type oracle -> bool that given an oracle o returns true if o is a valid oracle and false otherwise. A valid oracle
1. only returns Equal for a single number x between 1 and max inclusive
2. returns Higher for all numbers greater than or equal to 1 and strictly smaller than x ( x is strictly
greater than the number guessed)
3. returns Lower for all numbers strictly greater than x and smaller than or equal to max ( x is strictly
lower than the number guessed)
*)

    let validOracle o = 
        let rec aux i =
            if i > o.max then true
            else match o.f i with
                | Equal -> aux (i+1)
                | _ -> false
        aux 1
    
    
 (*    let validOracle (o: oracle) = 
        let rec aux i =
            match o.max, o.f with
                | x, fs  -> if i < x && i > 0 
                        
                | _ -> false *)
                

(* Question 3.2 *)

    let randomOracle m oseed = failwith "not implemented"

(* Question 3.3 *)
    
    let findNumber _ = failwith "not implemented"

(* Question 3.4 *)
    let evilOracle _ = failwith "not implemented"
    
(* Question 3.5 *)
    let parFindNumber _ = failwith "not implemented"

(* 4: Assembly *)

    type register = R1 | R2 | R3
    type address = uint

    type assembly =
    | MOVI of register * int
    | MULT of register * register * register
    | SUB of register * register * register
    | JGTZ of register * address
    
     
    let factorial x =           // Address
        [MOVI (R1, 1)           // 0
         MOVI (R2, x)           // 1
         MOVI (R3, 1)           // 2
         MULT (R1, R1, R2)      // 3 (Loop starts here)
         SUB  (R2, R2, R3)      // 4
         JGTZ (R2, 3u)]         // 5 (Loop ends here)
    
(* Question 4.1 *)

    type program = unit (* replace this entire type with your own *)
    let assemblyToProgram _ = failwith "not implemented"

(* Question 4.2 *)

    type state = unit (* replace this entire type with your own *)
    let emptyState _ = failwith "not implemented"
    

(* Question 4.3 *)

    let setRegister _ = failwith "not implemented"
    
    let getRegister _ = failwith "not implemented"
    
    let setProgramCounter _ = failwith "not implemented"
    
    let getProgramCounter _ = failwith "not implemented"
    
    let getProgram _ = failwith "not implemented"
    
(* Question 4.4 *)
    
    type StateMonad<'a> = SM of (state -> 'a * state)

    let ret x = SM (fun s -> x, s)
    let bind f (SM a) : StateMonad<'b> = 
      SM (fun s -> 
      let x, s' = a s
      let (SM g) = f x
      g s')

    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)

    let evalSM prog (SM f) = f (emptyState prog)

    let setReg _ = failwith "not implemented"
    
    let getReg _ = failwith "not implemented"
    
    let setPC _ = failwith "not implemented"
    
    let incPC _ = failwith "not implemented"
    
    let lookupCmd _ = failwith "not implemented"
    


(* Question 4.5 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let runProgram _ = failwith "not implemented"
    