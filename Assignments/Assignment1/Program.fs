// For more information see https://aka.ms/fsharp-console-apps
(*
    Green Assignments
*)
let sqr x= x*x

let pow x n= System.Math.Pow(x,n)

let rec sum n = 
    match n with
    | 0 -> 0
    | _ -> n + (sum (n-1))

let rec fib n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fib(n-1) + fib(n-2)

let dup n = n + " " + n


let dupn (s : string) (n : int) = String.replicate n s


let rec dupnr (s : string) (n : int) =
    match n with
    | 0 -> ""
    | _ -> s + (dupnr s (n-1))


let rec bin (n,k) =
    match n, k with
    | (n,0)  -> 1
    | (n, k) when n = k -> 1
    | (n, k) -> (bin ((n-1),(k-1))) + (bin ((n-1), k))  

(*
    Yellow Excersies
*)
let timediff (h1,m1) (h2, m2) = (h2-h1)*60 + m2-m1


let minutes (hh,mm) = timediff (0,0) (hh, mm)

let curry f x y = x+y
let uncurry f (x,y) = f x y


let empty (letter : char, point : int) pos =  (letter,point)

(*
    Red Exercises
*)
let add newPos (cv : (char * int)) (word : int -> char * int ) pos = 
    match pos with
    | k when k = newPos -> cv
    | _ -> word pos



