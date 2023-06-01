module Assignment5.Program

//5.1
let rec sum m n =
    let rec sumA y acc = 
        match y with
        |  0 -> m + acc
        |  y -> sumA (y-1) (acc + (m+y))
    sumA n 0 
 
 //5.2
let rec length list =
    let rec lengthA list acc =
        match list with
        | [] -> acc
        | x::xs -> lengthA xs (acc+1)
    lengthA list 0

//5.3
let rec foldBack f lst acc =
    let rec fBack f lst acc c =
        match lst with
        | [] -> c acc
        | x :: xs -> fBack f xs acc (fun acc -> c (f x acc))
    fBack f lst acc id

//5.4
let rec factC x =
    let rec aux x c =
        match x with
        | 0 -> c 1
        | x -> aux (x-1) (fun acc -> c (x * acc))
    aux x id

//5.5
let rec fibA x =
    let rec aux n acc acc2 =
        match n with
        | 0 -> acc
        | 1 -> acc2
        | n -> aux (n-1) acc2 (acc+acc2)
    aux x 0 1

let rec fibC x = 
    let rec aux n c  = 
        match n with
        | 0 -> c 0
        | 1 -> c 1
        | n -> aux (n-1) (fun acc -> aux (n-2) (fun acc2 -> c (acc + acc2))) 
    aux x id 

//5.6
let rec bigListK c =
    function
    | 0 -> c []
    | n -> bigListK (fun res -> c (1 :: res)) (n - 1)
(*
    Here we just change the stack a bunch of times, instead of building new stacks
*)

//5.7
 
type word = (char * int) list

type aExp =
    | N of int (* Integer literal *)
    | V of string (* Variable reference *)
    | WL (* Word length *)
    | PV of aExp (* Point value lookup at word index *)
    | Add of aExp * aExp (* Addition *)
    | Sub of aExp * aExp (* Subtraction *)
    | Mul of aExp * aExp (* Multiplication *)
    | CharToInt of cExp (* NEW: Cast to integer *)
and cExp =
    | C  of char (* Character literal *)
    | CV of aExp (* Character lookup at word index *)
    | ToUpper of cExp (* Convert character to upper case *)
    | ToLower of cExp (* Convert character to lower case *)
    | IntToChar of aExp (* NEW: Cast to character *)

let rec  arithEvalSimple a (w: word) s =
    match a with
    | N n -> n
    | V v -> 
                 match Map.tryFind v s with
                 | None -> 0
                 | Some s -> s
    | WL -> w.Length
    | PV i -> snd w.[arithEvalSimple i w s]
    | Add(x,y) -> 
                        let vx = arithEvalSimple x w s
                        let vy = arithEvalSimple y w s
                        vx + vy
    | Sub (x, y) -> 
                        let vx = arithEvalSimple x w s
                        let vy = arithEvalSimple y w s
                        vx - vy
    | Mul (x, y ) -> 
                        let vx = arithEvalSimple x w s
                        let vy = arithEvalSimple y w s
                        vx * vy
    | CharToInt c -> (int) (charEvalSimple c w s)
and  charEvalSimple c (w: word) s =
    match c with
    | C c -> c
    | ToUpper tu -> System.Char.ToUpper(charEvalSimple tu w s)
    | ToLower tl -> System.Char.ToLower(charEvalSimple tl w s)
    | CV i -> fst (w.[ arithEvalSimple i w s ])
    | IntToChar a -> (char) (arithEvalSimple a w s)


//5.8

let rec  arithEvalTail (w: word) s cont  a=
    let funA = arithEvalTail w s
    let funC = charEvalTail w s
    match a with
    | N n -> cont n
    | V v -> 
                 match Map.tryFind v s with
                 | None -> cont 0
                 | Some s -> cont s
    | WL -> cont w.Length
    | PV i -> funA (fun acc -> cont (snd w.[acc])) i
    | Add(x,y) -> 
                        funA (fun vx -> funA (fun vy -> cont (vx + vy)) y) x
    | Sub (x, y) -> 
                        funA (fun vx -> funA (fun vy -> cont (vx - vy)) y) x
    | Mul (x, y ) -> 
                        funA (fun vx -> funA (fun vy -> cont (vx * vy)) y) x
    | CharToInt c -> funC (fun vc -> cont ((int) vc)) c //charEvalTail c w s (fun acc -> id (acc))

and charEvalTail  w s cont  (c:cExp)=
    let funA = arithEvalTail w s
    let funC = charEvalTail w s
    match c with  
    | C c -> cont c
    | ToUpper tu -> funC (fun vtu -> cont (System.Char.ToUpper vtu)) tu //System.Char.ToUpper(charEvalTail tu w s id)
    | ToLower tl -> funC (fun vtl -> cont (System.Char.ToLower vtl)) tl//System.Char.ToLower(charEvalTail tl w s id)
    | CV i -> funA (fun acc -> cont (fst w.[acc])) i//fst (w.[ arithEvalTail i w s id])
    | IntToChar a -> funA (fun va -> cont ((char) va)) a//arithEvalTail a w s (fun acc -> id (acc))


let arithEval a w s = arithEvalTail  w s id a
let charEval c w s  = charEvalTail w s id c