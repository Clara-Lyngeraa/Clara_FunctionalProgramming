// For more information see https://aka.ms/fsharp-console-apps
(*
    Green
*)
let rec downto1 n = 
    if n > 0 then
     n :: downto1 (n-1)
    else []

let rec downto2 n = 
    match n with
    | n when n > 0 -> n :: downto2 (n-1)
    | _ -> []


(*
    2.2
*)
let rec removeOddIdx xs = 
    match xs with
        | [] -> []
        | [x] -> [x]
        | x::y::xs -> x :: removeOddIdx xs

(*
    2.3
*)

let rec combinePair xs =
    match xs with 
    | [] -> []
    | [x] -> []
    | x :: y :: xs -> (x,y) :: combinePair xs

(*
    2.4
*)
type complex = { a: float; b: float }

let mkComplex n k = {a = n; b = k}
let complexToPair ab = (ab.a, ab.b)

let (|+|) x y =  mkComplex (x.a+y.a)(x.b+y.b)
let (|*|) x y = mkComplex ((x.a*y.a)-(x.b*y.b))((x.b*y.a)+(x.a*y.b))

let (|-|) x y = 
    let inverse z = mkComplex -z.a -z.b
    (|+|) x (inverse y)

let (|/|) x y = 
    let divide z = mkComplex (z.a/(z.a**2.0+z.b**2.0)) (-z.b/(z.a**2.0+z.b**2.0))
    (|*|) x (divide y)

(*
    2.5
*)
let explode1 (s: string) = List.ofArray(s.ToCharArray())


let rec explode2 (s : string) =
    match s with
    | "" -> []
    | s -> s.[0]  :: explode2 s.[1..]

(*
    2.6
*)
let  implode cs= List.foldBack(fun c acc ->  (string) c + acc)cs ""

let implodeRev cs =  List.fold(fun c acc -> (string) acc + c) ""  cs

(*
    2.7 
*)

let toUpper s= implode (List.map System.Char.ToUpper (s |> explode2))
let toUpper2 s= List.map System.Char.ToUpper (s |> explode2) |> implode

(*
    2.8
*)

let rec ack (m,n) = 
    match (m,n) with
    | m,n when m = 0 -> n+1
    | m,n when m > 0 && n = 0 -> ack (m-1, 1)
    | m,n when m > 0 && n > 0 -> ack (m-1, ack(m,n-1))

(*
    Yellow
*)

(*
    2.9
*)

let time f =
  let start = System.DateTime.Now
  let res = f ()
  let finish = System.DateTime.Now
  (res, finish - start)

let timeArg1 f a = 
  match f with
    | f -> time (fun () -> f a)

(*
    2.10
*)
let rec downto3 f n e =
   match n with
   | n when n <= 0 -> e
   | _ -> downto3 f (n-1) (f n e)

let fac n = downto3 (fun n k -> n*k) n 1

let range (g:(int -> 'a) ) n =  downto3 (fun n y -> g n :: y) n []

(*
    2.11
*)

//type word = (char * int) list

let hello = [('H', 4);('E', 1); ('L', 1); ('L', 1); ('O', 1)]

(*
    2.12
*)
type word = (char * int) list

let singleLetterScore (word: word) pos acc = //snd word.[pos]
    let (_,b) = word.[pos]
    acc + b

let doubleLetterScore (word: word) pos acc =
    let (_,b) = word.[pos]
    acc + (b*2)

let tripleLetterScore (word:word) pos acc =
    let (_, b) = word.[pos]
    acc + (b*3)


(*
    2.13
*)
let doubleWordScore (word: word) pos acc = acc*2

let tripleWordScore (word: word) pos acc = acc*3

(*
    2.14
*)
let isConsonant c = 
    match System.Char.ToUpper c with
    |'A'|'E'|'I'|'O'|'U'|'Y' -> false
    | _ -> true

let oddConsonants (word: word) pos ac = 
    let amountConsonents  = List.fold (fun acc elem -> (if isConsonant (fst elem) then (acc+1) else acc)) 0 word
    if amountConsonents%2 = 0 then ac else (ac-1)

(*
    2.15
*)
type squareFun = word -> int -> int -> int
type square = (int * squareFun) list
let SLS : square = [(0, singleLetterScore)];;
let DLS : square = [(0, doubleLetterScore)];;
let TLS : square = [(0, tripleLetterScore)];;
let DWS : square = SLS @ [(1, doubleWordScore)];;
let TWS : square = SLS @ [(1, tripleWordScore)];;


let calculatePoints0 (squares: square list) (word: word) = 
    let mapiStuff = List.mapi (fun i (s: square) -> List.map (fun (prio,sqf) -> prio, sqf word i) s ) squares 
    let oneList = List.fold (fun acc elem -> acc @ elem) [] mapiStuff 
    let sorted = List.sortBy (fun prio -> fst(prio)) oneList
    let idk = List.map (fun acc -> snd acc) sorted
    let composed = List.fold (fun acc elem -> (acc >> elem)) id idk 
    composed 0  

let calculatePoints1 (squares: square list) (word: word) = 
    let composed = List.mapi (fun i (s: square) -> List.map (fun (prio,sqf) -> prio, sqf word i) s ) squares |>
                    List.fold (fun acc elem -> acc @ elem) [] |>
                    List.sortBy (fun prio -> fst(prio)) |>
                    List.map (fun acc -> snd acc) |>
                    List.fold (fun acc elem -> (acc >> elem)) id 
    composed 0

let calculatePoints2 (squares: square list) (word: word) = 
    List.mapi (fun i (s: square) -> List.map (fun (prio,sqf) -> prio, sqf word i) s ) squares |>
    List.fold (fun acc elem -> acc @ elem) [] |>
    List.sortBy (fun prio -> fst(prio)) |>
    List.map (fun acc -> snd acc) |>
    List.fold (fun acc elem -> (acc >> elem)) id <| 0
   
let calculatePoints3 (squares: square list) (word: word) = 
    List.mapi (fun i (s: square) -> List.map (fun (prio,sqf) -> prio, sqf word i) s ) squares |>
    List.fold (@) [] |>
    List.sortBy (fun prio -> fst(prio)) |>
    List.map (fun acc -> snd acc) |>
    List.fold (fun acc elem -> (acc >> elem)) id <| 0