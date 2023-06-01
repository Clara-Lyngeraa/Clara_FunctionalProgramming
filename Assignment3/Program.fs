(*
    Green
*)
//3.1
type aExp =
| N of int
| V of string
| WL
| PV of aExp
| Add of aExp * aExp  // Addition
| Sub of aExp * aExp  // Subtraction
| Mul of aExp * aExp  // Multiplication

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)
 

let rec arithEvalSimple a = 
    match a with   
    | N n -> n
    | Add (a, b) ->  arithEvalSimple a + arithEvalSimple b
    | Sub (a, b) ->  arithEvalSimple a - arithEvalSimple b
    | Mul (a, b) ->  arithEvalSimple a * arithEvalSimple b

//3.2


let rec arithEvalState a s = 
    match a with
    | N n -> n
    | V v -> 
                match Map.tryFind v s with
                | None -> 0
                | Some s -> s
    | Add(x,y) -> 
                        let vx = arithEvalState x s
                        let vy = arithEvalState y s
                        vx + vy
    | Sub (x, y) -> 
                        let vx = arithEvalState x s
                        let vy = arithEvalState y s
                        vx - vy
    | Mul (x, y ) -> 
                        let vx = arithEvalState x s
                        let vy = arithEvalState y s
                        vx * vy

//let binop f x y s= f (x s) (y s) 

// let rec arithEvalState2 a s = 
//     match a with
//     | N n -> n
//     | V v -> Map.find v s
//     | Add(x,y) -> binop (+) (arithEvalState2 x) (arithEvalState2 y) s
                        

//3.3


let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_");;
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithDoubleWordScore = N 2 .*. V "_acc_";;
let arithTripleWordScore = N 3 .*. V "_acc_";;

let hello = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)]
type word = (char * int) list

let rec arithEval a (w: word) s =
    match a with
    | N n -> n
    | V v -> 
                 match Map.tryFind v s with
                 | None -> 0
                 | Some s -> s
    | WL -> w.Length
    | PV i -> snd w.[arithEval i w s]
    | Add(x,y) -> 
                        let vx = arithEval x w s
                        let vy = arithEval y w s
                        vx + vy
    | Sub (x, y) -> 
                        let vx = arithEval x w s
                        let vy = arithEval y w s
                        vx - vy
    | Mul (x, y ) -> 
                        let vx = arithEval x w s
                        let vy = arithEval y w s
                        vx * vy


//3.4

type cExp =
| C  of char      (* Character value *)
| ToUpper of cExp (* Converts lower case to upper case character,
non-letters are unchanged *)
| ToLower of cExp (* Converts upper case to lower case character,
non-letters are unchanged *)
| CV of aExp      (* Character lookup at word index *)

let rec charEval c (w: word) s =
    match c with
    | C c -> c
    | ToUpper tu -> System.Char.ToUpper(charEval tu w s)
    | ToLower tl -> System.Char.ToLower(charEval tl w s)
    | CV i -> fst (w.[ arithEval i w s ])

//3.5
type bExp =
| TT
| FF
| AEq of aExp * aExp
| ALt of aExp * aExp
| Not of bExp
| Conj of bExp * bExp
| IsDigit of cExp
| IsLetter of cExp
| IsVowel of cExp

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)
let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)  (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let isConsonant0 c = 
    match System.Char.ToUpper c with
    |'A'|'E'|'I'|'O'|'U'|'Y' -> false
    | _ -> true

let rec boolEval b w s =
    match b with
    | TT  -> true
    | FF -> false
    | AEq (x,y) -> arithEval x w s = arithEval y w s
    | ALt (x,y) -> arithEval x w s < arithEval y w s
    | Not x -> not (boolEval x w s)
    | Conj (x, y) -> boolEval x w s && boolEval y w s
    | IsDigit c -> System.Char.IsDigit (charEval c w s)
    | IsLetter c -> System.Char.IsLetter (charEval c w s)
    | IsVowel c -> not (isConsonant0 (charEval c w s))

(*
    Yellow
*)

// 3.6

let isConsonant (c: cExp) =  Not (IsVowel c) 

//3.7
type stmnt =
| Skip
| Ass of string * aExp
| Seq of stmnt * stmnt
| ITE of bExp * stmnt * stmnt (* if-then-else statement *)
| While of bExp * stmnt       (* while statement *)

let rec evalStmnt stm w s =
    match stm with 
    | Skip                                         -> s
    | Ass (x, a)                    -> 
                                                let v = arithEval a w s
                                                s.Add (x, v) 
    | Seq (stm1, stm2)              -> 
                                                let s = evalStmnt stm1 w s
                                                evalStmnt stm2 w s
    | ITE (guard, stm1, stm2) -> 
                                                let b = boolEval guard w s
                                                if b = true then evalStmnt stm1 w s
                                                else evalStmnt stm2 w s
    | While (guard, stm)                -> 
                                                let b =  boolEval guard w s
                                                if b = true then 
                                                    let s' = evalStmnt stm w s
                                                    evalStmnt (While (guard,stm)) w s'
                                                else s


//3.8

type squareFun = word -> int -> int -> int

let stmntToSquareFun stm  = 
    (fun (w: word) (pos: int) (acc: int) -> 
        let s =(Map.empty.Add ("_pos_", pos)).Add  ("_acc_", acc)
        let r = evalStmnt stm w s
        r.Item "_result_" ) : squareFun

let singleLetterScore = stmntToSquareFun (Ass ("_result_", arithSingleLetterScore))
let doubleLetterScore = stmntToSquareFun (Ass ("_result_", arithDoubleLetterScore))
let tripleLetterScore = stmntToSquareFun (Ass ("_result_", arithTripleLetterScore))
let doubleWordScore = stmntToSquareFun (Ass ("_result_", arithDoubleWordScore))
let tripleWordScore = stmntToSquareFun (Ass ("_result_", arithTripleWordScore))

let containsNumbers =
  stmntToSquareFun
    (Seq (Ass ("_result_", V "_acc_"),
          While (V "i" .<. WL,
                 ITE (IsDigit (CV (V "i")),
                      Seq (
                           Ass ("_result_", V "_result_" .*. N -1),
                           Ass ("i", WL)),
                      Ass ("i", V "i" .+. N 1)))))

(*
    Red
*)
//3.9


//3.10

type square2 = (int * stmnt) list
type square = (int * squareFun) list

let SLS = [(0, Ass ("_result_", arithSingleLetterScore))]
let DLS = [(0, Ass ("_result_", arithDoubleLetterScore))]
let TLS = [(0, Ass ("_result_", arithTripleLetterScore))]
let DWS = [(1, Ass ("_result_", arithDoubleWordScore))] @ SLS
let TWS = [(1, Ass ("_result_", arithTripleWordScore))] @ SLS

let calculatePoints2 (squares: ((int * squareFun) list) list) (word: word) = 
    List.mapi (fun i (s: square) -> List.map (fun (prio,sqf) -> prio, sqf word i) s ) squares |>
    List.fold (fun acc elem -> acc @ elem) [] |>
    List.sortBy (fun prio -> fst(prio)) |>
    List.map (fun acc -> snd acc) |>
    List.fold (fun acc elem -> (acc >> elem)) id <| 0

//calculatePoints2 : square2 list -> word -> int

let calculatePoints (squares: (int * stmnt) list) (word: word) = 
    calculatePoints2 (List.map (fun ((prio: int),stm)  ->  (prio, stmntToSquareFun stm) :: []) squares) word 



