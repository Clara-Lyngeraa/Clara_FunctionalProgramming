module ImpParser

    open Eval
    open Types

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2                 
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2 
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2 

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')' <?> "parenthesise" 

    //|>> fun (x,y) -> System.String.Concat <|> (many (pchar '_')))
    let rec toS (lst: char list) (str: string) =
        match lst with
        | [] -> str
        | x::xs -> toS xs (str + (System.Char.ToString x))
     
    let pid =
        pchar '_' <|> pletter .>>. (many (palphanumeric <|>  (pchar '_')))
        |>> fun (x,y) -> System.String.Concat (x,(List.fold (fun acc elem -> acc+ (System.Char.ToString elem)) "" y))
        <?> "pid"

    
    let unop op p1  = op .>*>. p1 |>> (fun (x,y) -> y) <?> "unop"
    
    let binop op p1 p2 = p1 .>*> op .>*>. p2  <?> "binop"

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CtomParse, cref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

   
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-') AtomParse |>> (fun x ->  (N -1, x)) |>> Mul <?> "Neg"
    let PVParse = unop  pPointValue AtomParse |>> PV <?> "PV"
    let VParse = pid |>>  (fun x -> V x)
    let CharToIntParse = unop pCharToInt CtomParse |>> CharToInt <?> "CharToInt"
    do aref := choice [CharToIntParse; NegParse; NParse; PVParse; VParse; ParParse]

    let AexpParse = TermParse
    
    let CParParse = parenthesise CtomParse
    let CParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "C"
    let CVParse = unop pCharValue AtomParse |>> CV <?> "CV"
    let ToUpperParse = unop  pToUpper CtomParse |>> ToUpper <?> "ToUpper"
    let ToLowerParse = unop  pToLower CtomParse |>> ToLower <?> "ToLower"
    let IntToCharParse =  unop pIntToChar AtomParse |>> IntToChar <?> "IntToChar"
    
    do cref := choice [ToLowerParse; ToUpperParse; IntToCharParse; CParse; CVParse; CParParse]
    let CexpParse = CtomParse

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"


    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    let mkBoard (bp : boardProg) : board = failwith "not implemented"

