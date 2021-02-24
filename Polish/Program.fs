open System
open System.Collections.Generic

type ZeroOperator =
   | OpeningBracket
   | ClosingBracket
      
type BinaryOperator =
   | Plus
   | Minus
   | DivisionSign
   | TimesSign
      
   member lv.CompareToOperator (rv : BinaryOperator) =
        match lv with 
        | Plus 
        | Minus -> (match rv with 
                    | Plus 
                    | Minus -> 0
                    | DivisionSign
                    | TimesSign -> -1)
        | DivisionSign
        | TimesSign -> (match rv with 
                        | Plus 
                        | Minus -> 1
                        | DivisionSign
                        | TimesSign -> 0)
            
   
type Operator = 
    | ZeroOperator of ZeroOperator
    | BinaryOperator of BinaryOperator
    | None

    member lv.CompareToOperator (rv : Operator) =
        match lv with 
        | None -> (match rv with
                   | None -> 0
                   | _ -> -1)
        | ZeroOperator _ -> (match rv with
                               | None -> 1
                               | ZeroOperator _ -> 0
                               | BinaryOperator _ -> -1)
        | BinaryOperator lvbo -> (match rv with
                                   | BinaryOperator rvbo -> lvbo.CompareToOperator(rvbo)
                                   | _ -> -1)
    
let TryParseToOperator (str : string) : (bool * Operator) =
    match str with
    | "(" -> (true, Operator.ZeroOperator ZeroOperator.OpeningBracket)
    | ")" -> (true, Operator.ZeroOperator ZeroOperator.ClosingBracket)
    | "+" -> (true, Operator.BinaryOperator BinaryOperator.Plus)
    | "-" -> (true, Operator.BinaryOperator BinaryOperator.Minus)
    | "/" -> (true, Operator.BinaryOperator BinaryOperator.DivisionSign)
    | "*" -> (true, Operator.BinaryOperator BinaryOperator.TimesSign)
    | _ -> (false, Operator.None)

type Expr =
    | IntValue of int
    | DoubleValue of double
    | Add of Expr * Expr
    | Substract of Expr * Expr
    | Divide of Expr * Expr
    | Multiple of Expr * Expr
    | Brackets of Expr

type ParsedUnit =
    | Operator of Operator
    | Int of int
    | Double of double



let rec evaluate (expr : Expr) : double =
    match expr with 
    | IntValue(i) -> double i
    | DoubleValue(d) -> d
    | Add(lv,rv) -> evaluate lv + evaluate rv
    | Substract(lv,rv) -> evaluate lv - evaluate rv
    | Divide(lv,rv) -> evaluate lv / evaluate rv
    | Multiple(lv,rv) -> evaluate lv * evaluate rv
    | Brackets(e) -> evaluate e
   
let polishCore (tokens: seq<string>) : Stack<ParsedUnit> =
    let operators = Stack<Operator>()
    let output = Stack<ParsedUnit>()
    for token in tokens do
        match token |> TryParseToOperator with
        | (r, p) when r = true -> (match p with 
                                    | BinaryOperator _ when operators.Count = 0 -> operators.Push p
                                    | ZeroOperator OpeningBracket -> operators.Push p
                                    | ZeroOperator ClosingBracket -> (while not (operators.Peek() = Operator.ZeroOperator ZeroOperator.OpeningBracket) do
                                                                        operators.Pop() |> ParsedUnit.Operator |> output.Push)
                                    | BinaryOperator _ when operators.Count > 0 -> (match (operators.Peek() |> p.CompareToOperator) with 
                                                                                        | 1 -> operators.Push p
                                                                                        | _ -> ((while ((operators.Count > 0) && (operators.Peek() |> p.CompareToOperator) <= 0) do
                                                                                                    operators.Pop() |> ParsedUnit.Operator |> output.Push); operators.Push p)))
        | (r, _) when r = false -> token |> Double.Parse |> ParsedUnit.Double |> output.Push
    while operators.Count > 0 do
        operators.Pop() |> ParsedUnit.Operator |> output.Push
    output

let rec parsedUnitsToExpr (output : Stack<ParsedUnit>) : Expr =
    match output.Pop() with
        | Int i -> Expr.IntValue i
        | Double d -> Expr.DoubleValue d
        | ParsedUnit.Operator o -> (match o with 
                                    | Operator.BinaryOperator bo -> (parsedUnitsToExpr output, parsedUnitsToExpr output) 
                                                                    |> (fun (a, b) -> (b, a)) 
                                                                    |> (match bo with
                                                                        | BinaryOperator.Plus -> Expr.Add
                                                                        | BinaryOperator.Minus -> Expr.Substract
                                                                        | BinaryOperator.DivisionSign -> Expr.Divide
                                                                        | BinaryOperator.TimesSign -> Expr.Multiple))
    
let polish (tokens: seq<string>) : Expr =
    tokens |> polishCore |> parsedUnitsToExpr


let rec tokenize (str : string) = 
    seq {
        let mutable st : System.Text.StringBuilder = null
        for i in 0..str.Length-1 do
            let ch = str.Chars(i)
            match ch with 
                | '+' | '-'  | '/'  | '*'  | '('  | ')' when st <> null -> yield st.ToString(); st <- null; yield ch.ToString()
                | '+' | '-'  | '/'  | '*'  | '('  | ')'  -> yield ch.ToString()
                | ' ' when st <> null -> yield st.ToString(); st <- null;
                | ' ' -> ()
                | _ when Char.IsDigit ch -> (if st = null then st <- System.Text.StringBuilder()); st.Append(ch) |> ignore
                | _ -> failwith "Unknown character"
        if st <> null then
            yield st.ToString()
    }

let calc evaluation = 
    Double.Parse(evaluation)

[<EntryPoint>]
let main argv =
    () |> Console.ReadLine |> tokenize |> polish |> evaluate |> string |> Console.WriteLine
    0
