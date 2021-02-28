module Parse

open System.Collections.Generic

open Expression

type private ZeroOperator =
   | OpeningBracket
   | ClosingBracket
      
type private BinaryOperator =
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
            
   
type private Operator = 
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
    
let private TryParseToOperator (str : string) : (bool * Operator) =
    match str with
    | "(" -> (true, Operator.ZeroOperator ZeroOperator.OpeningBracket)
    | ")" -> (true, Operator.ZeroOperator ZeroOperator.ClosingBracket)
    | "+" -> (true, Operator.BinaryOperator BinaryOperator.Plus)
    | "-" -> (true, Operator.BinaryOperator BinaryOperator.Minus)
    | "/" -> (true, Operator.BinaryOperator BinaryOperator.DivisionSign)
    | "*" -> (true, Operator.BinaryOperator BinaryOperator.TimesSign)
    | _ -> (false, Operator.None)

type private ParsedUnit =
    | Operator of Operator
    | Int of int
    | Double of double

let private parseCore (tokens: seq<string>) : Stack<ParsedUnit> =
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
        | (r, _) when r = false -> token |> System.Double.Parse |> ParsedUnit.Double |> output.Push
    while operators.Count > 0 do
        operators.Pop() |> ParsedUnit.Operator |> output.Push
    output

let rec private unitsToExpression (output : Stack<ParsedUnit>) : Expression =
    match output.Pop() with
        | Int i -> Expression.IntValue i
        | Double d -> Expression.DoubleValue d
        | ParsedUnit.Operator o -> (match o with 
                                    | Operator.BinaryOperator bo -> (unitsToExpression output, unitsToExpression output) 
                                                                    |> (fun (a, b) -> (b, a)) 
                                                                    |> (match bo with
                                                                        | BinaryOperator.Plus -> Expression.Add
                                                                        | BinaryOperator.Minus -> Expression.Substract
                                                                        | BinaryOperator.DivisionSign -> Expression.Divide
                                                                        | BinaryOperator.TimesSign -> Expression.Multiple))

let parse (tokens: seq<string>) : Expression =
    tokens |> parseCore |> unitsToExpression