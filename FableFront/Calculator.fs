module Calculator

open Models

let tokenize = Tokenize.tokenize
let parse = Parse.parse
let evaluate = Evaluate.evaluate

type Calculator() =
   member _.Calculate (state : State) : State =
      match state with
      | _ when state.calculationString = null -> (CalculationResult.Error "No expression") |> state.SetResult
      | _ -> ((try (state.calculationString |> tokenize |> parse |> evaluate |> CalculationResult.Result) with 
                | _ -> CalculationResult.Error "Failed to calculate") |> state.SetResult)

