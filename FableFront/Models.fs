module Models

type CalculationResult = 
    | Result of double
    | Error of string
    | None

type State = 
    val public calculationString : string
    val public result : CalculationResult

    new() =
        { calculationString = ""; result = CalculationResult.None }

    new calculationString =
        { calculationString = calculationString; result = CalculationResult.None }

    private new(calculationString, result) =
        { calculationString = calculationString; result = result }

    member this.SetResult result =
        State(this.calculationString, result)