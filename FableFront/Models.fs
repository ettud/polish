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

    new (calculationString) =
        { calculationString = calculationString; result = CalculationResult.None }

    private new(calculationString, result) =
        { calculationString = calculationString; result = result }

    member this.SetResult result =
        State(this.calculationString, result)

    member this.GetRecord = //a hack because Thoth by default does not work with types that are neither tuples nor records
        {| calculationString = this.calculationString; result = this.result |}
        