module Expression

type Expression =
    | IntValue of int
    | DoubleValue of double
    | Add of Expression * Expression
    | Substract of Expression * Expression
    | Divide of Expression * Expression
    | Multiple of Expression * Expression
    | Brackets of Expression