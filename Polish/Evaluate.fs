module Evaluate

open Expression

let rec evaluate (expression : Expression) : double =
    match expression with 
    | IntValue(i) -> double i
    | DoubleValue(d) -> d
    | Add(lv,rv) -> evaluate lv + evaluate rv
    | Substract(lv,rv) -> evaluate lv - evaluate rv
    | Divide(lv,rv) -> evaluate lv / evaluate rv
    | Multiple(lv,rv) -> evaluate lv * evaluate rv
    | Brackets(e) -> evaluate e