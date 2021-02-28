module PolishUnitTests.EvaluateTests

open NUnit.Framework
open FsUnit

open Expression

let evaluate = Evaluate.evaluate

[<TestFixture>]
type ``TokenInputTests`` ()=
   static member ``args for test simple expressions`` () =
       seq {
           yield (Expression.IntValue 1, 1.)
           yield (Expression.DoubleValue 1., 1.)
           yield (Expression.Add (Expression.IntValue 1, Expression.IntValue 1), 2.)
           yield (Expression.Substract (Expression.IntValue 1, Expression.IntValue 1), 0.)
           yield (Expression.Multiple (Expression.IntValue 2, Expression.IntValue 3), 6.)
           yield (Expression.Divide (Expression.IntValue 4, Expression.IntValue 2), 2.)
           yield (Expression.Divide (Expression.IntValue 3, Expression.IntValue 2), 1.5)
       }
           
   [<TestCaseSource(nameof(``TokenInputTests``.``args for test simple expressions``))>] 
   member _.
      ``test simple expressions`` (args : Expression*double)=
         let (expr, expectedResult) = args
         expr |> evaluate |> should be (Constraints.EqualConstraint expectedResult)
