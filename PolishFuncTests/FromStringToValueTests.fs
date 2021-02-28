module PolishFuncTests.FromStringToValueTests

open NUnit.Framework
open FsUnit

let tokenize = Tokenize.tokenize
let parse = Parse.parse
let evaluate = Evaluate.evaluate

[<TestFixture>]
type ``TokenInputTests`` ()=
   static member ``args`` () =
       seq {
           yield ("1 +2", 3.)
           yield ("343- 2", 341.)
           yield ("2+3 /2", 3.5)
           yield ("7*6/2+1", 22.)
       }
           
   [<TestCaseSource(nameof(``TokenInputTests``.``args``))>] 
   member _.
      ``test`` (args : string*double)=
         let (str, expectedResult) = args
         str |> tokenize |> parse |> evaluate |> should be (Constraints.EqualConstraint expectedResult)
