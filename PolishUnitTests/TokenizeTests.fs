module PolishUnitTests.TokenizeTests

open NUnit.Framework
open FsUnit

let tokenize = Tokenize.tokenize

[<TestFixture>]
type ``TokenInputTests`` ()=
   static member ``args for test single tokens`` () =
       seq {
           yield ("+", "+")
           yield ("-", "-")
           yield ("/", "/")
           yield ("*", "*")
           yield (" ", null)
           for i in 0..9 do
               yield i |> string |> fun a -> (a, a)
       }
    
   static member ``args for test multi-digital token`` () =
       seq {
           yield ("123 ", 123)
       }
       
   [<TestCaseSource(nameof(``TokenInputTests``.``args for test single tokens``))>] 
   member _.
      ``test single tokens`` (args : string*string)=
         let (str, expectedStr) = args
         let tokens = str |> tokenize |> System.Linq.Enumerable.ToArray
         match expectedStr with 
         | null -> tokens.Length |> should be (Constraints.EqualConstraint 0)
         | _ -> tokens.Length |> should be (Constraints.EqualConstraint 1); tokens.[0] |> should be (Constraints.EqualConstraint expectedStr)

         
   [<TestCaseSource(nameof(``TokenInputTests``.``args for test multi-digital token``))>] 
   member _.
      ``test multi-digital token`` (args : string*int)=
         let (str, expectedNumber) = args
         let tokens = str |> tokenize |> System.Linq.Enumerable.ToArray
         tokens.[0] |> System.Int32.Parse |> should be (Constraints.EqualConstraint expectedNumber)