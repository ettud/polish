module Tokenize

open System

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