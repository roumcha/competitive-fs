[<AutoOpen>]
module Memo =
    let memoize f =
        let m = System.Collections.Generic.Dictionary()
        fun x ->
            match m.TryGetValue x with
            | true, r -> r
            | _ ->
                let r = f x
                m.Add(x, r)
                r

(* Usage:

let rec fib x =
    match x <= 2 with
    | true -> 1
    | false ->
        // Use 'fib_memo' instead of 'fib' when recurse.
        fib_memo (x - 1) + fib_memo (x - 2)

and fib_memo = memoize fib

*)
