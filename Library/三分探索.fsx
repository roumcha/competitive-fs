module TernSearch =
    let inline private bi (x: obj) =
        match x with
        | :? int as x -> bigint x
        | :? uint as x -> bigint x
        | :? int64 as x -> bigint x
        | :? uint64 as x -> bigint x
        | :? bigint as x -> x
        | _ -> exn () |> raise

    type private Vec<'a> = System.Collections.Generic.List<'a>
    let private fibs = Vec [ 1I; 1I; 2I; 3I; 5I ]

    let rec private findFibIdx i x =
        if i >= fibs.Count then
            fibs.[fibs.Count - 1] + fibs.[fibs.Count - 1]
            |> fibs.Add
        match bi x <= fibs.[i] with
        | true -> i
        | false -> findFibIdx (i + 1) x

    type private FibRange<'a, 'b> =
        { L: 'a
          R: 'a
          Mid1: 'a
          Mid2: 'a
          LVal: 'b
          RVal: 'b
          Mid1Val: 'b
          Mid2Val: 'b
          FibIdx: int }

    /// フィボナッチ探索 - O(log N)\
    /// l, r: int, uint, int64, uint64
    let inline searchFib l r defVal getVal comparer =
        let g1 = LanguagePrimitives.GenericOne
        let g2, g3 = g1 + g1, g1 + g1 + g1
        let l, fibIdx = l - g1, findFibIdx 0 (r - l + g2)
        let firstRng =
            { L = l
              R = l + fibs.[fibIdx]
              Mid1 = l + fibs.[fibIdx - 2]
              Mid2 = l + fibs.[fibIdx - 1]
              LVal = defVal
              RVal = defVal
              Mid1Val = defVal
              Mid2Val = defVal
              FibIdx = fibIdx }
        let rec lp rng = 
            

        lp firstRng
