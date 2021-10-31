/// ここにあるもの:\
/// 階乗(整数), 階乗(小数), nPr, nCr, パスカルの三角形, 挿入全通り, 順列全通り
module PermCombi =
    /// ジェネリック数値型 - 生成:O(n)\
    /// 参考: https://shuyo.hatenablog.com/entry/20101016/fsharp
    module private NumericLiteralG =
        let inline FromZero () = LanguagePrimitives.GenericZero
        let inline FromOne () = LanguagePrimitives.GenericOne
        let inline FromInt32 n = Seq.sumBy (fun _ -> FromOne()) { 1 .. n }

    let private fastCache: int64 [] = { 1G .. 20G } |> Seq.scan (*) 1G |> Seq.toArray
    /// 階乗(整数) - O(1)\
    /// 制約: 0 <= x <= 20
    let factorialFast x = Array.item x fastCache
    /// nPr - O(1)\
    /// 制約: 0 <= (n, r) <= 20
    let nPrFast n r =
        match n < r with
        | true -> 0G
        | false -> factorialFast n / factorialFast (n - r)
    /// nCr - O(1)\
    /// 制約: 0 <= (n, r) <= 20
    let nCrFast n r = nPrFast n r / factorialFast r
    /// 階乗(小数) - O(x)\
    /// 制約: 0 <= x
    let inline factorial x =
        match x <= 0G with
        | true -> 1G
        | false -> { 1G .. x } |> Seq.reduce (*)
    /// nPr - O(n)\
    /// 制約: 0 <= (n, r) <= 20
    let inline nPr n r =
        match n < r with
        | true -> 0G
        | false -> Seq.reduce (*) { n .. -1G .. r }
    /// パスカルの三角形 - O(n^2)\
    /// 制約: 0 < n, [ int: n <= 33 ] [ int64: n <= 66 ] ...
    let inline createPascalΔ n =
        let a = Array2D.zeroCreate (n + 1) (n + 1)
        let one = LanguagePrimitives.GenericOne
        a.[0, 0] <- one
        for i in 1 .. n do
            for j in 0 .. i do
                match i, j with
                | i, j when j = 0 || j = i -> a.[i, j] <- one
                | i, j -> a.[i, j] <- a.[i - 1, j - 1] + a.[i - 1, j]
        a
    /// 挿入全通り - O(わからん)\
    /// ソートされていない。
    let rec allInsertions newElem lst =
        match lst with
        | [] -> [ [ newElem ] ]
        | h :: t ->
            (newElem :: lst)
            :: ((allInsertions newElem t)
                |> List.map (fun x -> h :: x))
    /// 順列全通り(重複あり) - O(わからん)\
    /// ソートされていない。
    let allPermutations source =
        let rec lp lst =
            match lst with
            | [] -> seq { [] }
            | h :: t -> lp t |> Seq.collect (allInsertions h)
        Seq.toList source |> lp |> Seq.map seq

    let nextPermutation ary =
        let n = Array.length ary
        let sw i j =
            let t = Array.item i ary
            ary.[i] <- ary.[j]
            ary.[j] <- t

        let l =
            { n - 2 .. -1 .. 0 }
            |> Seq.tryFind (fun i -> ary.[i] < ary.[i + 1])
        match l with
        | None -> false
        | Some l ->

        let r =
            { n - 1 .. -1 .. l + 1 }
            |> Seq.find (fun i -> ary.[l] <= ary.[i])
        System.NotImplementedException() |> raise

// match ary.[i] = ary.[j] with
// | true
// | false ->
