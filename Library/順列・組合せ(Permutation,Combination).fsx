/// ここにあるもの:\
/// 階乗(整数), 階乗(小数), nPr, nCr, パスカルの三角形, 挿入全通り, 順列全通り
module PermCombi =
    let private factCache =
        { 1L .. 20L } |> Seq.scan (*) 1L |> Seq.toArray

    /// 階乗(整数) - O(1)\
    /// 制約: 0 <= x <= 20
    let factorialN x =
        assert (0 < x && x < 20)
        Array.item x factCache

    /// 階乗(小数) - O(x)\
    /// 制約: 0 <= x
    let factorialF x =
        assert (x > 0.)

        match x with
        | 0. -> 1.
        | x -> { 1. .. x } |> Seq.reduce (*)

    /// nPr - O(1)\
    /// 制約: 0 <= (n, r) <= 20
    let nPr n r =
        match n < r with
        | true -> 0L
        | false -> factorialN n / factorialN (n - r)

    /// nCr - O(1)\
    /// 制約: [ int: n <= 12 ] [ int64: n <= 20 ]
    let nCr n r = nPr n r / factorialN r

    /// パスカルの三角形 - O(n^2)\
    /// 制約: 0 < n, [ int: n <= 33 ] [ int64: n <= 66 ] ...
    let inline createPascalΔ n =
        let a = Array2D.zeroCreate<'a> (n + 1) (n + 1)
        let one = LanguagePrimitives.GenericOne
        a.[0, 0] <- one

        for i in 1 .. n do
            for j in 0 .. i do
                match i, j with
                | i, j when j = 0 || j = i -> a.[i, j] <- one
                | i, j -> a.[i, j] <- a.[i - 1, j - 1] + a.[i - 1, j]

        a

    /// 挿入全通り - O(わからん)
    let rec allInsertions newElem lst =
        match lst with
        | [] -> [ [ newElem ] ]
        | h :: t ->
            (newElem :: lst)
            :: ((allInsertions newElem t)
                |> List.map (fun x -> h :: x))

    /// 順列全通り(重複あり) - O(わからん)
    let allPermutations source =
        let rec lp lst =
            match lst with
            | [] -> seq { [] }
            | head :: tail -> lp tail |> Seq.collect (allInsertions head)

        lp (Seq.toList source) |> Seq.map seq

    /// 組み合わせ全通り - できてない
    let allCombinations source n = []
