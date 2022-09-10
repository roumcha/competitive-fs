/// ここにあるもの:\
/// 素数ですか, 素数一覧, 素因数分解
module PrimeNum =
    open System.Collections.Generic

    module private NumericLiteralG =
        let inline FromZero () = LanguagePrimitives.GenericZero
        let inline FromOne () = LanguagePrimitives.GenericOne
        let inline FromInt32 n = Seq.sumBy (fun _ -> FromOne()) { 1 .. n }
    /// 素数ですか - O(√n)
    let inline isPrime n =
        let g2, g3 = 2G, 3G
        match sign (n - g2), n % g2 = 0G with
        | 0, _ -> true
        | -1, _
        | _, true -> false
        | _, _ ->
            seq {
                let mutable i = g3
                while i * i <= n do
                    yield i
                    i <- i + g2
            }
            |> Seq.forall ((%) n >> (<>) 0G)
    /// 素数列挙 - O(u log log u)\
    /// u > 100000 あたりからきつい
    let inline getPrimes until =
        let g2, g3 = 2G, 3G
        match until with
        | u when u < g2 -> List()
        | u ->

        let res =
            List(float u / (log (float u) - 1.) * 1.01 + 5. |> int)
        res.Add g2
        for i in g3 .. g2 .. u do
            if Seq.forall ((%) i >> (<>) 0) res then res.Add i
        res

    // todo: 素数列挙(エラトステネスの篩版)

    /// 素因数分解 - O(なんだっけ)
    let inline primeFact n =
        let g2, g3 = 2G, 3G
        let mutable x = n
        let mutable i = g3
        seq {
            while x % g2 = 0G do
                x <- x / g2
                yield g2
            while i * i <= x do
                match x % i = 0G with
                | false -> i <- i + g2
                | true ->
                    x <- x / i
                    yield i
            yield x
        }
