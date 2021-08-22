/// ここにあるもの:\
/// 素数ですか, 素数一覧, 素因数分解
module PrimeNum =
    open System.Collections.Generic

    /// 素数ですか - O(√n)
    let isPrime n =
        match n, Seq.forall ((%) n >> (<>) 0) { 2 .. n |> float32 |> sqrt |> int } with
        | n, _ when n < 2 -> false
        | _, b -> b

    /// 素数列挙 - O(u log log u)\
    /// u > 100000 あたりからきつい
    let getPrimes until =
        match until with
        | u when u < 2 -> List()
        | u ->
            let res = List [ 2 ]

            for i in 3 .. u do
                if Seq.forall ((%) i >> (<>) 0) res then
                    res.Add i

            res

    /// 素因数分解 - O(なんだっけ)
    let primeFactorize x =
        /// 割れるだけ割った回数と余り
        let divCQ dived divor =
            let rec lp d i =
                let p, (r: int) = System.Math.DivRem(d, divor)

                match r with
                | 0 -> lp p (i + 1)
                | _ -> (i, d)

            lp dived 0

        /// メインループ
        let rec lp x i res =
            if i * i > x then
                match x with
                | 1 -> res
                | n -> (n, 1) :: res
            else
                match divCQ x i with
                | 0, quot -> lp quot (i + 1) res
                | divCnt, quot -> lp quot (i + 1) ((i, divCnt) :: res)

        lp x 2 [] |> List.rev
