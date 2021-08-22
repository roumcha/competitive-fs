/// ここにあるもの:\
/// 商と余り, 切り上げ除算, 範囲の総和, 最大公約数, 最大公約数I, 最小公倍数, 最小公倍数I
module IntCalc =
    /// 商と余り - O(1)\
    /// int64 に変更可。bigInt には bigInt.DivRem がある。
    let divRem x y : int * int = System.Math.DivRem(x, y)

    /// 切り上げ除算 - O(1)
    let divCeil divided divisor = (divided + divisor - 1) / divisor

    /// 範囲の総和 - O(1)
    let rangeSum min max = (max - min + 1) * (min + max) / 2

    /// 最大公約数 - O(わからん)
    let inline gcd x y : 'a =
        let rec gcdR x =
            function
            | y when y = LanguagePrimitives.GenericZero<'a> -> x
            | y -> gcdR y (x % y)

        gcdR (max x y) (min x y)

    /// 最大公約数I - O(わからん)
    let gcdI x y = bigint.GreatestCommonDivisor(x, y)

    /// 最小公倍数 - O(わからん)
    /// オーバーフロー注意: x * y
    let inline lcm x y : 'a =
        let rec gcdR x =
            function
            | y when y = LanguagePrimitives.GenericZero<'a> -> x
            | y -> gcdR y (x % y)

        x * y / gcdR (min x y) (max x y)

    /// 最小公倍数I - O(わからん)
    let lcmI x y =
        x * y / bigint.GreatestCommonDivisor(x, y)
