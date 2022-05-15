/// ここにあるもの:\
/// 商と余り, 切り上げ除算, 範囲の総和, 最大公約数, 最大公約数I, 最小公倍数, 最小公倍数I
module IntFunc =
    /// ジェネリック数値型(Replica) - 生成:O(n)
    module private NumericLiteralG =
        let inline FromZero () = LanguagePrimitives.GenericZero
        let inline FromOne () = LanguagePrimitives.GenericOne
        let inline FromInt32 n = Seq.sumBy (fun _ -> FromOne()) { 1 .. n }
    /// 商と余り - O(1)
    let inline divRem x y : int * int = System.Math.DivRem(x, y)
    /// 商と余り - O(1)
    let inline divRemL x y : int64 * int64 = System.Math.DivRem(x, y)
    /// 商と余り - O(1)
    let inline divRemI x y = bigint.DivRem(x, y)
    /// 切り上げ除算 - O(1)
    let inline divCeil divided divisor = (divided + divisor - 1G) / divisor
    /// 範囲の総和 - O(1)
    let inline rangeSum min max = (max - min + 1G) * (min + max) / 2G
    /// gcd, lcm の内部ループ
    let inline private gcdL x y =
        let mutable x, y = x, y
        while y <> 0G do
            let tmp = x
            x <- y
            y <- tmp % y
        x
    /// 最大公約数 - O(log(max(x, y)))
    let inline gcd x y = gcdL (max x y) (min x y)
    /// 最大公約数I - O(わからん)
    let inline gcdI x y = bigint.GreatestCommonDivisor(x, y)
    /// 最小公倍数 - O(log(max(x, y)))\
    /// オーバーフロー注意: x * y
    let inline lcm x y = x * y / gcdL (min x y) (max x y)
    /// 最小公倍数I - O(わからん)
    let inline lcmI x y = x * y / bigint.GreatestCommonDivisor(x, y)
