/// ここにあるもの:\
/// 商と余り, 切り上げ除算, 範囲の総和, 最大公約数, 最大公約数I, 最小公倍数, 最小公倍数I
module IntFunc =
    /// ジェネリック数値型(Replica) - 生成:O(n)\
    /// 参考: https://shuyo.hatenablog.com/entry/20101016/fsharp
    module private NumericLiteralG =
        let inline FromZero () = LanguagePrimitives.GenericZero
        let inline FromOne () = LanguagePrimitives.GenericOne
        let inline FromInt32 n = Seq.sumBy (fun _ -> FromOne()) { 1 .. n }
    /// 商と余り - O(1)
    let divRem x y : int * int = System.Math.DivRem(x, y)
    /// 商と余り - O(1)
    let divRemL x y : int64 * int64 = System.Math.DivRem(x, y)
    /// 商と余り - O(1)
    let divRemI x y = bigint.DivRem(x, y)
    /// 切り上げ除算 - O(1)
    let inline divCeil divided divisor = (divided + divisor - 1G) / divisor
    /// 範囲の総和 - O(1)
    let inline rangeSum min max = (max - min + 1G) * (min + max) / 2G
    /// 最大公約数 - O(わからん)
    let inline gcd x y =
        let rec gcdR x =
            function
            | y when y = 0G -> x
            | y -> gcdR y (x % y)
        gcdR (max x y) (min x y)
    /// 最大公約数I - O(わからん)
    let gcdI x y = bigint.GreatestCommonDivisor(x, y)
    /// 最小公倍数 - O(わからん)\
    /// オーバーフロー注意: x * y
    let inline lcm x y =
        let rec gcdR x =
            function
            | y when y = 0G -> x
            | y -> gcdR y (x % y)
        x * y / gcdR (min x y) (max x y)
    /// 最小公倍数I - O(わからん)
    let lcmI x y = x * y / bigint.GreatestCommonDivisor(x, y)
