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
