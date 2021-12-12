/// ここにあるもの:\
/// 二分探索, 二分探索_挿入左
module BinSearch =
    /// 二分探索 - O(log N)\
    /// めぐる式二分探索
    let inline search ok ng condition =
        let g1 = LanguagePrimitives.GenericOne
        let g2 = g1 + g1
        let rec lp o n f =
            match abs (n - o), (o + n) / g2 with
            | c, _ when c <= g1 -> o
            | _, m when f m -> lp m n f
            | _, m -> lp o m f
        lp ok ng condition
    /// 二分探索_挿入左 - O(log N)\
    /// ソートされた配列に対する、新要素の挿入位置(のうち最も左)を探索
    let inline indexLeft item ary =
        match System.Array.BinarySearch(ary, item) with
        | i when i < 0 -> ~~~i
        | i -> i
