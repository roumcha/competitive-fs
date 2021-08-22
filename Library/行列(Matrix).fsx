/// ここにあるもの:\
/// 文字列形式に変換
module Matrix =
    /// 文字列形式に変換
    let toString matrix =
        { 0 .. Array2D.length1 matrix - 1 }
        |> Seq.map
            (fun i ->
                matrix.[i, *]
                |> Seq.map string
                |> String.concat " ")
        |> String.concat "\n"
