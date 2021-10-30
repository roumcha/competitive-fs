/// ここにあるもの:\
/// 文字列形式に変換
module Matrix =
    /// 文字列形式に変換
    let inline toString matrix =
        { 0 .. Array2D.length1 matrix - 1 }
        |> Seq.map (fun i ->
            { 0 .. Array2D.length2 matrix - 1 }
            |> Seq.map (fun j -> matrix.[i, j])
            |> fun sq -> System.String.Join(' ', sq))
        |> fun sq -> System.String.Join(' ', sq)
