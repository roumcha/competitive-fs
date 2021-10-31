/// ここにあるもの:\
/// 文字列形式に変換, 90度単位回転
module Matrix =
    open System.Text
    /// 文字列形式に変換
    let inline toString matrix =
        { 0 .. Array2D.length1 matrix - 1 }
        |> Seq.fold
            (fun (sb: StringBuilder) i ->
                { 0 .. Array2D.length2 matrix - 1 }
                |> Seq.map (fun j -> matrix.[i, j])
                |> fun sq -> sb.AppendJoin(' ', sq).Append('\n'))
            (StringBuilder())

    /// 90度単位回転 - O(H*W)
    /// x軸を右、y軸を下として、時計回り
    let turn90d (source: 'a [,]) count =
        let l1, l2 =
            match count % 2 with
            | 0 -> Array2D.length1 source, Array2D.length2 source
            | _ -> Array2D.length2 source, Array2D.length1 source
        System.NotImplementedException() |> raise
// let offx, offy =
//     match count % 4 with
//     | 0 -> 0, 0
//     | 1 ->
