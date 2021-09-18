/// ここにあるもの:\
/// 90度単位回転
module Collections2D =
    /// 90度単位回転 - O(H*W)
    /// x軸を右、y軸を下として、時計回り
    let turn2d90 (source:'a[,]) count =
        let l1, l2 = 
            match count % 2 with
            | 0 -> Array2D.length1 source, Array2D.length2 source
            | _ -> Array2D.length2 source, Array2D.length1 source
        let offx, offy = 
            match count % 4 with
            | 0 -> 0, 0
            | 1 -> 

    /// 90度単位回転 - O(H*W)
    /// x軸を右、y軸を下として、時計回り
    let turnjagged90 (source:'a[][]) count = 0
