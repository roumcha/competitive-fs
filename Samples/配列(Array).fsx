// 長さ10、デフォルト値で初期化した配列を型推論して作成
let ary = Array.zeroCreate 10
// 長さ10、デフォルト値で初期化した配列を bool[] 型で作成
let bAry = Array.zeroCreate<bool> 10

// 0番目を999に書き換え (1)
ary.[0] <- 999
// 0番目を999に書き換え (2)
Array.set ary 0 999

// 以下の処理はすべて非破壊
// 一括適用
ary |> Array.map (fun n -> n + 7)
// 戻り値無しの一括処理
ary |> Array.iter (stdout.WriteLine)
// あらゆるペア
ary |> Array.allPairs
// 隣り合うペア
ary |> Array.pairwise
// 隣り合う3つ
ary |> Array.windowed 3
// 重複除去
ary |> Array.distinct
