// 作成 (1)
let mp = Map.empty
// 作成 (2)
let mp2 = [| "a", 1; "b", 2; "c", 3 |] |> Map

// 追加 - O(log N)
Map.add "d" 4 mp2
// 削除 - O(log N)
Map.remove "d" mp2
// 読み出し - O(log N)
Map.find "b" mp2
// あれば読み出し - O(log N)
Map.tryFind "b" mp2
