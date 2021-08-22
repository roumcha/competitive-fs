// 作成 (1)
let st = Set.empty
// 作成 (2)
let st2 = [| "a"; "b"; "c" |] |> set

// 追加 - O(log N)
Set.add "d" st2
// 削除 - O(log N)
Set.remove "d" st2

// 存在確認 - O(log N)
Set.contains "b" st2
