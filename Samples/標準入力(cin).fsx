// ---------- Lib --------------
[<AutoOpen>]
module Cin =
    let private q = System.Collections.Generic.Queue()

    let inline cin _ =
        while q.Count = 0 do
            for s in stdin.ReadLine().Split() do
                if s <> null && s <> "" then q.Enqueue s
        q.Dequeue()
// ---------- End Lib ----------

// 文字列
let s = cin ()

// 整数
let n = cin () |> int

// 文字配列
let s2 = cin().ToCharArray()

// 3個
let s3 = Array.init 3 cin

// 整数3個
let a = Array.init 3 (cin >> int)

// 分けて代入
let x, y = cin (), cin ()

// 10x20 の整数ジャグ配列
let jaggedAry =
    Array.init 10 (fun _ -> Array.init 20 (cin >> int))

// 10x20 の小数二次元配列
let ary2d =
    Seq.init 10 (fun _ -> Seq.init 20 (cin >> float32))
    |> array2D
