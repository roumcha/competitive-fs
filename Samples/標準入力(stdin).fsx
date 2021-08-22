// 1行 -> 文字列
let s = stdin.ReadLine()

// 1行 -> 整数
let n = stdin.ReadLine() |> int

// 1行 -> 文字配列
let s2 = stdin.ReadLine().ToCharArray()

// 1行 -> 空白区切り
let s3 = stdin.ReadLine().Split()

// 1行　-> 空白区切り整数
let a =
    stdin.ReadLine().Split() |> Array.map int

// 1行 -> 空白区切り -> 分けて代入 (1)
// Pros: 短く書ける
// Cons: すべて同じ型でのみ有効, パターンマッチ警告が出る
// 警告を消すには、ファイルの頭に #nowarn "25"
#nowarn "25"
let [| x; y |] = stdin.ReadLine().Split()

// 1行 -> 空白区切り -> 分けて代入 (2)
// Pros: 警告出ない, 個別に型変換できる(次項)
// Cons: 長い, 書きづらい
let x2, y2 =
    stdin.ReadLine().Split() |> fun a -> a.[0], a.[1]

// 1行 -> 空白区切り -> 個別の型変換 -> 分けて代入
let x3, y3, z3 =
    stdin.ReadLine().Split()
    |> fun a -> int a.[0], float a.[1], a.[2]

// 10x(不定) の整数ジャグ配列
let jaggedAry =
    Array.init 10 (fun _ -> stdin.ReadLine().Split() |> Array.map int)

// 10x(一定) の整数二次元配列
let ary2d =
    Seq.init 10 (fun _ -> stdin.ReadLine().Split() |> Array.map int)
    |> array2D
