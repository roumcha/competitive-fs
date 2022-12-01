// 行 -> string
let s = stdin.ReadLine()

// 行 -> int
let n = stdin.ReadLine() |> int

// 行 -> char[]
let s2 = stdin.ReadLine().ToCharArray()

// 行 -> 空白区切り -> string[]
let s3 = stdin.ReadLine().Split()

// 行　-> 空白区切り -> int[]
let a = stdin.ReadLine().Split() |> Array.map int

// 行 -> 空白区切り -> 分けて代入
// メリット: 短く書ける
// デメリット: すべて同じ型限定、パターンマッチ警告が出る
// 警告を消すには、ファイルの頭に #nowarn "25"
#nowarn "25"
let [| x; y |] = stdin.ReadLine().Split()

// 行 -> 空白区切り -> 分けて代入 (別解)
// メリット: 警告出ない、個別に型変換できる(次項)
// デメリット: 長くて書きづらい
let x2, y2 =
    stdin.ReadLine().Split() |> fun a -> a.[0], a.[1]

// 行 -> 空白区切り -> バラバラの型 -> 分けて代入
let x3, y3, z3 =
    stdin.ReadLine().Split()
    |> fun a -> int a.[0], float a.[1], a.[2]

// 10x(不定) の整数ジャグ配列
let jaggedAry =
    Array.init 10 (fun _ -> stdin.ReadLine().Split() |> Array.map int)

// 10x(固定) の整数二次元配列
let ary2d =
    Seq.init 10 (fun _ -> stdin.ReadLine().Split() |> Array.map int)
    |> array2D
