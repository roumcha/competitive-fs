// 1行
stdout.WriteLine 42
// 1行 (遅い)
printfn "number: %A" 42

// 末尾改行なし
stdout.Write 42

// 末尾改行なし（遅い）
printf "number: %A" 42

// スペース区切り
[ "Hello"; "F#"; "World!" ]
|> String.concat " "
|> stdout.WriteLine

// スペース区切り(2)
let sb1 = System.Text.StringBuilder()

[ 123; 456; 789 ]
|> fun lst -> sb1.AppendJoin(" ", lst)
|> stdout.WriteLine

// 改行区切り(1)
[ "Hello"; "F#"; "World!" ]
|> String.concat "\n"
|> stdout.WriteLine

// 改行区切り(2)
let sb2 = System.Text.StringBuilder()

[ 123; 456; 789 ]
|> fun lst -> sb2.AppendJoin("\n", lst)
|> stdout.WriteLine

// 改行区切り(3)（遅い）
[ "Hello"; "F#"; "World!" ]
|> Seq.iter stdout.WriteLine

// 文字配列 -> 文字列
[| 'H'; 'e'; 'l'; 'l'; 'o' |]
|> System.String
|> stdout.WriteLine

// 文字Seq -> 文字列
[ 'H'; 'e'; 'l'; 'l'; 'o' ]
|> System.String.Concat
|> stdout.WriteLine
