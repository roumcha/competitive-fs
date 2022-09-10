/// ジェネリック数値型 - 生成:O(n)\
/// 参考: https://shuyo.hatenablog.com/entry/20101016/fsharp
module NumericLiteralG =
    let inline FromZero () = LanguagePrimitives.GenericZero
    let inline FromOne () = LanguagePrimitives.GenericOne
    let inline FromInt32 n = Seq.sumBy (fun _ -> FromOne()) { 1 .. n }
