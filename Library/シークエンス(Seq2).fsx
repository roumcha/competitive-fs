/// ここにあるもの:\
/// 最初の重複を探す, 連長圧縮, 交互に分ける
module Seq2 =
    /// ソートして最初の重複を探す - O(N)
    /// 0個以上、要素が = < > で比較できるとき
    let tryFirstDup source =
        match source with
        | []
        | [ _ ] -> None
        | src ->
            src
            |> Seq.indexed
            |> Seq.sortBy (fun (ind, item) -> item, ind)
            |> Seq.toArray
            |> fun ary ->
                { 0 .. Array.length ary - 2 }
                |> Seq.tryFindIndex (fun i -> ary.[i] = ary.[i + 1])
                |> function
                    | Some ind -> Some(snd ary.[ind], fst ary.[ind], fst ary.[ind + 1])
                    | None -> None

    /// 最初の重複を探す - O(N^2)
    /// 0個以上、要素が = で比較できるとき
    let tryFirstDupNoSort source =
        let rec lp i buf =
            function
            | [] -> None
            | h :: t ->
                match buf |> List.tryFindIndex ((=) h) with
                | Some x -> Some(h, (List.length buf) - x - 1, i)
                | None -> lp (i + 1) (h :: buf) t
        Seq.toList source |> lp 0 []

    /// 連長圧縮 - O(N)
    let runLengthEncode source =
        let rec lp res src =
            match src, res with
            | [], res -> res
            | sh :: st, (item, cnt) :: rt when sh = item -> lp ((item, cnt + 1) :: rt) st
            | sh :: st, _ -> lp ((sh, 1) :: res) st
        source |> Seq.toList |> lp [] |> Seq.rev

    /// 交互に分ける - O(N)
    let divideAlternately source =
        source
        |> Seq.indexed
        |> Seq.fold
            (fun (l, r) (ind, item) ->
                match ind &&& 1 with
                | 0 -> item :: l, r
                | _ -> l, item :: r)
            ([], [])
        |> fun (f, l) -> Seq.rev f, Seq.rev l
