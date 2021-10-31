// 参考: https://github.com/key-moon/ac-library-cs/blob/master/AtCoderLibrary/DSU.cs

/// Disjoint Set Union (Union Find)
type DSU = { Count: int; ParentOrSize: int [] }
/// Disjoint Set Union (Union Find)
module DSU =
    let create count =
        { Count = count; ParentOrSize = Array.create count -1 }

    let rec leader a (dsu: DSU) =
        match dsu.ParentOrSize.[a] < 0 with
        | true -> a
        | false ->

        dsu.ParentOrSize.[a] <- leader dsu.ParentOrSize.[a] dsu
        dsu.ParentOrSize.[a]

    let merge (a, b) (dsu: DSU) =
        assert (0 <= a && a < dsu.Count || 0 <= b && b < dsu.Count)
        match leader a dsu, leader b dsu with
        | x, y when x = y -> x
        | x, y ->

        let x, y =
            match -dsu.ParentOrSize.[x] < -dsu.ParentOrSize.[y] with
            | true -> y, x
            | false -> x, y
        dsu.ParentOrSize.[x] <- dsu.ParentOrSize.[x] + dsu.ParentOrSize.[y]
        dsu.ParentOrSize.[y] <- x
        x

    let same (a, b) (dsu: DSU) =
        assert (0 <= a && a < dsu.Count || 0 <= b && b < dsu.Count)
        leader a dsu = leader b dsu

    let size a (dsu: DSU) =
        assert (0 <= a && a < dsu.Count)
        -dsu.ParentOrSize.[leader a dsu]

    let groups (dsu: DSU) =
        let leaderBuf = Array.init dsu.Count (fun i -> leader i dsu)
        let id = Array.zeroCreate dsu.Count
        let result = Array.zeroCreate<int []> dsu.Count
        let groupCount =
            leaderBuf
            |> Seq.indexed
            |> Seq.filter (fun (i, l) -> i = l)
            |> Seq.fold
                (fun groupCount (i, _) ->
                    Array.set id i groupCount
                    Array.set result id.[i] (Array.zeroCreate -dsu.ParentOrSize.[i])
                    groupCount + 1)
                0
        let ind = Array.zeroCreate groupCount
        let slicedResult = Array.take groupCount result
        leaderBuf
        |> Seq.iteri (fun i ldr ->
            let leaderID = id |> Array.item ldr
            Array.set slicedResult.[leaderID] ind.[leaderID] i
            Array.item leaderID ind + 1
            |> Array.set ind leaderID)
        slicedResult
