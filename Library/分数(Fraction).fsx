/// Fraction(分数)
type Frn(numer: int64, denom: int64) =
    static let rec gcdInner x =
        function
        | y when y = 0L -> x
        | y -> gcdInner y (x % y)
    /// O(log(min(x, y)))
    static let gcd x y = gcdInner (max x y) (min x y)
    /// O(log(min(x, y)))
    static let lcm x y = x * y / gcdInner (min x y) (max x y)
    /// O(log(min(n, d)))
    static let spl (n, d) =
        let div = gcd n d
        match d >= 0L with
        | true -> (n / div, d / div)
        | false -> (n / div * -1L, d / div * -1L)
    let n, d =
        match numer, denom with
        | _, 0L -> System.DivideByZeroException() |> raise
        | 0L, _ -> (0L, 1L)
        | _ -> spl (numer, denom)

    new(numer) = Frn(numer, 1L)
    new(numer: int, denom: int) = Frn(int64 numer, int64 denom)
    new(numer: int) = Frn(int64 numer, 1L)
    member _.N = n
    member _.D = d
    override x.ToString() = sprintf "Frn[%d/%d]" x.N x.D
    override x.GetHashCode() = hash x
    static member (+)(x: Frn, y: Frn) = (x.N * y.D + y.N * x.D, x.D * y.D) |> spl |> Frn
    static member (-)(x: Frn, y: Frn) = (x.N * y.D - y.N * x.D, x.D * y.D) |> spl |> Frn
    static member (*)(x: Frn, y: Frn) = (x.N * y.N, x.D * y.D) |> spl |> Frn
    static member (/)(x: Frn, y: Frn) =
        match y.N with
        | 0L -> System.DivideByZeroException() |> raise
        | _ -> (x.N * y.D, x.D * y.N) |> spl |> Frn
    override x.Equals y =
        match y with
        | :? Frn as y -> (x.N * y.D) = (y.N * x.D)
        | _ -> System.ArgumentException() |> raise
    interface System.IComparable with
        member x.CompareTo y =
            match y with
            | :? Frn as y -> compare (x.N * y.D) (y.N * x.D)
            | _ -> System.ArgumentException() |> raise
