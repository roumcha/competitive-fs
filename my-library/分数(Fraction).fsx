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
        match n / div, d / div with
        | n, d when d < 0L -> -n, -d
        | n, d -> n, d
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
    static member (+)(x: Frn, y: Frn) = (x.N * y.D + y.N * x.D, x.D * y.D) |> spl |> Frn
    static member (-)(x: Frn, y: Frn) = (x.N * y.D - y.N * x.D, x.D * y.D) |> spl |> Frn
    static member (*)(x: Frn, y: Frn) = (x.N * y.N, x.D * y.D) |> spl |> Frn
    static member (/)(x: Frn, y: Frn) =
        match y.N with
        | 0L -> System.DivideByZeroException() |> raise
        | _ -> (x.N * y.D, x.D * y.N) |> spl |> Frn
    override _.Equals y =
        match y with
        | :? Frn as y -> n = y.N && d = y.D
        | _ -> System.ArgumentException() |> raise
    override _.GetHashCode() = n * d |> int
    override _.ToString() = sprintf "Frn[%d/%d]" n d
    interface System.IComparable with
        member _.CompareTo y =
            match y with
            | :? Frn as y -> compare (n * y.D) (y.N * d)
            | _ -> System.ArgumentException() |> raise
