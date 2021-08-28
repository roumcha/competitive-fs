// experimantal
// / %がまだ

/// Dynamic ModInt
type DModInt =
    { V: uint32
      M: uint32 }
    override this.ToString() = string this.V
    static member inline (+)(x: DModInt, y) = { x with V = (x.V + uint32 y) % x.M }
    static member inline (+)(x, y: DModInt) = { y with V = (uint32 x + y.V) % y.M }
    /// バグ防止のため、Mod が異なる場合はエラーにする
    static member (+)(x: DModInt, y: DModInt) =
        if x.M <> y.M then failwith "Different Modulo!"
        x + y.V
    static member inline (-)(x: DModInt, y) = { x with V = (x.V - uint32 y) % x.M }
    static member inline (-)(x, y: DModInt) = { y with V = (uint32 x - y.V) % y.M }
    /// バグ防止のため、Mod が異なる場合はエラーにする
    static member (-)(x: DModInt, y: DModInt) =
        if x.M <> y.M then failwith "Different Modulo!"
        x - y.V
    static member inline (*)(x: DModInt, y) =
        { x with V = uint32 (uint64 x.V * uint64 y % uint64 x.M) }
    static member inline (*)(x, y: DModInt) =
        { y with V = uint32 (uint64 x * uint64 y.V % uint64 y.M) }
    /// バグ防止のため、Mod が異なる場合はエラーにする
    static member (*)(x: DModInt, y: DModInt) =
        if x.M <> y.M then failwith "Different Modulo!"
        x * y.V

/// Dynamic ModInt
module DModInt =
    let inline create modulo value =
        { DModInt.V = uint32 (uint64 value % uint64 modulo)
          M = uint32 modulo }

    let inline mod1e9p7 x = create 1000000007u x
    let inline mod1e9p9 x = create 1000000009u x
    let inline mod99xx53 x = create 998244353u x

    let inline changeMod modulo (x: DModInt) =
        { DModInt.V = x.V % uint32 modulo; M = uint32 modulo }

    // 余りはループするんだからもうちょっと何とかならん？
    let inline pown x n =
        let v = uint64 x.V
        let m = uint64 x.M
        let rec pownR res =
            function
            | 0UL -> res
            | i -> pownR (res * v % m) (i - 1UL)
        { DModInt.V =
              match int64 n >= 0L with
              | true -> pownR 1UL (uint64 n) |> uint32
              | false -> 0u
          M = x.M }

let inline dmodint modulo value = DModInt.create modulo value
