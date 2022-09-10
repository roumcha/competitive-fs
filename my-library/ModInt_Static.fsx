// todo: /, %, pow, ...
// todo: マイナス時補正してない
/// Static ModInt (32bit)\
/// !!DON'T USE THE CONSTRUCTOR!! Use ```smodint n``` or suffix N instead.
[<Struct>]
type SModInt(v: uint32) =
    [<Literal>]
    static let m = 1000000007u // modify this
    member _.V = v
    override _.ToString() = string v
    static member inline Zero = SModInt 0u
    static member inline One = SModInt 1u
    static member inline (+)(x: SModInt, y: SModInt) = SModInt((x.V + y.V) % m)
    static member inline (-)(x: SModInt, y: SModInt) = SModInt((x.V + m - y.V) % m)
    static member inline (*)(x: SModInt, y: SModInt) =
        SModInt(uint32 (uint64 x.V * uint64 y.V % uint64 m))

module NumericLiteralN =
    let inline FromZero () = SModInt.Zero
    let inline FromOne () = SModInt.One
    let inline FromInt32 (n: int) = SModInt(uint32 n)
    let inline FromInt64 (n: int64) = SModInt(uint32 n)

let inline smodint n = n |> uint32 |> SModInt
