// experimental
// /, % がまだ
/// Static ModInt
type SModInt =
    { V: uint32 }
    override this.ToString() = string this.V

/// Static ModInt
module SModInt =
    [<Literal>]
    let M = 1000000007u // ここを変える

    let FromZero () = { SModInt.V = 0u }
    let FromOne () = { SModInt.V = 1u }
    let FromUInt32 (n: uint32) = { SModInt.V = n % M }
    let FromInt32 (n: int) = FromUInt32(uint32 n)
    let FromUInt64 (n: uint64) = { SModInt.V = uint32 (n % uint64 M) }
    let FromInt64 (n: int64) = FromUInt64(uint64 n)
    let value (smodint: SModInt) = smodint.V

type SModInt with
    static member inline (+)(x: SModInt, y: SModInt) = SModInt.FromUInt32(x.V + y.V)
    static member inline (+)(x: SModInt, y) = SModInt.FromUInt32(x.V + uint32 y)
    static member inline (+)(x, y: SModInt) = SModInt.FromUInt32(uint32 x + y.V)
    static member inline (-)(x: SModInt, y: SModInt) = SModInt.FromUInt32(x.V - y.V)
    static member inline (-)(x: SModInt, y) = SModInt.FromUInt32(x.V - uint32 y)
    static member inline (-)(x, y: SModInt) = SModInt.FromUInt32(uint32 x - y.V)

    static member inline (*)(x: SModInt, y) =
        SModInt.FromUInt64(uint64 x.V * uint64 y)

    static member inline (*)(x, y: SModInt) =
        SModInt.FromUInt64(uint64 x * uint64 y.V)

    static member (*)(x: SModInt, y: SModInt) =
        SModInt.FromUInt64(uint64 x.V * uint64 y.V)

/// 1N, 2N, ... と書ける
module NumericLiteralN = SModInt // G(I)NQRZ しか suffix 空いてないみたい...
let inline smodint n = SModInt.FromUInt64(uint64 n)
