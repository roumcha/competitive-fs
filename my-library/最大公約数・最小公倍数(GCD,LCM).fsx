/// ここにあるもの:\
/// 最大公約数, 最大公約数I, 最小公倍数, 最小公倍数I
module GCDLCM =
    /// 最大公約数 - O(log(max(x, y)))
    let inline gcd x y =
        let mutable x, y = x, y
        while y <> 0 do
            let r = x % y
            x <- y
            y <- r
        x
    /// 最大公約数I - O(わからん)
    let inline gcdI x y = bigint.GreatestCommonDivisor(x, y)
    /// 最小公倍数 - O(log(max(x, y)))\
    /// オーバーフロー注意: x * y
    let inline lcm x y = x / gcd x y * y
    /// 最小公倍数I - O(わからん)
    let inline lcmI x y = x * y / bigint.GreatestCommonDivisor(x, y)
