package kata.primes

// Pollard's rho algorithm
object Rho {
  def rho(n: BigInt, f: BigInt => BigInt): Option[BigInt] = {
    var x = BigInt(2)
    var y = BigInt(2)
    var d = BigInt(1)

    while (d == 1) {
      x = f(x)
      y = f(f(y))
      d = (x - y).abs gcd n
    }

    if (d == n) None else Some(d)
  }
}
