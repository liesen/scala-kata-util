package kata.primes

class MillerRabin(val certainty: Int) extends Primes {
  def isPrime(m: Long): Boolean = BigInt(m).isProbablePrime(certainty)
}
