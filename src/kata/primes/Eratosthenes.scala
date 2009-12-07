package kata.primes
import scala.collection._

class Eratosthenes(private val sieve: Array[Int]) extends Primes {
  override def isPrime(m: Long) =
    if (m > Int.MaxValue) error("too large") else (sieve(m.toInt >> 5) & (1 << (m & 31))) != 0

  def elements =
    for { (m, i) <- Iterator.fromArray(sieve).zipWithIndex
          j <- (0 to 31).elements
          if (m & (1 << j)) != 0 }
     yield (i << 5) + j
}

object Eratosthenes {
  def apply(max: Int): Eratosthenes =
    if (max <= 0)
      error("upper limit must be positive")
    else {
      val sieve = Array.make((max >> 5) + (if ((max & 31) != 0) 1 else 0),
                             0xaaaaaaaa) // Strike all even numbers
      sieve(0) &= ~(1 << 1) // Strike 1
      sieve(0) |= 1 << 2    // Unstrike 2

      var p = 3

      while (p < max) {
        (p + p until max by p) foreach { i => sieve(i >> 5) &= ~(1 << (i & 31)) } // Multiples of p
        p += 2; while (p < max && (sieve(p >> 5) & (1 << (p & 31))) == 0) p += 2 // Next p
      }

      new Eratosthenes(sieve)
  }
}
