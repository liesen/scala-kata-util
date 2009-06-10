package kata

import scala.collection.mutable.ArrayBuffer

/**
 * A range of prime numbers (in order).
 */
sealed class PrimeRange(val primes: Array[Long]) extends RandomAccessSeqProxy[Long] {
  import java.util.Arrays.binarySearch
  import Lists.{ count, group }
  
  val self = primes
  
  val toArray = primes
  
  // Minimus prime
  val MinValue = primes.first
  
  // Maximus prime
  val MaxValue = primes.last
  
  private lazy val sqrtMax = Math.sqrt(MaxValue).toLong
  
  def indexOf(m: Long): Int = binarySearch(primes, m)
  
  def contains(m: Long): Boolean = m >= MinValue && m <= MaxValue && indexOf(m) >= 0
  
  def isPrime(m: Long): Boolean = contains(m) || !( 
    // Try if any prime <= sqrt(m) divides m
    (0 to Math.sqrt(m).toInt) exists { m % primes(_) == 0 })
  
  def factors(m: Long): List[Long] = {
    var xs: List[Long] = Nil
    var n = m
    var i = 0
    var p = primes(i)
    
    while (n > 1 && i < primes.size) {
      while (n % p == 0) {
        xs ::= p
        n /= p
      }
      
      i += 1
      
      if (i < primes.size) {
        p = primes(i)
      }
    }
    
    xs
  }
  
  /**
   * Recursively finds the primes factors of /m/.
   */
  def factorsR(m: Long): List[Long] = if (isPrime(m)) List(m) else 
    primes find { m % _ == 0 } match {
      case Some(f) => f :: factorsR(m / f)
      case None    => throw new IllegalStateException("No prime number that is a divisor found")
    }
  
  /**
   * Returns the number of positive integers (less or equal to /n/) that are coprime to /n/. This
   * is Euler's totient function. 
   * 
   * @see http://en.wikipedia.org/wiki/Euler%27s_totient_function
   */
  def phi(n: Long): BigInt = n match {
    case 1 => BigInt(1)
    case m if isPrime(m) => BigInt(m - 1)
    case m => count(group(factors(n))).foldLeft(BigInt(1)){ (a, p) =>
      a * (p match {
        case (q, k) => BigInt(q - 1) * BigInt(q).pow(k - 1)
      })
    }
  }

  def totient: Long => BigInt = phi
  
  /**
   * The Prime-counting function
   * 
   * Returns the number of primes less than or equal to /n/.
   */
  def pi(n: Long): Int = if (n < MinValue) 0 else indexOf(n) match {
    case m if m < 0 => Math.abs(m) - 1
    case m          => m + 1
  }
}

object Primes {
  import java.util.Scanner
  import java.io.{ File, Closeable }
  
  val PRIMES_FILE: String = "/Users/liesen/workspace/Kata/primes/primes.txt"
  val NUM_PRIMES = 602489
    
  val elements = new Iterator[Long] with Closeable {
    val scanner = new Scanner(new File(PRIMES_FILE))
    
    def hasNext = scanner.hasNextLong
    
    def next = scanner.nextLong
    
    def close = scanner.close
  }
  
  lazy val primes: PrimeRange = take(NUM_PRIMES)
  
  def take(numPrimes: Int): PrimeRange = {
    if (numPrimes > NUM_PRIMES) {
      throw new IllegalArgumentException("I only haz " + NUM_PRIMES + " primez")
    }
    
    val ps = new Array[Long](numPrimes)
    elements.readInto(ps)
    elements.close
    new PrimeRange(ps)
  }
  
  def takeWhile(p: Long => Boolean): PrimeRange = {
    val ps = new ArrayBuffer ++ (elements takeWhile p)
    new PrimeRange(ps.toArray)
  }
}
