package util

/**
 * A range of prime numbers (in order).
 */
sealed class PrimeRange(val primes: Array[Long]) extends RandomAccessSeqProxy[Long] {
  import java.util.Arrays.binarySearch
  import Lists.{ count, group }
  
  val self = primes
  
  // Minimus prime
  val min = primes.first
  
  // Maximus prime
  val max = primes.last
  
  private val sqrtMax = Math.sqrt(max).toLong 
  
  def indexOf(m: Long): Int = binarySearch(primes, m)
  
  def isPrime(m: Long): Boolean = {
    if (m < min)
      false
    else if (m <= max)
      indexOf(primes, m) >= 0
    else { // Try if any prime <= sqrt(m) divides m
      val s = Math.sqrt(m).toLong
      var i = 0
      
      while (primes(i) <= s) {
        if (m % primes(i) == 0)
          return false
        
        i += 1
      }
      
      true
    }
  }
  
  def factors(m: Long): List[Long] = {
    var xs = List[Long]()
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
    case 1               => BigInt(1)
    case m if isPrime(m) => BigInt(m - 1)
    case m               => count(group(factors(n))) map { 
      _ match {
        case (p, k) => BigInt(p - 1) * BigInt(p).pow(k - 1)
      }
    } reduceLeft { (_: BigInt) * _ }
  }
  
  /**
   * The Prime-counting function
   * 
   * Returns the number of primes less than or equal to /n/.
   */
  def pi(n: Long): Int = if (n < min) 0 else binarySearch(primes, n) match {
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
    if (numPrimes > NUM_PRIMES)
      throw new IllegalArgumentException("I only haz " + NUM_PRIMES + " primez")
    
    val ps = new Array[Long](numPrimes)
    elements.readInto(ps)
    elements.close
    new PrimeRange(ps)
  }
  
  def takeWhile(p: Long => Boolean): PrimeRange = {
    val ps = new scala.collection.mutable.ArrayBuffer ++ (elements takeWhile p)
    new PrimeRange(ps.toArray)
  }
}
