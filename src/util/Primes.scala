package util

sealed class PrimeRange(val primes: Array[Long]) extends RandomAccessSeqProxy[Long] {
  import java.util.Arrays.binarySearch
  import Lists.{ count, group }
  
  val self = primes
  
  // Minimus prime
  val min = primes(0)
  
  // Maximus prime
  val max = primes(primes.size - 1)
  
  def isPrime(m: Long): Boolean = {
    if (m < min)
      return false
    
    if (m > max)
      throw new IndexOutOfBoundsException(m + " is outside of the valid range [" + min + ", " + max + "]")
  
    binarySearch(primes, m) >= 0
  }
  
  def factors(m: Long): List[Long] = {
    var xs = List[Long]()
    var n = m
    var i = 0
    var p = primes(i)
    
    while (n > 1) {
      while (n % p == 0) {
        xs ::= p
        n /= p
      }
      
      i += 1
      p = primes(i)
    }
    
    xs
  }
  
  def factorsR(m: Long): List[Long] = if (isPrime(m)) List(m) else 
    primes find { m % _ == 0 } match {
      case Some(f) => f :: factorsR(m / f)
      case None    => throw new IllegalStateException("No prime number that is a divisor found")
    }
  
  
  def phi(n: Long): BigInt = n match {
    case 1               => BigInt(1)
    case m if isPrime(m) => BigInt(m - 1)
    case m               => count(group(factors(n))) map { 
      _ match {
        case (p, k) => BigInt(p - 1) * BigInt(p).pow(k - 1)
      }
    } reduceLeft { (_: BigInt) * _ }
  }
}

object Primes {
  import java.util.Scanner
  import java.io.{ File, Closeable }
  
  val PRIMES_FILE: String = "primes/primes.txt"
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
}
