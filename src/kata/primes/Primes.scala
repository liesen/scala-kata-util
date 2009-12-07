package kata.primes

import scala.collection.mutable.ArrayBuffer

trait Primes {
  def isPrime(m: Long): Boolean
}


object Primes {
  import java.util.Scanner
  import java.io.{ File, Closeable }
  
  val PRIMES_FILE: String = "/Users/liesen/workspace/Kata/primes/primes.txt"
  val NUM_PRIMES = 602489

  def fromFile(path: String) = new Iterator[Long] with Closeable {
    val scanner = new Scanner(new File(path))
    
    def hasNext = scanner.hasNextLong
    
    def next = scanner.nextLong
    
    def close = scanner.close
  }

  lazy val elements = fromFile(PRIMES_FILE)
  
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
