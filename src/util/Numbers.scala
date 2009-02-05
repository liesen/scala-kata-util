package util


/**
 * Useful stuff for dealing with numbers (mostly integers).
 */
object Numbers {
  import java.lang.Character
  
  def divMod(n: Long, d: Long): (Long, Long) = (n / d, n % d)
  
  /**
   * Returns a list containing the digits of {@code n}.
   * @param n a number
   */
  def digits(n: Long): List[Int] = if (n < 10) List(n.toInt) else {
    val _ @ (m, r) = divMod(n, 10)
    digits(m) ::: List(r.toInt)
  }
  
  def digitsS(n: Number): List[Int] = n.toString.toList map { Character.digit(_, 10) }
  
  def undigits(digits: Seq[Int]): BigInt = digits.size match {
    case 0 => BigInt(0)
    case n => BigInt(10).pow(n - 1) * BigInt(digits.first) + undigits(digits drop 1)
  }
  
  /** Returns the number of digits in a base 10 number, {@code n}. */
  def numDigits(n: Long): Int = {
    if (n < 10L) 1 else
    if (n < 100L) 2 else
    if (n < 1000L) 3 else
    if (n < 10000L) 4 else
    if (n < 100000L) 5 else
    if (n < 1000000L) 6 else
    if (n < 10000000L) 7 else
    if (n < 100000000L) 8 else
    if (n < 1000000000L) 9 else
    if (n < 10000000000L) 10 else
    if (n < 100000000000L) 11 else
    if (n < 1000000000000L) 12 else
    if (n < 10000000000000L) 13 else
    if (n < 100000000000000L) 14 else
    if (n < 1000000000000000L) 15 else
    if (n < 10000000000000000L) 16 else
    if (n < 100000000000000000L) 17 else
    if (n < 1000000000000000000L) 18 else 19
  }
  
  def numDigits(m: BigInt): Int = if (m < Long.MaxValue) numDigits(m.intValue) else {
    1 + numDigits(m / 10)
  }
  
  def stream(seed: Int, f: Int => Int): Stream[Int] = Stream.cons(seed, stream(f(seed), f))
  
  /** /n/:th traingle number */
  def triangle(n: Int): Int = n * (n + 1) / 2
  
  def triangles(): Stream[Int] = Stream.from(0) map triangle
  
  def pentagonals(): Stream[Int] = Stream.from(0) map { n => n * (3 * n - 1) / 2 }
  
  def hexagonals(): Stream[Int] = Stream.from(0) map { n => n * (2 * n - 1) }
  
  def properDivisors(num: Int): Iterator[Int] = Iterator.range(1, 1 + num / 2) filter { num % _ == 0 }
  
  def divisors(num: Int): Iterator[Int] = properDivisors(num) ++ Iterator.single(num)
  
  def isDeficient(num: Int): Boolean = (properDivisors(num) reduceLeft { _ + _ }) < num
    
  def isAbundant(num: Int): Boolean = (properDivisors(num) reduceLeft { _ + _ }) > num
  
  def isPerfect(num: Int): Boolean = (properDivisors(num) reduceLeft { _ + _ }) == num
  
  /** Checks if the numbers 0, 1, ..., 9 are in /num/ */
  def isPandigital(num: Int): Boolean = digits(num).foldLeft(0){ _ | 1 << _ } == (1 << 10) - 2
  
  /** Iterative Euclidean algorithm for computing the greatest common divisor between two numbers */
  def gcd(a: Long, b: Long): Long = {
    var m = Math.abs(a)
    var n = Math.abs(b)
    
    while (n != 0) {
      val t = n
      n = m % n
      m = t
    }
    
    return m
  }
  
  def factors(num: Int): Iterable[Int] = {
    import scala.collection.mutable.{ Buffer, ListBuffer }
    
    val xs: Buffer[Int] = new ListBuffer
    var n = num
    var s = Math.sqrt(num)
    var i = 2
    
    while (i <= s) {
      while (n % i == 0) {
        xs += i
        n /= i
        s = Math.sqrt(n)
      }
      
      i += 1
    }
    
    if (n > 1) {
      xs += n
    }
    
    xs
  }
  
  /**
   * Divisibility tests for numbers up to 10 excluding 7.
   */
  def divisibleBy(n: Int): PartialFunction[Long, Boolean] = (m: Long) => n match {
    case 1  => true
    case 2  => (m & 1) == 0 
    case 3  => (digits(m) reduceLeft { _ + _ }) % 3 == 0
    case 4  => (m % 100) % 4 == 0
    case 5  => val last = m % 10; m == 0 || m == 5
    case 6  => divisibleBy(2)(m) && divisibleBy(3)(m)
    case 8  => (digits(m % 1000) reduceLeft { _ + _ }) % 8 == 0
    case 9  => (digits(m) reduceLeft { _ + _ }) % 9 == 0
    case 10 => (m % 10) == 0
  }
  
  /**
   * Writes out a number in words: one, two, three, ... 
   */
  def toWords(n: Int): String = {
    if (n >= 1000) {
      (n / 1000, n % 1000) match {
        case (k, 0) => toWords(k) + " thousand"
        case (k, m) => toWords(k) + " thousand " + toWords(m)
      }
    } else if (n >= 100) {
      (n / 100, n % 100) match {
        case (h, 0) => toWords(h) + " hundred"
        case (h, m) => toWords(h) + " hundred and " + toWords(m)
      }
    } else if (n >= 20) {
      val tens = Array("twen", "thir", "for", "fif", "six", "seven", "eigh", "nine")
      
      (n / 10, n % 10) match {
        case (d, 0) => tens(d - 2) + "ty"
        case (d, m) => tens(d - 2) + "ty-" + toWords(m)
      }
    } else if (n >= 13) {
      Array("thir", "four", "fif", "six", "seven", "eigh", "nine")(n - 13) + "teen"
    } else n match {
      case 12 => "twelve"
      case 11 => "eleven"
      case 10 => "ten"
      case _  => Array("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")(n)
    }
  }
}
